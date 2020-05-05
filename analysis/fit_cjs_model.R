#-----------------------------------------------
#
# A script for running CJS models
#
# Created by: Mike Ackerman & Kevin See
# Date created: 4/22/20
# Last modified:
#
#-----------------------------------------------

#-------------------------
# load necessary libraries
#-------------------------
library(telemetyr)
library(tidyverse)
library(readxl)
library(lubridate)
library(magrittr)
library(janitor)
library(rjags)
library(postpack)

theme_set(theme_bw())

#-------------------------
# read in RT site metadata
#-------------------------
rec_meta = read_excel('data/prepped/site_metadata/rt_site_metadata.xlsx')

# which RT receivers were used each year?
rec_df = rec_meta %>%
  filter(site_type == 'rt_fixed') %>%
  filter(as.integer(str_sub(rt_rkm, end = 3)) <= 274) %>%
  mutate_at(vars(site_code),
            list(~ factor(., levels = unique(.)))) %>%
  gather(season, use, starts_with("use")) %>%
  mutate(season = str_remove(season, "use")) %>%
  filter(use) %>%
  select(-use) %>%
  select(season, everything())

#-------------------------
# load capture histories for all years
#-------------------------
# load all years of data
rt_ch = list('pilot',
     '2018_2019',
     '2019_2020') %>%
  rlang::set_names() %>%
  map_df(.id = 'Year',
         .f = function(x) {
           load(paste0('data/prepped/', x, '/cap_hist.rda'))
           ch = cap_hist_list$tag_df %>%
             left_join(cap_hist_list$ch_wide) %>%
             mutate(some_det = if_else(is.na(cap_hist), F, T)) %>%
             filter(duty_cycle == 'batch_1' | some_det) %>%
             select(-some_det)

           return(ch)
         }) %>%
  select(-cap_hist) %>%
  # filter out releases from PAHTRP and HYDTRP
  filter(release_site %in% c("LEMTRP", "LLRTP")) %>%
  # filter out on-off duty cycles
  filter(duty_cycle != "on_off") %>%
  mutate(LLRTP = if_else(release_site == 'LLRTP',
                         1, 0),
         LEMTRP = if_else(release_site == 'LEMTRP',
                         1, 0)) %>%
  # put sites in correct order for CJS model
  gather(site, seen, -(Year:tag_id)) %>%
  mutate(site = factor(site,
                       levels = c("LEMTRP", levels(rec_df$site_code), "LLRTP")),
         site = fct_relevel(site, "LLRTP", after = 8)) %>%
  filter(!is.na(site)) %>%
  spread(site, seen,
         fill = 0)


# # which year of data to load?
# load('data/prepped/pilot/cap_hist.rda')
# load('data/prepped/2018_2019/cap_hist.rda')
# load('data/prepped/2019_2020/cap_hist.rda')

# some preliminary examination of data
tabyl(rt_ch, duty_cycle, release_site, season) %>%
  adorn_totals(where = c("row", "col"))

#-----------------------------------
# specify model in JAGS
jags_model = function() {
  # PRIORS
  phi[1] <- 1
  p[1] <- 1
  for(j in 2:J) {
    phi[j] ~ dbeta(1,1) # survival probability between arrays
    p[j] ~ dbeta(1,1)   # detection probability at each array
  }

  # LIKELIHOOD - Here, p and phi are global
  for (i in 1:N) {
    # first known occasion must be z == 1
    # z[i, f[i]] <- 1
    # j = 1 is the release occasion - known alive; i.e., the mark event
    for (j in (f[i] + 1):J) {
      # survival process: must have been alive in j-1 to have non-zero pr(alive at j)
      z[i,j] ~ dbern(phi[j] * z[i,j-1]) # fish i in period j is a bernoulli trial

      # detection process: must have been alive in j to observe in j
      y[i,j] ~ dbern(p[j] * z[i,j]) # another bernoulli trial
    }
  }

  # DERIVED QUANTITIES
  # survivorship is probability of surviving from release to a detection occasion
  survship[1] <- 1 # the mark event; everybody survived to this point
  for (j in 2:J) { # the rest of the events
    survship[j] <- survship[j-1] * phi[j]
  }
}

# write model to a text file
jags_file = "analysis/CJS_models/CJS_model.txt"
write_model(jags_model, jags_file)

# specify which parameters to track
jags_params = c("phi", "p", "survship")
# if interested in estimates of final location
# jags_params = c(jags_params, "z")


#-----------------------------------
# put together data for JAGS
y_list = rt_ch %>%
  split(list(.$season)) %>%
  map(.f = function(x) {
    if(x$season %in% c('17_18', '18_19')) {
      sites = rec_df %>%
        filter(season == unique(x$season)) %>%
        pull(site_code) %>%
        as.character()
      y = x %>%
        select(one_of(c('LLRTP', sites))) %>%
        select(one_of(names(x))) %>%
        as.matrix()
    }

    if(x$season == '19_20') {
      sites = rec_df %>%
        filter(season == unique(x$season)) %>%
        pull(site_code) %>%
        as.character()
      y = x %>%
        # filter out fish released from lower Lemhi trap but detected upstream
        filter(!(release_site == 'LLRTP' &
                 (BC == 1 | TC == 1 | EC == 1 | SS == 1 | EU == 1 | LF == 1))) %>%
        select(one_of(c("LEMTRP", 'LLRTP', sites))) %>%
        select(one_of(names(x))) %>%
        as.matrix()
    }
    return(y)
  })

# fit models for each year separately
for(i in 1:3) {
  cat(paste("Working on", names(y_list)[i], "\n"))

  y = y_list[[i]]

  jags_data = list(
    N = nrow(y),
    J = ncol(y),
    y = y,
    z = known_alive(y),
    f = first_alive(y)
  )


  # using rjags package
  jags = jags.model(jags_file,
                    data = jags_data,
                    n.chains = 4,
                    n.adapt = 1000)
  # burnin
  update(jags, n.iter = 2500)
  # posterior sampling
  post = coda.samples(jags,
                      jags_params,
                      n.iter = 2500,
                      thin = 5)

  # posterior summaries
  param_summ = post_summ(post,
                         jags_params,
                         Rhat = T,
                         ess = T) %>%
    t() %>%
    as_tibble(rownames = "param") %>%
    mutate(cv = abs(sd / mean))

  # save model results
  file_nm = paste0('RT_only_CJS_', names(y_list)[i], '.rda')

  save(jags_data, post, param_summ,
       file = paste0("analysis/CJS_models/", file_nm))

  rm(y, jags_data, jags, post, param_summ, file_nm)
}


#-----------------------------------
# pull out parameter summaries
#-----------------------------------
param_summ_all = c("17_18",
                   "18_19",
                   "19_20") %>%
  as.list() %>%
  rlang::set_names() %>%
  map_df(.id = 'season',
         .f = function(x) {
           load(paste0("analysis/CJS_models/RT_only_CJS_", x, '.rda'))
           param_summ %>%
             mutate(site_num = str_extract(param, "[:digit:]+"),
                    site_num = as.integer(site_num)) %>%
             left_join(tibble(site = colnames(jags_data$y)) %>%
                         mutate(site = factor(site, levels = site),
                                site_num = as.integer(site)) %>%
                         arrange(site_num))
         }) %>%
  mutate(site = factor(site,
                       levels = c("LEMTRP", levels(rec_df$site_code), "LLRTP")),
         site = fct_relevel(site, "LLRTP", after = 8)) %>%
  group_by(season) %>%
  filter(site_num != max(site_num)) %>%
  ungroup()

#-----------------------------------
# Make some plots
#-----------------------------------
dodge_width = 0.3

det_p = param_summ_all %>%
  filter(grepl('^p\\[', param)) %>%
  ggplot(aes(x = site,
             y = mean,
             color = season)) +
  scale_color_brewer(palette = "Set1",
                     name = "Season") +
  geom_errorbar(aes(ymin = `2.5%`,
                    ymax = `97.5%`),
                width = 0,
                position = position_dodge(width = dodge_width)) +
  geom_point(position = position_dodge(width = dodge_width)) +
  labs(x = 'Site',
       y = 'Detection Probability')

phi_p = param_summ_all %>%
  filter(grepl('phi', param)) %>%
  ggplot(aes(x = site,
             y = mean,
             color = season)) +
  scale_color_brewer(palette = "Set1",
                     name = "Season") +
  geom_errorbar(aes(ymin = `2.5%`,
                    ymax = `97.5%`),
                width = 0,
                position = position_dodge(width = dodge_width)) +
  geom_point(position = position_dodge(width = dodge_width)) +
  labs(x = 'Site',
       y = 'Survival From Previous Site')

surv_p = param_summ_all %>%
  filter(grepl('surv', param)) %>%
  ggplot(aes(x = site,
             y = mean,
             color = season)) +
  scale_color_brewer(palette = "Set1",
                     name = "Season") +
  geom_errorbar(aes(ymin = `2.5%`,
                    ymax = `97.5%`),
                width = 0,
                position = position_dodge(width = dodge_width)) +
  geom_point(position = position_dodge(width = dodge_width)) +
  labs(x = 'Site',
       y = 'Cummulative Survival')

#----------------------------------------------------------
# reset cummulative survival for 2019-20 to start at LLRTP
load("analysis/CJS_models/RT_only_CJS_19_20.rda")
surv_summ = post %>%
  as.matrix(chain = T,
            iter = T) %>%
  as_tibble() %>%
  select(-(`p[1]`:`phi[8]`),
         -(`survship[1]`:`survship[8]`)) %>%
  mutate(`survship[9]` = 1) %>%
  mutate(`survship[10]` = `survship[9]` * `phi[10]`,
         `survship[11]` = `survship[10]` * `phi[11]`,
         `survship[12]` = `survship[11]` * `phi[12]`,
         `survship[13]` = `survship[12]` * `phi[13]`,
         `survship[14]` = `survship[13]` * `phi[14]`,
         `survship[15]` = `survship[14]` * `phi[15]`,
         `survship[16]` = `survship[15]` * `phi[16]`,
         `survship[17]` = `survship[16]` * `phi[17]`) %>%
  select(ssurv_summ = post %>%
           as.matrix(chain = T,
                     iter = T) %>%
           as_tibble() %>%
           select(-(`p[1]`:`phi[7]`),
                  -(`survship[1]`:`survship[7]`)) %>%
           mutate(`survship[8]` = 1) %>%
           mutate(`survship[9]` = `survship[8]` * `phi[9]`,
                  `survship[10]` = `survship[9]` * `phi[10]`,
                  `survship[11]` = `survship[10]` * `phi[11]`,
                  `survship[12]` = `survship[11]` * `phi[12]`,
                  `survship[13]` = `survship[12]` * `phi[13]`,
                  `survship[14]` = `survship[13]` * `phi[14]`,
                  `survship[15]` = `survship[14]` * `phi[15]`,
                  `survship[16]` = `survship[15]` * `phi[16]`,
                  `survship[17]` = `survship[16]` * `phi[17]`) %>%
           select(starts_with('surv')) %>%
           gather(param, value) %>%
           group_by(param) %>%
           summarise(mean = mean(value),
                     sd = sd(value),
                     `50%` = quantile(value, 0.5),
                     `2.5%` = quantile(value, 0.025),
                     `97.5%` = quantile(value, 0.975),
                     cv = sd / mean) %>%
           ungroup() %>%
           mutate(season = '19_20') %>%
           select(season, everything())

surv_p2 = param_summ_all %>%
  anti_join(surv_summ %>%
              select(season, param)) %>%
  bind_rows(surv_summ %>%
              left_join(param_summ_all %>%
                          select(-(mean:`97.5%`),
                                 -cv))) %>%
  filter(grepl('surv', param)) %>%
  filter(!(season == '19_20' & site_num < 8)) %>%
  ggplot(aes(x = site,
             y = mean,
             color = season)) +
  scale_color_brewer(palette = "Set1",
                     name = "Season") +
  geom_errorbar(aes(ymin = `2.5%`,
                    ymax = `97.5%`),
                width = 0,
                position = position_dodge(width = dodge_width)) +
  geom_point(position = position_dodge(width = dodge_width)) +
  labs(x = 'Site',
       y = 'Cummulative Survival')

det_p
phi_p
surv_p
surv_p2
