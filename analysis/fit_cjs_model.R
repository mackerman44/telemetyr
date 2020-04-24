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


#-------------------------
# load capture history
#-------------------------
load('data/prepped/pilot/cap_hist.rda')
load('data/prepped/2018_2019/cap_hist.rda')
load('data/prepped/2019_2020/cap_hist.rda')

tabyl(cap_hist_list$tag_df,
      release_site,
      duty_cycle) %>%
  adorn_totals(where = c("row", "col"))

cap_hist_list$tag_df



# how many detections at each site by duty cycle
cap_hist_list$tag_df %>%
  select(tag_id, duty_cycle) %>%
  left_join(cap_hist_list$ch_wide) %>%
  group_by(duty_cycle) %>%
  summarise_at(vars(-tag_id, -cap_hist),
               list(sum),
               na.rm = T)

cap_hist = cap_hist_list$ch_wide

#--------------------------------------
# Capture history matrix
# 2017-2018
y = cap_hist_list$tag_df %>%
  filter(release_site == 'LLRTP',
         duty_cycle != "on_off") %>%
  select(tag_id, release_site, duty_cycle) %>%
  left_join(cap_hist) %>%
  mutate(some_det = if_else(is.na(cap_hist), F, T)) %>%
  filter(duty_cycle == 'batch_1' | some_det) %>%
  select(-some_det) %>%
  mutate_at(vars(-(tag_id:cap_hist)),
            list(~ if_else(is.na(.), 0, .))) %>%
  mutate(LLRTP = if_else(duty_cycle == "batch_1",
                         1, NA_real_)) %>%
  select(tag_id:cap_hist, LLRTP, everything()) %>%
  select(-(tag_id:cap_hist)) %>%
  as.matrix()

# 2018-2019
y = cap_hist_list$tag_df %>%
  filter(release_site == 'LLRTP') %>%
  select(tag_id, release_site, duty_cycle) %>%
  left_join(cap_hist %>%
              select(-(DG:DC))) %>%
  mutate(some_det = if_else(is.na(cap_hist), F, T)) %>%
  filter(duty_cycle == 'batch_1' | some_det) %>%
  select(-some_det) %>%
  mutate_at(vars(-(tag_id:cap_hist)),
            list(~ if_else(is.na(.), 0, .))) %>%
  mutate(LLRTP = if_else(duty_cycle == "batch_1",
                       1, NA_real_)) %>%
  select(tag_id:cap_hist, LLRTP, everything()) %>%
  select(-(tag_id:cap_hist)) %>%
  as.matrix()

# 2019-2020
y = cap_hist_list$tag_df %>%
  select(tag_id, release_site, duty_cycle) %>%
  left_join(cap_hist) %>%
  mutate(some_det = if_else(is.na(cap_hist), F, T)) %>%
  filter(duty_cycle == 'batch_1' | some_det) %>%
  select(-some_det) %>%
  mutate(Rel = if_else(duty_cycle == "batch_1" & release_site %in% c('LEMTRP', 'HYDTRP'),
                       1, NA_real_),
         LLRTP = if_else(duty_cycle == "batch_1" & release_site == 'LLRTP',
                         1, NA_real_)) %>%
  select(tag_id:cap_hist, Rel, BC:LF, LLRTP, everything()) %>%
  select(-(release_site:duty_cycle)) %>%
  select(-tag_id, -cap_hist) %>%
  as.matrix()

# put together data for JAGS
jags_data = list(
  N = nrow(y),
  J = ncol(y),
  y = y,
  z = known_alive(y),
  f = first_alive(y)
)

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
  mutate(cv = sd / mean)

qplot(Rhat, data = param_summ)
qplot(ess, data = param_summ)
param_summ %>%
  filter(ess == 0)

param_summ %>%
  filter(grepl('^z\\[', param)) %>%
  mutate(tag = str_extract(param, "[:digit:]+"),
         loc = str_extract(param, "\\,[:digit:]+"),
         loc = str_remove(loc, "^,")) %>%
  mutate_at(vars(tag, loc),
            list(as.integer)) %>%
  arrange(tag, loc) %>%
  # filter(tag == 63)
  group_by(tag) %>%
  filter(loc == max(loc[`50%` == 1])) %>%
  # filter(loc == max(loc[mean > 0.4])) %>%
  # filter(loc == 12)
  tabyl(loc) %>%
  adorn_pct_formatting() %>%
  left_join(tibble(loc = 1:ncol(y),
                   site = colnames(y))) %>%
  select(site, everything()) %>%
  arrange(desc(n))

# plots of estimates
surv_p = param_summ %>%
  filter(grepl('survship', param)) %>%
  mutate(site = factor(colnames(y),
                       levels = colnames(y))) %>%
  ggplot(aes(x = site,
             y = mean)) +
  geom_errorbar(aes(ymin = `2.5%`,
                    ymax = `97.5%`),
                width = 0) +
  geom_point() +
  labs(x = 'Site',
       y = 'Cummulative Survival')

phi_p = param_summ %>%
  filter(grepl('phi', param)) %>%
  mutate(site = factor(colnames(y),
                       levels = colnames(y))) %>%
  ggplot(aes(x = site,
             y = mean)) +
  geom_errorbar(aes(ymin = `2.5%`,
                    ymax = `97.5%`),
                width = 0) +
  geom_point() +
  labs(x = 'Site',
       y = 'Survival From Previous Site')

det_p = param_summ %>%
  filter(grepl('^p\\[', param)) %>%
  mutate(site = factor(colnames(y),
                       levels = colnames(y))) %>%
  ggplot(aes(x = site,
             y = mean)) +
  geom_errorbar(aes(ymin = `2.5%`,
                    ymax = `97.5%`),
                width = 0) +
  geom_point() +
  labs(x = 'Site',
       y = 'Detection Probability')


surv_p
phi_p
det_p
