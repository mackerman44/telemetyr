#########################################################
# A demo script showing an example workflow using the
#   "telemetyr" R package. Here we use the 2018/2019
#   season for the Lemhi River winter study as an
#   example case
#
# Authors: Mike Ackerman, Kevin See, Nick Porter
# First Created: 05/12/2020

# load libraries
library(telemetyr)
library(tidyverse)
library(ggplot2)

# set working directory
setwd("S:/mike/tmp/workflow_script")

#------------------------
# READ AND COMPRESS DATA
#------------------------
# read in data
raw = read_txt_data(path = "S:/data/telemetry/lemhi/fixed_site_downloads/2018_2019")

# the above could then be saved and later loaded to save time e.g.
save(raw, file = "data/raw.Rda")
load("data/raw.Rda")

# clean data, round tag codes, compress data, and assign weeks
compressed = compress_raw_data(raw_data = raw,
                               min_yr = 2018,
                               max_yr = 2019,
                               filter_valid = T,
                               round_to = 5,
                               max_min = 2,
                               assign_week = T,
                               week_base = "0901",
                               append_week = "first")
save(compressed, file = "data/compressed.Rda")

# also, option to save data as .csv if necessary
library(readr)
write_csv(compressed, "data/compressed.csv")

#------------------------
# DIAGNOSTICS, ETC.
#------------------------
# set receivers to perform diagnostics (remove activation and test receivers)
receiver_nms = unique(compressed$receiver) %>%
  setdiff(c("ACT", "TT1", "TT2"))

# operations summary
?summarise_timer_data
operations_summary = summarise_timer_data(compress_df = compressed,
                                          receiver_codes = receiver_nms,
                                          season_start = "20180915",
                                          season_end = "20190401",
                                          include_noise = T)
# contains 3 objects; note you can save any of these using save, write_csv, or ggsave() in the case
# of plots
operations_summary$operations_summ
operations_summary$p_operational
operations_summary$operations_plot

# noise summary
?summarise_noise_data
noise_summary = summarise_noise_data(compress_df = compressed,
                                     receiver_codes = receiver_nms,
                                     operations_summary = operations_summary$operations_summ)
# 2 objects
noise_summary$noise_tbl
noise_summary$noise_plot

# tag battery life
?summarise_test_data
test_summary = summarise_test_data(compress_df = compressed,
                                   tag_data = tag_releases)
# note that you can use the already loaded 'tag_releases' data for tag_data in summarise_test_data().
# and it uses the years in compressed to determine which test tags to grab.
# 4 objects
test_summary$test_tag_ids
test_summary$test_df
test_summary$tag_life
test_summary$tag_life_p

# volt or temp information
volt_temp_df = read_volt_temp_data(path = download_path)
volt_p = plot_volt_temp_data(volt_temp_df,
                             column = "volt_avg",
                             receiver_codes = receiver_nms)

#------------------------
# CAPTURE HISTORIES
#------------------------
# prep tag list for capture histories
fish_releases_1819 = tag_releases %>%
  filter(season == "18_19",
         tag_purpose == "fish")

# prep site list for capture histories
sites_1819 = site_metadata %>%
  filter(use18_19 == TRUE,
         site_type == "rt_fixed") %>%
  select(site = site_code,
         receivers) %>%
  group_by(site) %>%
  nest() %>%
  ungroup() %>%
  mutate(receiver = map(data,
                        .f = function(x) {
                          str_split(x, "\\,") %>%
                            extract2(1) %>%
                            str_trim()
                        })) %>%
  select(-data) %>%
  unnest(cols = receiver) %>%
  mutate_at(vars(site, receiver),
            list(~ factor(., levels = unique(.))))

# Note the above is just one way to go from the site_metadata down to a 2-column dataframe
# with "site" and "receiver", but you could create or import this any way. You'll just want
# it to be a factor with levels defining the order of sites.

# prepare capture histories
cap_hist_list = prep_capture_history(compressed,
                                     tag_data = fish_releases_1819,
                                     n_obs_valid = 3,
                                     rec_site = sites_1819,
                                     delete_upstream = T,
                                     location = "site",
                                     output_format = "all")

save(cap_hist_list, file = "data/cap_hist_list.Rda")

#------------------------
# CJS MODEL
#------------------------
library(magrittr)
library(postpack)

# pull out the needed objects from cap_hist_list
cap_hist = cap_hist_list$ch_wide
tag_df = cap_hist_list$tag_df

# add records for those fish that were never observed
cap_hist %<>%
  full_join(tag_df %>%
              filter(duty_cycle == "batch_1") %>%
              select(tag_id),
            by = "tag_id") %>%
  mutate(ch_width = nchar(cap_hist)) %>%
  fill(ch_width) %>%
  rowwise() %>%
  mutate(cap_hist = if_else(is.na(cap_hist),
                            as.character(paste0(rep(0, ch_width), collapse = '')),
                            cap_hist)) %>%
  select(-ch_width) %>%
  mutate_at(vars(-tag_id, -cap_hist),
            list(~ if_else(is.na(.), 0, .)))

y = cap_hist %>%
  mutate(Rel = 1) %>%
  select(tag_id, cap_hist, Rel, everything()) %>%
  select(-tag_id, -cap_hist) %>%
  as.matrix()

jags_data = list(
  N = nrow(y),         # number of fish
  J = ncol(y),         # number of sites
  y = y,               # capture histories
  z = known_alive(y)   # known alive matrix
)

# SPECIFY THE JAGS MODEL
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
    # j = 1 is the release occasion - known alive; i.e., the mark event
    for (j in 2:J) {
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
jags_file = "model.txt"
write_model(jags_model, jags_file)

# specify which parameters to track
jags_params = c("phi", "p", "survship")

# FIT JAGS MODEL AND GET mcmc.list OF SAMPLES FROM POSTERIOR
library(rjags)
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

# CJS RESULTS
param_summ = post_summ(post,
                       jags_params,
                       Rhat = T,
                       ess = T) %>%
  t() %>%
  as_tibble(rownames = "param") %>%
  mutate(cv = sd / mean)

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

# MODEL DIAGNOSTICS
# looking for "grassy" plots here to assess convergence of chains. These look pretty good!
diag_plots(post, "phi", layout = "5x3", ext_device = T)      # p(surv) between sites
diag_plots(post, "survship", layout = "5x3", ext_device = T) # p(surv) to a location
diag_plots(post, "^p[", layout = "5x3", ext_device = T)      # p(detetion)
