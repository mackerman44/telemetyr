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
# set NAS prefix, depending on operating system
#-------------------------
if(.Platform$OS.type != 'unix') {
  nas_prefix = "S:"
}
if(.Platform$OS.type == 'unix') {
  nas_prefix = "~/../../Volumes/ABS"
}


#-------------------------
# read in RT site metadata
#-------------------------
rec_meta = read_excel(paste0(nas_prefix, '/data/telemetry/lemhi/site_metadata/rt_site_metadata.xlsx'))

# which RT receivers were used each year?
# rec_df = rec_meta %>%
#   filter(site_type == 'rt_fixed') %>%
#   filter(as.integer(str_sub(rt_rkm, end = 3)) <= 274) %>%
#   mutate_at(vars(site_code),
#             list(~ factor(., levels = unique(.)))) %>%
#   gather(season, use, starts_with("use")) %>%
#   mutate(season = str_remove(season, "use")) %>%
#   filter(use) %>%
#   select(-use) %>%
#   select(season, everything())
#
#
rec_df = rec_meta %>%
  filter(site_type %in% c('rt_fixed', 'rst')) %>%
  arrange(desc(rt_rkm)) %>%
  select(site = site_code,
         receivers, rt_rkm) %>%
  group_by(site, rt_rkm) %>%
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
  mutate(receiver = if_else(grepl('NA', receiver),
                            NA_character_,
                            receiver)) %>%
  mutate_at(vars(site, receiver),
            list(~ factor(., levels = unique(.))))


# drop a few upstream sites for these analyses
rec_df %<>%
  filter(!site %in% c('PAHTRP', 'DG', 'KP', 'DC', 'HYDTRP')) %>%
  # filter(!site %in% c("LLRTP")) %>%
  mutate_at(vars(site, receiver),
            list(fct_drop)) %>%
  mutate(site_num = as.integer(site))


#-------------------------
# write Bayesian CJS model
#-------------------------
model_file = "analysis/CJS_models/CJS_model.txt"
write_bayes_cjs(model_file)

#-------------------------
# load capture histories
#-------------------------
rt_cjs = tibble(season = c('17_18',
                           '18_19',
                           '19_20')) %>%
  mutate(cap_hist_list = map(season,
                             .f = function(yr) {
                               load(paste0(nas_prefix, "/Nick/telemetry/raw/cap_hist_", yr, ".rda"))
                               return(cap_hist_list)
                             }),
         cap_hist_wide = map(cap_hist_list,
                             .f = "ch_wide"),
         tag_meta = map(cap_hist_list,
                        .f = "tag_df"),
         # for 2019-2020, force all tags released at LLRTP to be batch_3 so they enter model at first detection, and drop LLRTP as a site
         tag_meta = map(tag_meta,
                        .f = function(x) {
                          x %>%
                            mutate(duty_cycle = if_else(season == "19_20" & release_site == "LLRTP",
                                                        "batch_3",
                                                        duty_cycle))
                        }),
         cap_hist_wide = map2(season,
                              cap_hist_wide,
                              .f = function(x, y) {
                                if(x == '19_20') {
                                  y %<>%
                                    select(-LLRTP)
                                }
                                return(y)
                              }),
         jags_data = map2(cap_hist_wide,
                          tag_meta,
                          .f = function(x, y) {
                            prep_jags_cjs(x, y)
                          }),
         cjs_post = map(jags_data,
                        .f = function(x) {
                          run_jags_cjs(file_path = model_file,
                                       jags_data = x,
                                       n_burnin = 5000,
                                       n_iter = 5000,
                                       n_thin = 10)
                        }),
         param_summ = map2(cjs_post,
                           jags_data,
                           .f = function(x, j_data) {
                             summarise_jags_cjs(x,
                                                Rhat = T,
                                                ess = T) %>%
                               left_join(tibble(site = colnames(j_data$y)) %>%
                                           mutate(site = factor(site, levels = site),
                                                  site_num = as.integer(site))) %>%
                               select(param_grp, site_num,
                                      site,
                                      param,
                                      everything())
                           }))

param_summ_all = rt_cjs %>%
  select(season, param_summ) %>%
  unnest(cols = param_summ) %>%
  # ignore parameter estimates for last site each season
  group_by(season) %>%
  filter(site_num < max(site_num)) %>%
  ungroup() %>%
  mutate(site = factor(site,
                       levels = levels(rec_df$site))) %>%
  arrange(season, param_grp, site)

# save the results
save(rec_df, rt_cjs, param_summ_all,
     file = 'analysis/CJS_models/RT_only_CJS_all.rda')

#-----------------------------------
# Make some plots
#-----------------------------------
dodge_width = 0.3

det_p = param_summ_all %>%
  filter(param_grp == 'p') %>%
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
  filter(param_grp == 'phi') %>%
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
  filter(param_grp == 'survship') %>%
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
# redo posterior samples
surv_llrtp_post = rt_cjs %>%
  filter(season == '19_20') %>%
  pull(cjs_post) %>%
  extract2(1) %>%
  as.matrix(chain = T,
            iter = T) %>%
  as_tibble() %>%
  select(CHAIN, ITER, starts_with("phi")) %>%
  pivot_longer(-(CHAIN:ITER),
               names_to = 'param',
               values_to = 'value') %>%
  mutate(param_grp = str_extract(param, "[:alpha:]+"),
         site_num = str_extract(param, "[:digit:]+"),
         site_num = as.integer(site_num)) %>%
  filter(site_num >= 7) %>%
  mutate_at(vars(value),
            list(~ if_else(site_num == 7,
                           1, .))) %>%
  group_by(CHAIN, ITER) %>%
  mutate(survship = cumprod(value)) %>%
  ungroup() %>%
  mutate(param = str_replace(param, "phi", "survship")) %>%
  select(CHAIN, ITER, param, survship) %>%
  pivot_wider(names_from = "param",
              values_from = "survship")

# summarise survival parameters for 19_20 from LLRTP downstream
surv_summ = rt_cjs %>%
  filter(season == '19_20') %>%
  pull(cjs_post) %>%
  extract2(1) %>%
  as.matrix(chain = T,
            iter = T) %>%
  as_tibble() %>%
  select(-(`survship[7]`:`survship[17]`)) %>%
  left_join(surv_llrtp_post) %>%
  split(list(.$CHAIN)) %>%
  map(.f = as.mcmc) %>%
  as.mcmc.list() %>%
  summarise_jags_cjs(Rhat = T,
                     ess = T) %>%
  mutate(season = '19_20') %>%
  select(season, everything()) %>%
  filter(site_num < max(site_num)) %>%
  left_join(param_summ_all %>%
              select(season, param, site_num, site)) %>%
  select(one_of(names(param_summ_all))) %>%
  arrange(season, param_grp, site_num)


surv_p2 = param_summ_all %>%
  anti_join(surv_summ %>%
              select(season, param)) %>%
  bind_rows(surv_summ) %>%
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

#----------------------------------------------------------
# some diagnostic plots
#----------------------------------------------------------
# with mcmcr package
library(mcmcr)
i = 3
my_mcmcr = rt_cjs %>%
  pull(cjs_post) %>%
  extract2(i) %>%
  as.mcmcr

# get Rhat statistics for all parameters
conv_df = rhat(my_mcmcr,
               by = 'term',
               as_df = T) %>%
  left_join(esr(my_mcmcr,
                by = 'term',
                as_df = T)) %>%
  # which parameters have converged and which haven't?
  left_join(converged(my_mcmcr,
                      by = 'term',
                      as_df = T))

# how many parameters did not converge?
sum(conv_df$converged == F)

conv_df %>%
  arrange(esr) %>%
  head(10)

# with ggmcmc package
library(ggmcmc)
i = 3
my_ggs = rt_cjs %>%
  pull(cjs_post) %>%
  extract2(i) %>%
  # ggs(family = c('p')) %>%
  # filter(grepl('^p\\[', Parameter))
  ggs(family = c('phi'))
  # ggs(family = c('survship'))


dens_p = ggs_density(my_ggs) +
  stat_function(fun = function(x) dbeta(x, 1, 1),
                color = 'black') +
  facet_wrap(~ Parameter)
trace_p = ggs_traceplot(my_ggs) +
  facet_wrap(~ Parameter)
run_mean_p = ggs_running(my_ggs)
rhat_p = ggs_Rhat(my_ggs)
geweke_p = ggs_geweke(my_ggs)
ggs_autocorrelation(my_ggs)
ggs_crosscorrelation(my_ggs)


#----------------------------------------------------------
# compare with other CJS models
library(marked)
library(ggrepel)

rt_marked = rt_cjs %>%
  mutate(ch_proc = map(jags_data,
                       .f = function(j_data) {
                         j_data$y %>%
                           as_tibble() %>%
                           unite("ch", everything(),
                                 sep = '') %>%
                           process.data()
                       }),
         ch_ddl = map(ch_proc,
                      .f = make.design.data),
         crm_mod = map2(ch_proc,
                        ch_ddl,
                        .f = function(x, y) {
                          crm(data = x,
                              ddl = y,
                              model.parameters = list(Phi = list(formula = ~ -1 + time),
                                                      p = list(formula = ~ -1 + time)),
                              hessian = T)
                        })) %>%
  mutate(crm_res = map2(crm_mod,
                        jags_data,
                       .f = function(x, j_data) {
                         x$results$reals %>%
                           map_df(.id = 'param_grp',
                                  .f = as_tibble) %>%
                           mutate(param_grp = recode(param_grp,
                                                     'Phi' = 'phi')) %>%
                           mutate(site_num = if_else(param_grp == 'p',
                                                     occ,
                                                     as.integer(occ + 1))) %>%
                           left_join(tibble(site = colnames(j_data$y)) %>%
                                       mutate(site = factor(site, levels = site),
                                              site_num = as.integer(site)))
                       }))

marked_param = rt_marked %>%
  select(season, crm_res) %>%
  unnest(cols = crm_res) %>%
  mutate(site = factor(site,
                       levels = levels(rec_df$site))) %>%
  arrange(season, param_grp, site) %>%
  select(-time, -occ)

# add cummulative survival
marked_param %<>%
  bind_rows(marked_param %>%
              filter(param_grp == 'phi') %>%
              group_by(season) %>%
              mutate_at(vars(estimate, lcl, ucl),
                        list(cumprod)) %>%
              mutate(se = NA_real_) %>%
              mutate(param_grp = 'survship') %>%
              ungroup()) %>%
  arrange(season, param_grp, site)

comp_df = param_summ_all %>%
  select(-param) %>%
  left_join(marked_param %>%
              select(-site_num)) %>%
  filter(!is.na(estimate))

comp_df %>%
  mutate(diff = estimate - mean) %>%
  arrange(desc(abs(diff)))

comp_df %>%
  mutate(ci_width_b = `97.5%` - `2.5%`,
         ci_width_f = ucl - lcl,
         ci_diff = ci_width_f - ci_width_b) %>%
  filter(ci_width_f < 1) %>%
  arrange(desc(abs(ci_diff)))

comp_df %>%
  filter(ucl - lcl < 1) %>%
  ggplot(aes(x = estimate,
             y = mean)) +
  geom_errorbar(aes(ymin = `2.5%`,
                    ymax = `97.5%`),
                width = 0,
                color = 'gray40') +
  geom_errorbarh(aes(xmin = lcl,
                     xmax = ucl),
                 height = 0,
                 color = 'gray40') +
  geom_point(size = 3) +
  geom_abline(linetype = 2,
              color = 'red') +
  theme_bw() +
  facet_grid(season ~ param_grp) +
  labs(x = 'marked',
       y = 'JAGS')
