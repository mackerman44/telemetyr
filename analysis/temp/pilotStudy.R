###########################################################################################################################
#
# Data Analysis For RT Pilot Study
#
# This script is the analysis for a pilot study tracking the migration, distribution, and survival of Chinook salmon
# pre-smolts that are tagged at a rotary screw trap in the lower Lemhi River and subsequently tracked through the mainstem
# Salmon River from the town of Salmon, ID to the Riggins, ID area.
#
# Created by Mike Ackerman on 4/4/2018
#    with a LOT of assistance from Kevin See
#
###########################################################################################################################

rm(list = ls()) # clear the working environment
#setwd('C:/Users/mikea/Dropbox/Projects/BOR/RT/R/')
setwd('R')

# load necessary libraries
library(tidyverse)
library(lubridate)
library(marked)
library(boot)
library(magrittr)
library(readxl)
library(psych)
library(msm)

# source readRTdata.R. This file contains functions to read in the radio telemetry data output from NOAA's Tracker software
source('rtFuncs.R')

########################################
# deal with data that had been missing #
########################################

# Path to previously missing data
missPath = '../Data/missingAdata/2017-2018'
# 
# missDataList = compress.txt.to.csv(missPath)

# compress the txt files that contain the missing data to csv files. save as csv file if one doesn't exist
# fileNms = str_split(names(missDataList), '\\/', simplify = T) %>%
#   as.data.frame() %>%
#   tbl_df() %>%
#   rename(site = V1,
#          txtFile = V2) %>%
#   mutate(fileNm = if_else(as.integer(str_sub(txtFile, 1, 1)) >= 2,
#                           paste0('17', txtFile),
#                           paste0('18', txtFile)),
#          fileNm = str_replace(fileNm, '.txt$', '.csv'))
# 
# identical(nrow(fileNms), length(missDataList))
# 
# for(i in 1:length(missDataList)) {
#   missDataList[[i]] %>%
#     mutate_at(vars(start, end),
#               funs(as.character(paste0(month(.), '-', day(.), '-', year(.), ' ', hour(.), ':', minute(.), ':', second(.))))) %>%
#     write_csv(paste(missPath, fileNms$site[i], fileNms$fileNm[i], sep = '/'),
#               col_names = F)
# }

################
# READ IN DATA #
################

# set path to receiver downloads
rt_path = '../SiteDownloads/2017_2018'

# get all the .csv files
csv_df = read.csv.data(path = rt_path)

# grab the data that had been missing
miss_df = read.csv.data(path = missPath)

# let's clean up csv_df a bit and rename it rt_df, also merge in formerly missing data
rt_df = csv_df %>%
  bind_rows(miss_df) %>%
  mutate(receiver = recode(receiver, 
                           'BR1' = 'TB1',      # recode BR1 to TB1
                           'BR2' = 'TB2',      # recode BR2 to TB2
                           '039' = 'TT1')) %>% # recode 039 to TT1
  arrange(receiver, tag_id, start) %>%
  filter(valid == 1) %>%
  mutate(tag_id = as.character(tag_id))

# parse rt_df into a few new useful datasets
timer_df = rt_df %>% # data frame containing all of the timer tag data
  filter(grepl('575$', tag_id))# %>%
  #filter(n >= 5)     # only consider timer tag data valid if >= 3 observations were made, for a channel, each hour

# data frame containing all of the 995 noise data
noise_df = rt_df %>% # data frame containing all of the 995 noise data
  filter(grepl('995$', tag_id))

##########################
# SITE OPERATIONAL TIMES #
##########################

# Examine when receivers were operational versus down using timer_df
#
# This section originally created by Kevin See on 3/22/2018
#   modified by: Mike Ackerman 4/4/2018

# range of time for timer tags
hrRange = floor_date(range(timer_df$start), unit = 'hours')
# how many total hours in that range?
nHrs = difftime(hrRange[2], hrRange[1], units = 'hours') %>%
  as.integer()
nHrs
# how many days were receivers/sites operational?
nDays = round(nHrs/24,0)
nDays

op_plot_df = timer_df %>%
  mutate(hr = floor_date(start, unit = 'hours')) %>%
  # include all hours in hrRange. This adds a record to timer_df for each instance that no timer tag data exists for a receiver
  # i.e., adds a record for each hour that a receiver was not operational
  full_join(expand.grid(list(receiver = unique(timer_df$receiver),
                             hr = hrRange[1] + dhours(seq(0, nHrs)))) %>%
              tbl_df()) %>%
  # when did each site first come online?
  left_join(timer_df %>%
              group_by(receiver) %>%
              summarise(start_hr = min(floor_date(start, unit = 'hours'), na.rm = T)) %>%
              ungroup()) %>%
  mutate(operational = ifelse(!is.na(n), 1, 0)) %>%
  arrange(receiver, hr) %>%
  # for each hour, is site operational?
  group_by(receiver, start_hr, hr) %>%
  summarise_at(vars(operational),
               funs(max)) %>%
  mutate(operational = ifelse(operational == 1, T, F)) %>%
  ungroup() %>%
  mutate(receiver = factor(receiver, levels = c('LH1','LH2','DC1','DC2','MB1','MB2','TR1','TR2','RR1','RR2','BG1','BG2','NF1','NF2',
                                                'DW1','DW2','LR1','LR2','SR1','SR2','CC1','CC2','VC1','VC2','SB1','SB2','TB1','TB2')))

# Calculate the proportion of time that each receiver was operation from the time it first came online to the final time the
# timer tag was observed
p_time_df = op_plot_df %>%
  mutate(site = substr(receiver, 1, 2)) %>%
  left_join(op_plot_df %>%
              filter(operational == T) %>%
              group_by(receiver) %>%
              summarise(end_hr = max(floor_date(hr, unit = 'hours'), na.rm = T)) %>%
              ungroup()) %>%
  filter(hr >= start_hr) %>%
  filter(hr <= end_hr) %>%
  group_by(receiver) %>%
  summarise(p_op = sum(operational == T) / length(operational)) %>%
  ungroup() 
p_time_df

# plot operational times for each of the receivers
p_time_p = op_plot_df %>%
  ggplot(aes(x = hr,
             y = fct_rev(receiver),
             color = operational)) +
  geom_line(size = 2,
            color = 'black') +
  geom_point(data = op_plot_df %>%
               filter(!operational),
             size = 1.5, color = 'orange') +
  theme_bw() +
  labs(x = 'Date - Hour',
       y = 'Receiver') +
  theme(axis.text.x = element_text(color = 'black', size = 12),
        axis.text.y = element_text(color = 'black', size = 12),
        axis.title.x = element_text(color = 'black', size = 14),
        axis.title.y = element_text(color = 'black', size = 14))
p_time_p  

# save a copy of operations plot
ggsave('figures/operationalPlot.pdf',
       p_time_p,
       width = 16,
       height = 6,
       units = 'in')

##############
# SITE NOISE #
##############

# examine noise data for each receiver
noise_tbl = noise_df %>%
  filter(!receiver %in% c('MT1','TT1')) %>%
  mutate(channel = as.numeric(substr(tag_id, 1, 1))) %>%
  group_by(receiver, channel) %>%
  summarise(n_noise = sum(n)) %>%
  spread(channel, n_noise) %>%
  left_join(op_plot_df %>%
              group_by(receiver) %>%
              summarise(hours = sum(operational)) %>%
              ungroup()) %>%
  ungroup() %>%
  mutate_at(vars('1':'9'), funs(. / hours)) %>%
  mutate_at(2:10, round, 1) %>%
  mutate(receiver = factor(receiver, levels = c('LH1','LH2','DC1','DC2','MB1','MB2','TR1','TR2','RR1','RR2','BG1','BG2','NF1','NF2',
                                                'DW1','DW2','LR1','LR2','SR1','SR2','CC1','CC2','VC1','VC2','SB1','SB2','TB1','TB2'))) %>%
  arrange(receiver)

# calculate average noise across channels for each receiver
noise_avg = noise_tbl %>%
  select(-receiver) %>%
  rowMeans()

# tack average noise onto noise_tbl
noise_tbl = noise_tbl %>%
  mutate(Avg = round(noise_avg,1))
noise_tbl

# Write noise_tbl
write.csv(noise_tbl, file = 'output/noisePerHour.csv', row.names = F)

# Plot noise per channel and average as a heatmap. IMPROVE THIS LATER!!!!
noise_p = noise_tbl %>%
  gather(channel, value, -receiver) %>%
  ggplot(aes(x = channel, 
             y = fct_rev(receiver))) + 
  geom_tile(aes(fill = value)) +
  scale_fill_gradient2(low = 'white', high = 'red') +
  theme_bw() +
  labs(x = 'Channel',
       y = 'Receiver',
       title = 'Noise/Hour') +
  theme(axis.text.x = element_text(color = 'black', size = 12),
        axis.text.y = element_text(color = 'black', size = 12),
        axis.title.x = element_text(color = 'black', size = 14),
        axis.title.y = element_text(color = 'black', size = 14),
        plot.title = element_text(size = 16),
        legend.title = element_blank(),
        legend.text = element_text(size = 12))
noise_p 

# save a copy of noise plot
ggsave('figures/noisePlot.pdf',
       noise_p,
       width = 10,
       height = 10,
       units = 'in')

#################
# DATA CLEANING #
#################

# clean up radio telemetry data so it only contains valid detections

# First, let's read in tag data
tag_df = read.csv('../Data/Tags/2017_2018/Release/TagReleases2017.csv') %>%
  mutate(Activation = as.POSIXct(Activation, format = '%m/%d/%Y %H:%M')) %>%
  mutate(Release = as.POSIXct(Release, format = '%m/%d/%Y %H:%M')) %>%
  mutate(tag_id = str_sub(RadioTag, end = -2))

# create some tag lists
fish_tags = tag_df %>%
  filter(TagType == 'Fish') %>%
  select(tag_id) %>%
  as.matrix() %>%
  as.vector()

const_tags = tag_df %>%
  filter(TagType == 'Fish',
         DutyCycle == 'constant') %>%
  select(tag_id) %>%
  as.matrix() %>%
  as.vector()

on_off_tags = tag_df %>%
  filter(TagType == 'Fish',
         DutyCycle == 'onOff') %>%
  select(tag_id) %>%
  as.matrix() %>%
  as.vector()

test_tags = tag_df %>%
  filter(TagType == 'Test') %>%
  select(RadioTag, TagType, DutyCycle, tag_id)

# Create new tagDf object with cleaned fish tag data
tagDf = tag_df %>%
  tbl_df() %>%
  filter(TagType == 'Fish') %>%
  select(tag_id,
         PITTagNum,
         Release,
         SRR:Weight,
         Name,
         DutyCycle,
         everything())

# first, clean up rt_df
rt = rt_df %>%
  filter(!grepl('995$', tag_id)) %>% # filter out noise
  filter(!grepl('575$', tag_id)) %>% # filter out timer tag data
  filter(tag_id %in% fish_tags) %>%  # filter to only include tag_ids included in released fish
  mutate(site = str_sub(receiver, 1, 2)) %>% # add site column
  filter(!site %in% c('MT','AC','TT')) %>% # remove aberrent sites (mobile, activation, test)
  arrange(tag_id, start) 

# grab release times for fish
release_times = tag_df %>%
  filter(TagType == 'Fish') %>%
  select(tag_id, Release)

# filter any record where observation is before the release time
rt = rt %>%
  left_join(release_times) %>%
  filter(start > Release) %>%
  select(-Release)

# clean fish observations by receiver  
fish_obs_unf = addGrpRec(rt)

# NOTE: We can filter on detections by receiver OR detections by site
fish_obs_rec = fish_obs_unf #%>%
  #filter(n >= 3) # only accept >= 3 observations after grouping sequetial observations

# for each fish, group sequential receiver detections into detections by site
fish_obs_site = addGrpSite(fish_obs_rec) %>%
  filter(n >= 5) # filter to include only detections where fish was observed at site >= x times

# define site order. used to remove upstream fish movement
rt_site_order = data.frame(site = c('Rel','LH','MB','TR','RR','BG','NF','DW','LR','SR','CC','VC','SB','TB'),
                           site_order = 1:14)

# remove upstream movement from dataset
fish_obs = dropUpStrm(fish_obs_site, rt_site_order)

# add observations for initial capture/release at RST
rel_obs = tag_df %>%
  filter(TagType == 'Fish') %>%
  select(tag_id, Release)  %>%
  mutate(site = 'Rel') %>%
  mutate(site_order = 1) %>%
  rename(end = Release) %>%
  mutate(start = end)

# bind fish_obs and rel_obs
fish_obs = fish_obs %>%
  bind_rows(rel_obs) %>%
  arrange(tag_id, start)

# save fish release data and cleaned observation data
save(fish_obs, tagDf, file = "fish_obs.Rdata")

# parse data to separate observations for constant and on-off fish
const_obs = fish_obs %>%
  filter(tag_id %in% const_tags)

on_off_obs = fish_obs %>%
  filter(tag_id %in% on_off_tags)

##############################
# PLOT RECEIVER OBSERVATIONS #
##############################
# Let's examine how many times fish were detected at each receiver. i.e., how good are the antennas/receivers
rec_obs = fish_obs_rec %>%
  select(receiver, n) %>% 
  filter(receiver != 'DC2') %>%
  mutate(receiver = factor(receiver, levels =
                            c('LH1','LH2','MB1','MB2','TR1','TR2','RR1','RR2','BG1','BG2','NF1','NF2','DW1','DW2',
                              'LR1','LR2','SR1','SR2','CC1','CC2','VC1','VC2'))) %>%
  filter(n >= 3) %>%
  filter(n <= 100)

rec_obs_p = ggplot(rec_obs, aes(x = n)) +
  geom_histogram(bins = 25, fill = 'steelblue') +
  scale_x_continuous(limit = c(1,50)) +
  theme_classic() +
  labs(x = 'Observations', y = 'Frequency') +
  theme(axis.text.x = element_text(color = 'black', size = 10),
        axis.text.y = element_text(color = 'black', size = 10)) +
  facet_wrap(~ receiver)
rec_obs_p

# save a copy of noise plot
ggsave('figures/ObsByReceiver.pdf',
       rec_obs_p,
       width = 10,
       height = 10,
       units = 'in')
  
#####################
# CAPTURE HISTORIES #
#####################
# create df that indicates each unique site that each fish was observed at. i.e., remove duplicate observations
fish_by_site = fish_obs %>%
  select(site, tag_id) %>%
  group_by(tag_id) %>%
  distinct(site) %>%
  ungroup() %>%
  mutate(site = factor(site,
                       levels = c('Rel','LH','MB','TR','RR','BG','NF','DW','LR','SR','CC','VC','SB','TB')),
         site_order = as.integer(site)) %>%
  filter(!is.na(site))

# create capture history
ch_df = fish_by_site %>%
  select(-site_order) %>%
  mutate(seen = 1) %>%
  spread(site, seen, fill = 0) %>%
  # add the fish never seen anywhere
  bind_rows(tibble(tag_id = fish_tags) %>%
              anti_join(fish_by_site %>%
                          select(tag_id) %>%
                          distinct())) %>%
  mutate_at(vars(-tag_id),
            funs(ifelse(is.na(.), 0, .))) %>%
  arrange(tag_id)

# save capture histories
write.csv(ch_df, 'output/pilot_ch.csv')

# parse capture histories by constant and of-off
constant_ch = ch_df %>%
  filter(tag_id %in% const_tags)

on_off_ch = ch_df %>%
  filter(tag_id %in% on_off_tags)

##################################
# SIMPLE DETECTION PROBABILITIES #
##################################

# calculate detection probability for each site, based on tags detected downstream of that site. Use only capture histories from constant fish
det_probs = NULL
for(i in 2:(ncol(constant_ch) - 1)) {
  tagsSeen = constant_ch %>%
    select(1, i) %>%
    mutate(downstream = ifelse(rowSums(constant_ch[,-c(1:i)]) >= 1,
                               1,
                               0)) %>%
    filter(downstream == 1) %>%
    select(2,3) %>%
    colSums()
  
  det_probs = det_probs %>%
    bind_rows(tibble(Site = names(constant_ch)[i],
                     DownstreamTags = tagsSeen[2],
                     SiteTags = tagsSeen[1],
                     DetectProb = tagsSeen[1] / tagsSeen[2],
                     DetectProb_SE = sqrt((DetectProb * (1 - DetectProb)) / tagsSeen[2]),
                     DetectProb_CV = DetectProb_SE / DetectProb))
}

det_probs %>%
  mutate_at(vars(DownstreamTags, SiteTags),
            funs(as.integer)) %>%
  write_csv('output/DetectionProbs.csv')

############# 
# CJS MODEL #
#############

# cormack-jolly-seber model
cjs_df = constant_ch %>%
  # add a one for the release site
  #mutate(Rel = 1) %>%
  # construct the capture history
  mutate(ch = paste0(Rel, LH, MB, TR, RR, BG, NF, DW, LR, SR, CC, VC)) %>%
  # add some other information
  left_join(tag_df %>%
              select(tag_id, Release, Length, Weight) %>%
              distinct()) %>%
  select(tag_id, Release, Length, Weight, ch)

# # process data for the marked R packge
# rt_proc = cjs_df %>%
#   process.data()
# 
# # set up model design
# RTddl = make.design.data(rt_proc)
# 
# # fit CJS model
# mod = crm(rt_proc,
#           RTddl,
#           model.parameters = list(Phi = list(formula = ~ -1 + time),
#                                   p = list(formula = ~ -1 + time)))

# fit CJS model
mod = crm(cjs_df,
          model.parameters = list(Phi = list(formula = ~ -1 + time),
                                  p = list(formula = ~ -1 + time)),
          method = 'Nelder-Mead')

# get standard errors
mod_hess = cjs.hessian(mod)

# detection probabilities
detect_est = inv.logit(mod$results$beta$p)
names(detect_est) = paste('p', names(detect_est), sep = '.')

# survival and movement probabilities
Phi_est = inv.logit(mod$results$beta$Phi)
names(Phi_est) = paste('Phi', names(Phi_est), sep = '.')
# names(Phi_est) = paste(c('Release', names(constant_ch)[-c(1, ncol(constant_ch))]), names(constant_ch)[-1], sep = ' to ')

# standard errors
betas = c(Phi_est, detect_est)

# using delta method and derivative of inverse logit
# # this is a conservative estimate of standard error, because it assumes parameter estimates are independent.
se = sqrt((exp(betas) / ((1 + exp(betas))^2))^2 * diag(mod_hess$results$beta.vcv)[names(betas)])

# this is better
# cov_mat = diag((exp(betas) / ((1 + exp(betas))^2))^2) %*% mod_hess$results$beta.vcv
# se = sqrt(diag(cov_mat))
# names(se) = names(betas)

# put them all together
param_est = tibble(Type = 'Detection',
                   Parameter = names(detect_est),
                   Estimate = detect_est) %>%
  bind_rows(tibble(Type = 'Transition',
                   Parameter = names(Phi_est),
                   Estimate = Phi_est)) %>%
  left_join(tibble(Parameter = names(se),
                   SE = se)) %>%
  mutate(CV = SE / Estimate) %>%
  # drop final detection and survival estimates (unidentifiable)
  filter(!Parameter %in% c(names(detect_est)[length(detect_est)],
                           names(Phi_est)[length(Phi_est)])) %>%
  mutate(Parameter = c(colnames(constant_ch)[-c(1:2, ncol(constant_ch))],
                       paste(c(names(constant_ch)[-c(1, (ncol(constant_ch) - 1), ncol(constant_ch))]), 
                             names(constant_ch)[-c(1:2, ncol(constant_ch))], sep = ' to ')))

# calculate overall movement and survival through the whole system
overall_trans = param_est %>%
  filter(Type == 'Transition') %>%
  select(Estimate) %>%
  as.matrix() %>%
  as.numeric() %>%
  prod()

# write cjs parameter estimates
param_est %<>%
  bind_rows(tibble(Type = 'Transition',
                   Parameter = 'Overall',
                   Estimate = overall_trans)) %>%
  write_csv('output/CJS_Parameter_Estimates.csv')

############################################
# SUMMARY TABLE TO DETECTION PROBABILITIES #
############################################
det_summ = det_probs %>%
  filter(Site != 'Rel') %>%
  left_join(param_est %>%
              filter(Type == 'Detection') %>%
              select(-Type),
            by = c('Site' = 'Parameter')) %>%
  rename(Simp_Det_Prob = DetectProb,
         Simp_Det_SE   = DetectProb_SE,
         Simp_Det_CV   = DetectProb_CV,
         CJS_Det_Prob  = Estimate,
         CJS_Det_SE    = SE,
         CJS_Det_CV    = CV)

################
# BATTERY LIFE #
################

# tag battery life
test_df = rt_df %>%
  filter(tag_id %in% test_tags$tag_id) %>%
  filter(receiver %in% c('ACT','TT1')) %>%
  group_by(tag_id) %>%
  summarise(activation = min(start), dead = max(end)) %>%
  ungroup() %>%
  left_join(test_tags) %>%
  select(-RadioTag) %>%
  mutate(tag_life = as.numeric(difftime(dead, activation, units = 'days'))) %>%
  arrange(DutyCycle, tag_life)

# Estimates of battery life for test tags
tagLife = test_df %>%
  group_by(DutyCycle) %>%
  summarise(mean(tag_life))
tagLife

# plot tag life
test_p <- ggplot(test_df, aes(x = DutyCycle, y = tag_life, fill = DutyCycle)) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.6) +
  theme_bw() +
  theme(legend.position = 'none') +
  stat_summary(fun.y = mean, geom = 'point', shape = 18, size = 3, color = 'red') +
  labs(x = 'Duty Cycle', y = 'Tag Life (Days)')
test_p

# save tag life plot
ggsave('figures/tagLifePlot.pdf',
       test_p,
       width = 5,
       height = 5,
       units = 'in')

##############
# TAG BURDEN #
##############

# set tag weight
tag_weight = 0.5 # grams

burden_df = tag_df %>%
  select(tag_id, Length, Weight, TagType) %>%
  filter(!is.na(Weight)) %>%
  mutate(burden_pct = (tag_weight/Weight))

burden_summ = print(describe(burden_df$burden_pct), digits = 3)

################
# TRAVEL TIMES #
################

# examine first detection at each site
first_obs = fish_obs %>%
  group_by(tag_id, site) %>%
  summarise_at(vars(start),
               funs(min),
               na.rm = T) %>%
  ungroup() %>%
  mutate(site = factor(site,
                       levels = c('Rel','LH','MB','TR','RR','BG','NF','DW','LR','SR','CC','VC','SB','TB')),
         site_order = as.integer(site)) %>%
  arrange(tag_id, site_order)

# how many days between consecutive sites?
travel_time = first_obs %>%
  group_by(tag_id) %>%
  mutate(next_site = lead(site),
         next_site_ord = lead(site_order)) %>%
  filter(next_site_ord - site_order == 1) %>%
  left_join(first_obs %>%
              select(tag_id,
                     next_site = site,
                     next_start = start)) %>%
  mutate(time_hours = as.numeric(difftime(next_start, start, units = 'hours'))) %>%
  mutate(time_days = time_hours/24) %>%
  mutate(label = paste(site, next_site, sep = ' to '),
         label = factor(label,
                        levels = paste(levels(first_obs$site)[1:length(levels(first_obs$site)) -1], 
                                       levels(first_obs$site)[-1], sep = ' to '))) %>%
  filter(time_hours >= 0)

# plot travel times between sites
travel_p = ggplot(travel_time, aes(x = time_days)) +
  geom_histogram(fill = 'lightblue',
                 color = 'blue',
                 binwidth = 0.25) +
  geom_vline(xintercept = 0,
             linetype = 2) +
  facet_wrap(~ label,
             scales = 'free') +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15)) +
  labs(x = 'Days',
       y = 'Count',
       title = 'Travel Time')
travel_p

# summary of travel times
travel_summ = travel_time %>%
  group_by(site, next_site, label) %>%
  summarise (n_tags = n_distinct(tag_id),
             min_hours = min(time_hours),
             median_hours = median(time_hours),
             mean_hours = mean(time_hours),
             max_hours = max(time_hours),
             min_days = min(time_days),
             median_days = median(time_days),
             mean_days = mean(time_days),
             max_days = max(time_days)) %>%
  ungroup() %>%
  write_csv('output/TravelTimeSummary.csv')
travel_summ

# travel speeds between consecutive sites (km/day)
travel_speed = travel_summ %>%
  left_join(tibble(next_site = c('LH','MB','TR','RR','BG','NF','DW','LR','SR','CC','VC','SB','TB'),
                   km = c(6.6, 9, 7.2, 5.5, 6.7, 4.6, 9.2, 14.6, 34.2, 18.2, 124.8, 39.4, 36.6))) %>%
  mutate_at(vars(matches('Days$')),
            funs(km / .)) %>%
  select(site, next_site,
         km,
         n_tags,
         min_speed = max_days,
         median_speed = median_days,
         mean_speed = mean_days,
         max_speed = min_days) %>%
  left_join(travel_summ)
travel_speed
 
##########################
# TAGS PASSING EACH SITE #
##########################

# Using the simple estimates of detection probabilities
simp_fish_pass = first_obs %>%
  left_join(tag_df %>%
              select(tag_id, DutyCycle)) %>%
  group_by(site, DutyCycle) %>%
  summarise(unique_tag = n_distinct(tag_id)) %>%
  spread(DutyCycle, unique_tag) %>%
  mutate(total = constant + onOff) %>%
  select(site, total, constant, onOff) %>%
  left_join(det_probs %>%
              select(Site, DetectProb, DetectProb_SE),
            by = c('site' = 'Site')) %>%
  mutate(constPass = constant / DetectProb) %>%
  mutate(constPassSE = (constPass*DetectProb_SE) / (DetectProb^2) ) %>%
  mutate(constPassLCI = max(max(constPass - (1.96*constPassSE),0), constant),
         constPassUCI = min(constPass + (1.96*constPassSE), length(const_tags))) %>%
  ungroup() %>%
  mutate(site = factor(site,
                       levels = c('Rel','LH','MB','TR','RR','BG','NF','DW','LR','SR','CC','VC')))

# Using the CJS estimates of detection probabilities
cjs_fish_pass = first_obs %>%
  left_join(tag_df %>%
              select(tag_id, DutyCycle)) %>%
  group_by(site, DutyCycle) %>%
  summarise(unique_tag = n_distinct(tag_id)) %>%
  spread(DutyCycle, unique_tag) %>%
  mutate(total = constant + onOff) %>%
  select(site, total, constant, onOff) %>%
  left_join(det_summ %>%
              select(Site, CJS_Det_Prob, CJS_Det_SE) %>%
              bind_rows(data.frame(Site = 'Rel',
                                   CJS_Det_Prob = 1,
                                   CJS_Det_SE = 0)),
            by = c('site' = 'Site')) %>%
  mutate(constPass = constant/CJS_Det_Prob) %>%
  mutate(constPassSE = (constPass*CJS_Det_SE) / (CJS_Det_Prob^2) ) %>%
  mutate(constPassLCI = max(max(constPass - (1.96*constPassSE),0), constant),
         constPassUCI = min(constPass + (1.96*constPassSE), length(const_tags))) %>%
  ungroup() %>%
  mutate(site = factor(site,
                       levels = c('Rel','LH','MB','TR','RR','BG','NF','DW','LR','SR','CC','VC')))

# Using transition probabilities from the CJS model
trans_fish_pass = param_est %>%
  filter(Type == 'Transition',
         Parameter != 'Overall') %>%
  mutate(site = str_sub(Parameter, -2)) %>%
  select(site, Estimate, SE) %>%
  rename(Trans_Prob = Estimate,
         Trans_Prob_SE = SE) %>%
  mutate(Cum_Trans_Prob = cumprod(Trans_Prob),
         Cum_Trans_SE = NA)

# the standard error of a product (for the cumSurvSE) takes a little extra work
for(i in 1:nrow(trans_fish_pass)) {
  if(i == 1) trans_fish_pass$Cum_Trans_SE[i] = trans_fish_pass$Trans_Prob_SE[i]
  
  if(i > 1) {
    varX = (trans_fish_pass$Cum_Trans_SE[i-1])^2
    varY = (trans_fish_pass$Trans_Prob_SE[i])^2
    varXY = varX * varY + varX * trans_fish_pass$Trans_Prob[i]^2 + varY * trans_fish_pass$Trans_Prob[i-1]^2
    trans_fish_pass$Cum_Trans_SE[i] = sqrt(varXY)
    rm(varX, varY, varXY)
  }
}

# Now estimate the number of tags passing each site, based on the number of tags released, and the number of tags we started with
trans_fish_pass %<>%
  mutate(constPass = length(const_tags) * Cum_Trans_Prob,
         constPassSE = length(const_tags) * Cum_Trans_SE) %>%
  group_by(site) %>%
  mutate(constPassLCI = max(constPass - (1.96*constPassSE),0),
         constPassUCI = min(constPass + (1.96*constPassSE), length(const_tags))) %>%
  ungroup() %>%
  mutate(site = factor(site,
                       levels = c('LH','MB','TR','RR','BG','NF','DW','LR','SR','CC')))

# How similar are the estimates of tags passing from each method?
pass_compare = simp_fish_pass %>%
  select(site, constPass, constPassSE) %>%
  rename(Simple_Est = constPass,
         Simple_SE = constPassSE) %>%
  left_join(cjs_fish_pass %>%
              select(site, constPass, constPassSE) %>%
              rename(CJS_Est = constPass,
                     CJS_SE = constPassSE)) %>%
  left_join(trans_fish_pass %>%
              select(site, constPass, constPassSE) %>%
              rename(Trans_Est = constPass,
                     Trans_SE = constPassSE))

# Plot transition probabilities
trans_df = trans_fish_pass %>%
  select(site, Trans_Prob, Cum_Trans_Prob) %>%
  rename(Transition = Trans_Prob,
         Cumulative = Cum_Trans_Prob) %>%
  mutate(Reach = param_est$Parameter[11:20]) %>%
  select(Reach, Transition, Cumulative) %>%
  mutate(Reach = factor(Reach,
                        levels = c('Rel to LH', 'LH to MB', 'MB to TR', 'TR to RR', 'RR to BG', 'BG to NF', 'NF to DW', 'DW to LR',
                                   'LR to SR', 'SR to CC')))

trans_p = trans_df %>%
  ggplot(aes(x = Reach, y = Transition, group = 1)) +
  geom_bar(stat = 'identity', alpha = 0.75, fill = 'steelblue4') +
  geom_line(aes(x = Reach, y = Cumulative), colour = 'black', size = 1.5) +
  theme_classic() +
  labs(x = 'Reach', y = 'Transition Probability') +
  theme(axis.text.x = element_text(color = 'black', size = 10),
        axis.text.y = element_text(color = 'black', size = 10))
trans_p

# save transition probabilities plot
ggsave('figures/transitionPlot.pdf',
       trans_p,
       width = 6,
       height = 6,
       units = 'in')

# Plot fish passing each site. Here, I use the estimates from CJS detection probabilities
fish_pass_df = cjs_fish_pass %>%
  select(site,
         tagObs = constant,
         tagEst = constPass,
         SE = constPassSE,
         LCI = constPassLCI,
         UCI = constPassUCI) %>%
  write_csv('output/nTagsPast.csv')

# use estimates of tags based on observed tags and CJS estimates of detection probabilities
#fish_pass_df %>%
#  mutate(prop = tagEst / length(const_tags)) %>%
#  select(site,
#         DetectProb,
#         tagsObs = constant,
#         tagsEst = const_pass,
#         SE = const_pass_SE,
#         prop) %>%
#  write_csv('output/nTagsPast.csv')

# The plot
fish_pass_p = fish_pass_df %>%
  ggplot(aes(x = site,
             y = tagEst)) +
  geom_point(color = 'black',
             size = 3) +
  geom_errorbar(aes(ymin = LCI,
                    ymax = UCI),
                width = 0.2) +
  geom_hline(yintercept = length(const_tags),
             color = 'blue') +
  labs(x = 'Site', y = 'Number of Tags Passing') +
  theme_classic() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))
fish_pass_p

# Save fish passing plot
ggsave('figures/nTagsPast.pdf',
       fish_pass_p,
       width = 6.5,
       height = 3.5)

####################
# EXAMINE RST DATA #
####################
rst_raw = read.csv('../Data/RST/2017-2018/LLRTP_fall_2017CK.csv')

# little bit of cleaning
rst_df = rst_raw %>%
  filter(Tag2RecaptureType != 'Efficiency Recapture') %>%
  select(SiteName, SurveyDateTime, FishObservationKey, Species, FishForkLength, FishWeight, PitTagID, PitTagCaptureType,
         ReleaseLocation, FishStatus, FishCount) %>%
  filter(FishStatus != 'Dead')

# the proportion of all fish captured from Oct 1 on that were of taggable size
p_taggable = rst_df %>%
  select(FishForkLength) %>%
  na.omit() %>%
  summarise(n_fish = n(),
            n_tag = sum(FishForkLength >= 105),
            p_tag = n_tag / n_fish)
p_taggable

# length frequency histogram for Chinook presmolts captured from 10/1 to end of season
lf_p = ggplot(rst_df, aes(x = FishForkLength)) +
  scale_x_continuous(limits = c(60, 140)) +
  geom_histogram(aes(y = ..count..), binwidth = 2, fill = 'steelblue3') +
  geom_vline(xintercept = 105, color = 'red', size = 1) +
  labs(x = 'Fork Length (mm)', y = 'Count') +
  theme_classic() +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 24))
lf_p

##########################################################################
# APPLY CJS TRANSITION PROBABILITIES TO L3A0 PRESMOLT EMIGRANT ESTIMATES #
##########################################################################
l3ao_df = data.frame(brood_year = '2016',
                     species = 'Chinook',
                     life_stage = 'Presmolt',
                     size = c('subtaggable','taggable'),
                     Est = c(28932.2, 22067.3),
                     LCI_95 = c(27111, 20344),
                     UCI_95 = c(30881, 23840),
                     SE = c(956.3, 910.4),
                     CV = c(0.033053, 0.041256))

taggable_trans_df = trans_fish_pass %>%
  select(site, Trans_Prob, Trans_Prob_SE, Cum_Trans_Prob, Cum_Trans_SE) %>%
  rowwise() %>%
  mutate(taggable_presmolts_passing = Cum_Trans_Prob * l3ao_df$Est[l3ao_df$size == 'taggable'],
         taggable_presmolts_SE = deltamethod(~ x1 * x2,
                                             mean = c(Cum_Trans_Prob, l3ao_df$Est[l3ao_df$size == 'taggable']),
                                             cov = diag(c(Cum_Trans_SE, l3ao_df$SE[l3ao_df$size == 'taggable'])^2)),
         taggable_presmolts_LCI_95 = max(0, taggable_presmolts_passing + qnorm(0.025) * taggable_presmolts_SE),
         taggable_presmolts_UCI_95 = taggable_presmolts_passing + qnorm(0.975) * taggable_presmolts_SE) %>%
  ungroup()

taggable_trans_df %>%
  select(site, matches('taggable'))

### END 5-23-2018

