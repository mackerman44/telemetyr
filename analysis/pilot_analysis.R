#-----------------------------------------------
#
# A script for the Lemhi River radio telemetry pilot study.
# Includes cleaning, reduction, analysis, and visualization of data
# and roughly based on the original pilotStudy.R script used to generate the
# original report
#
# Created by: Mike Ackerman & Kevin See on 4/8/2020
# Date created: 4/8/2020
# Last modified:
#
#-----------------------------------------------

#-------------------------
# load necessary libraries
#-------------------------
library(tidyverse)
library(magrittr)
library(telemetyr)

#-------------------------
# load compressed data
#-------------------------
load("data/prepped/pilot/compressed.rda")

#-------------------------
# receiver operation times
#-------------------------
# summarise pilot study timer data - all receiver codes
timer_summ = summarise_timer_data(compress_df)

# a list of the receiver names
receiver_nms = c('LH1','LH2',
                 'DC1','DC2',
                 'MB1','MB2',
                 'TR1','TR2')

# summarise pilot study timer data - only receiver codes in receiver_nms
timer_summ = summarise_timer_data(compress_df,
                                  receiver_codes = receiver_nms)

# summarise pilot study timer data - only receiver codes in receiver_nms and use season start and end
timer_summ = summarise_timer_data(compress_df,
                                  receiver_codes = receiver_nms,
                                  season_start = "2017-09-12",
                                  season_end = "2018-02-15")

# objects from timer_summ
head(timer_summ$operations_summ)
timer_summ$operations_plot
timer_summ$p_operational

#-------------------------
# load pilot_raw.rda
load("data/raw/pilot_raw.rda")

# clean raw data a little bit
pilot_clean = clean_raw_data(pilot_raw)

# fix tag codes
pilot_round = round_tag_codes(pilot_clean,
                              round_to = 5)

# summarise data to make it more like csv output
pilot_summ = summarise_txt_data(pilot_round)
#save(pilot_summ, file = "data/prepped/pilot_summ.rda")

#-----------------------------------------
# following instructions from Nick
#-----------------------------------------
# get tag ids for tags that were released
library(readxl)
tag_list = read_excel('data/raw/tag_release/TagReleases2017.xlsx',
                    'RTs') %>%
  mutate(tag_id = str_extract(RadioTag, "[:digit:]*")) %>%
  pull(tag_id)

# can parse out tag data in a couple ways
tag_df_v1 = parse_tag_list(pilot_summ,
                           tags = tag_list)

tag_df_v2 = parse_tag_list(pilot_round,
                           tags = tag_list) %>%
  summarise_txt_data()

identical(tag_df_v1,
          tag_df_v2)

# Nick says he'd like to make that max_min 2 min, so we can do that
tag_df = parse_tag_list(pilot_round,
                        tags = tag_list) %>%
  summarise_txt_data(max_min = 2)





#-------------------------
# noise data
#-------------------------
# parse out noise data
noise_df = parse_noise(pilot_summ)

# summarise_noise_data - all receivers in noise_data, raw noise observations
noise_summ = summarise_noise_data(noise_data = noise_df,
                                  receiver_codes = NULL,
                                  operations_summary = NULL)

# summarise_noise_data - only receivers in receiver_nm, raw noise observations
noise_summ = summarise_noise_data(noise_data = noise_df,
                                  receiver_codes = receiver_nms,
                                  operations_summary = NULL)

# summarise_noise_data - only receivers in receiver_nm, noise converted to a rate (noise per hour)
noise_summ = summarise_noise_data(noise_data = noise_df,
                                  receiver_codes = receiver_nms,
                                  operations_summary = timer_summ$operations_summ)
