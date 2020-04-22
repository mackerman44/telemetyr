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
library(readxl)

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
# noise data
#-------------------------
# summarise_noise_data - all receivers in noise_data, raw noise observations
noise_summ = summarise_noise_data(compress_df,
                                  receiver_codes = NULL,
                                  operations_summary = NULL)

# summarise_noise_data - only receivers in receiver_nm, raw noise observations
noise_summ = summarise_noise_data(compress_df,
                                  receiver_codes = receiver_nms,
                                  operations_summary = NULL)

# summarise_noise_data - only receivers in receiver_nm, noise converted to a rate (noise per hour)
noise_summ = summarise_noise_data(compress_df,
                                  receiver_codes = receiver_nms,
                                  operations_summary = timer_summ$operations_summ)

#-------------------------
# test tags
#-------------------------
test_summ = summarise_test_data(compress_df,
                                tag_data_path = 'data/prepped/tag_release/lemhi_winter_telemetry_tag_info.xlsx')

#-------------------------
# volt & temp info
#-------------------------
vt_df = read_volt_temp_data(path)
