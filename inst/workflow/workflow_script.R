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
# CAPTURE HISTORIES AND CJS
#------------------------

