#-----------------------------------------------
#
# A script for reading in and saving observation data,
# both .txt and .csv format, from the Tracker software
# for the fixed site antennas and receiver and for the
# Lemhi River juvenile Chinook salmon winter radio
# telemetry study.
#
# Created by: Mike Ackerman & Kevin See
# Date created:
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
library(janitor)


#-------------------------
# point to NAS based on operating system
if(.Platform$OS.type != 'unix') {
  nas_prefix = "S:"
}
if(.Platform$OS.type == 'unix') {
  nas_prefix = "~/../../Volumes/ABS"
}

#-------------------------
# Where should data be saved?
# save it to a raw folder under Nick's name on the NAS, to be QA/QC'd
save_path = paste0(nas_prefix, "/Nick/telemetry/raw/")

#-------------------------
# Metadata about receivers and tags
#-------------------------
# used for constructing capture histories
# read in some metadata associated with the receivers
rec_meta = read_excel('data/prepped/site_metadata/rt_site_metadata.xlsx')

# which receivers were used each year?
rec_site_list = rec_meta %>%
  gather(season, use, starts_with("use")) %>%
  mutate(season = str_remove(season, "use")) %>%
  filter(use) %>%
  select(-use) %>%
  split(list(.$season)) %>%
  map(.f = function(x) {
    x %>%
      filter(site_type == 'rt_fixed') %>%
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
  })

# metadata about each tag, split by year
# get data about each released tag, including code
tag_df_list = read_excel(paste0(nas_prefix, '/data/telemetry/lemhi/tag_release/lemhi_winter_telemetry_tag_info.xlsx')) %>%
  mutate(tag_id = str_extract(radio_tag_id, "[:digit:]*"),
         tag_id = as.numeric(tag_id)) %>%
  mutate_at(vars(activation_time, release_time),
            list(as.numeric)) %>%
  mutate_at(vars(activation_time, release_time),
            list(excel_numeric_to_date),
            include_time = T) %>%
  split(list(.$season))

#-------------------------
# PILOT STUDY
#-------------------------
# path to the folder on Biomark NAS
# be sure to be connected to the Biomark VPN
pilot_path = paste0(nas_prefix, "/data/telemetry/lemhi/fixed_site_downloads/2017_2018")

# deal with data that had previously been missing in the pilot study due to errors when resetting receiver
# timers after downloading data
miss_path = paste0(nas_prefix, "/data/telemetry/lemhi/fixed_site_downloads/2017_2018_missing_A_data")

# read in the "raw" .txt format data
raw_df = read_txt_data(path = pilot_path)

# for the pilot year, we have to add this "missing" data
miss_df = read_txt_data(path = miss_path)

# note that both raw_df and miss_df have some dates with date == "00/00/00"
raw_df %<>%
  mutate(source = 'reg') %>%
  select(source, everything()) %>%
  bind_rows(miss_df %>%
              mutate(source = 'miss'))

# fix a few receiver codes
raw_df %<>%
  mutate(receiver = recode(receiver,
                           'BR1' = 'TB1',      # recode BR1 to TB1
                           'BR2' = 'TB2',      # recode BR2 to TB2
                           '039' = 'TT1'))     # recode 039 to TT1

# clean, round and compress data
compress_df = compress_raw_data(raw_df,
                                min_yr = 2017,
                                max_yr = 2018,
                                round_to = 5)


#------------------------------------
# prep some fish capture history data
yr_label = "17_18"

# list with wide, long capture histories and tag info
cap_hist_list = prep_capture_history(compress_df,
                                     tag_data = tag_df_list[[yr_label]],
                                     n_obs_valid = 3,
                                     rec_site = rec_site_list[[yr_label]],
                                     delete_upstream = T,
                                     location = 'site',
                                     output_format = 'all')

#--------------------------
# save a couple objects
save(raw_df,
     file = paste0(save_path, "raw_", yr_label, ".rda"))
save(compress_df,
     file = paste0(save_path, "compressed_", yr_label, ".rda"))
save(cap_hist_list,
     file = paste0(save_path, "cap_hist_", yr_label, ".rda"))

# #--------------------------
# # read in and save the csv format data
# pilot_csv_df = read_csv_data(path = pilot_path) %>%
#   bind_rows(read_csv_data(path = miss_path)) %>%
#   arrange(receiver, tag_id, start)
# # save as .rda object
# save(pilot_csv_df, file = "data/raw/pilot_csv_df.rda")


# # read in pilot study receiver on/off and volt/temp data from NAS
# pilot_on_off_df = read_on_off_data(path = pilot_path)
# pilot_volt_temp_df = read_volt_temp_data(path = pilot_path)

#-------------------------
# 2018-2019 SEASON
#-------------------------
# path to the folder on Biomark NAS; be sure to be connected to the Biomark VPN
ssn_1819_path = paste0(nas_prefix, "/data/telemetry/lemhi/fixed_site_downloads/2018_2019")

# # read in and save the csv format data
# ssn_1819_csv_df = read_csv_data(path = ssn_1819_path) %>%
#   arrange(receiver, tag_id, start)
# # save as .rda object
# save(ssn_1819_csv_df, file = "data/raw/ssn_1819_csv_df.rda")

# read in the "raw" .txt format data
raw_df = read_txt_data(path = ssn_1819_path)

# clean, round and compress data
compress_df = compress_raw_data(raw_df,
                                min_yr = 2018,
                                max_yr = 2019,
                                round_to = 5)

#------------------------------------
# prep some fish capture history data
yr_label = "18_19"

# look for detections of batch 2 and 3 tags within 24 hrs of activation, and delete those (these tags run for 24 hrs, then shut off)
ch_compress = compress_df %>%
  anti_join(tag_df_list[[yr_label]] %>%
              filter(tag_purpose == 'fish',
                     duty_cycle != 'batch_1') %>%
              select(tag_id, release_time) %>%
              inner_join(compress_df) %>%
              filter(end <= release_time + lubridate::hours(1)))

# list with wide, long capture histories and tag info
cap_hist_list = prep_capture_history(ch_compress,
                                     tag_data = tag_df_list[[yr_label]],
                                     n_obs_valid = 3,
                                     rec_site = rec_site_list[[yr_label]],
                                     delete_upstream = T,
                                     location = 'site',
                                     output_format = 'all')

#--------------------------
# save a couple objects
save(raw_df,
     file = paste0(save_path, "raw_", yr_label, ".rda"))
save(compress_df,
     file = paste0(save_path, "compressed_", yr_label, ".rda"))
save(cap_hist_list,
     file = paste0(save_path, "cap_hist_", yr_label, ".rda"))

#-------------------------
# 2019-2020 SEASON
#-------------------------
# path to the folder on Biomark NAS; be sure to be connected to the Biomark VPN
ssn_1920_path = paste0(nas_prefix, "/data/telemetry/lemhi/fixed_site_downloads/2019_2020")

# # read in and save the csv format data
# ssn_1920_csv_df = read_csv_data(path = ssn_1920_path) %>%
#   arrange(receiver, tag_id, start)
# # save as .rda object
# save(ssn_1920_csv_df, file = "data/raw/ssn_1920_csv_df.rda")

# read in the "raw" .txt format data
raw_df = read_txt_data(path = ssn_1920_path)

# clean, round and compress data
compress_df = compress_raw_data(raw_df,
                                min_yr = 2019,
                                max_yr = 2020,
                                round_to = 10)

# 370,451 rows have NAs for start and end. All from receivers TT1 and TT2

#------------------------------------
# prep some fish capture history data
yr_label = "19_20"

# look for detections of batch 2 and 3 tags within 24 hrs of activation, and delete those (these tags run for 24 hrs, then shut off)
ch_compress = compress_df %>%
  anti_join(tag_df_list[[yr_label]] %>%
              filter(tag_purpose == 'fish',
                     duty_cycle != 'batch_1') %>%
              select(tag_id, release_time) %>%
              inner_join(compress_df) %>%
              filter(end <= release_time + lubridate::hours(1)))


# list with wide, long capture histories and tag info
cap_hist_list = prep_capture_history(ch_compress,
                                     tag_data = tag_df_list[[yr_label]],
                                     n_obs_valid = 3,
                                     rec_site = rec_site_list[[yr_label]],
                                     delete_upstream = T,
                                     location = 'site',
                                     output_format = 'all')


#--------------------------
# save a couple objects
save(raw_df,
     file = paste0(save_path, "raw_", yr_label, ".rda"))
save(compress_df,
     file = paste0(save_path, "compressed_", yr_label, ".rda"))
save(cap_hist_list,
     file = paste0(save_path, "cap_hist_", yr_label, ".rda"))
