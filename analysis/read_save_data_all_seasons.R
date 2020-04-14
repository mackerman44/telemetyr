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

#-------------------------
# PILOT STUDY
#-------------------------
# path to the folder on Biomark NAS
# be sure to be connected to the Biomark VPN
# for Mike
pilot_path = "S:/telemetry/lemhi/fixed_site_downloads/2017_2018"
# for Kevin
pilot_path = "~/../../Volumes/ABS/telemetry/lemhi/fixed_site_downloads/2017_2018"

# deal with data that had previously been missing in the pilot study due to errors when resetting receiver
# timers after downloading data
# for Mike
miss_path = "S:/telemetry/lemhi/fixed_site_downloads/2017_2018_missing_A_data"
# for Kevin
miss_path = "~/../../Volumes/ABS/telemetry/lemhi/fixed_site_downloads/2017_2018_missing_A_data"

# read in the "raw" .txt format data
raw_df = read_txt_data(path = pilot_path)

# for the pilot year, we have to add this "missing" data
miss_df = read_txt_data(path = miss_path) %>%
  # add characters corresponding to the year of the file name, to make it consistent
  mutate(file_char = nchar(file)) %>%
  mutate(jday = str_sub(file, 1, 3),
         jday = as.numeric(jday),
         yr = if_else(jday < 100,
                      18,
                      17)) %>%
  mutate(file = if_else(file_char == 11,
                        paste0(yr, file),
                        file)) %>%
  select(-c(file_char:yr))

raw_df %<>%
  bind_rows(miss_df)

# fix a few receiver codes
raw_df %<>%
  mutate(receiver = recode(receiver,
                           'BR1' = 'TB1',      # recode BR1 to TB1
                           'BR2' = 'TB2',      # recode BR2 to TB2
                           '039' = 'TT1'))     # recode 039 to TT1

# clean, round and compress data
compress_df = compress_raw_data(raw_df)

#--------------------------
# save a couple objects
save_path = "data/prepped/pilot/"

write_rds(raw_df, paste0(save_path, "raw.rds"))
write_rds(compress_df, paste0(save_path, "compressed.rds"))

#--------------------------
# read in and save the csv format data
pilot_csv_df = read_csv_data(path = pilot_path) %>%
  bind_rows(read_csv_data(path = miss_path)) %>%
  arrange(receiver, tag_id, start)
# save as .rda object
save(pilot_csv_df, file = "data/raw/pilot_csv_df.rda")


# read in pilot study receiver on/off and volt/temp data from NAS
pilot_on_off_df = read_on_off_data(path = pilot_path)
pilot_volt_temp_df = read_volt_temp_data(path = pilot_path)

#-------------------------
# 2018-2019 SEASON
#-------------------------
# path to the folder on Biomark NAS; be sure to be connected to the Biomark VPN
# for Mike
ssn_1819_path = "S:/telemetry/lemhi/fixed_site_downloads/2018_2019"
# for Kevin
ssn_1819_path = "~/../../Volumes/ABS/telemetry/lemhi/fixed_site_downloads/2018_2019"

# read in and save the csv format data
ssn_1819_csv_df = read_csv_data(path = ssn_1819_path) %>%
  arrange(receiver, tag_id, start)
# save as .rda object
save(ssn_1819_csv_df, file = "data/raw/ssn_1819_csv_df.rda")

# read in the "raw" .txt format data
ssn_1819_raw = read_txt_data(path = ssn_1819_path)

# save as a .rda object
save(ssn_1819_raw, file = "data/raw/ssn_1819_raw.rda")

#-------------------------
# 2019-2020 SEASON
#-------------------------
# path to the folder on Biomark NAS; be sure to be connected to the Biomark VPN
# for Mike
ssn_1920_path = "S:/telemetry/lemhi/fixed_site_downloads/2019_2020"
# for Kevin
ssn_1920_path = "~/../../Volumes/ABS/telemetry/lemhi/fixed_site_downloads/2019_2020"

# read in and save the csv format data
ssn_1920_csv_df = read_csv_data(path = ssn_1920_path) %>%
  arrange(receiver, tag_id, start)
# save as .rda object
save(ssn_1920_csv_df, file = "data/raw/ssn_1920_csv_df.rda")

# read in the "raw" .txt format data
ssn_1920_raw = read_txt_data(path = ssn_1920_path)

# save as a .rda object
save(ssn_1920_raw, file = "data/raw/ssn_1920_raw.rda")
