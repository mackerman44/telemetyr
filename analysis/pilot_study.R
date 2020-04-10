#-----------------------------------------------
# A script for the Lemhi River radio telemetry pilot study.
# Includes reading, cleaning, reduction, analysis, and visualization of data
# and roughly based on the original pilotStudy.R script used to generate the
# original report
#
# Created by Mike Ackerman on 4/8/2020
#-----------------------------------------------

#-------------------------
# load necessary libraries
#-------------------------

#-------------------------
# read in pilot study data from NAS
# be sure to be connected to Biomark VPN
#-------------------------
# create df of file names
# for Mike
pilot_path = "S:/telemetry/lemhi/fixed_site_downloads/2017_2018"
# for Kevin
pilot_path = "~/../../Volumes/ABS/telemetry/lemhi/fixed_site_downloads/2017_2018"
pilot_path_missing = "~/../../Volumes/ABS/telemetry/lemhi/fixed_site_downloads/2017_2018_missing_A_data"

# file_df = get.file.nms(path = pilot_path)

# read in csv format data
pilot_csv_df = read.csv.data(path = pilot_path)
save(pilot_csv_df, file = "data/raw/pilot_csv_df.rda")

# this function reads in the "raw" text files
pilot_txt_df = read.txt.data(path = pilot_path)
pilot_raw = read.txt.data(path = pilot_path)
# clean raw data a little bit
pilot_clean = clean.raw.data(pilot_raw)
# fix tag codes
pilot_round = round.tag.codes(pilot_clean,
                              round_to = 5)



save(pilot_txt_df, file = "data/raw/pilot_txt_df.rda")

#-------------------------
# deal with data that had previously been missing in the pilot study due to errors when resetting receiver
# timers after downloading data
#-------------------------
miss_path = "S:/telemetry/lemhi/fixed_site_downloads/2017_2018_missing_A_data"

#-------------------------
# read in 2018-2019 season data from NAS
#-------------------------
# NOTE: This should probably be moved to a script for that season at a later time. I just wanted to read in that data
# and save it to the repo so that it was available for testing.

# create df of file names
ssn_1819_path = "S:/telemetry/lemhi/fixed_site_downloads/2018_2019"
file_df = get.file.nms(path = ssn_1819_path)

# read in csv format data
ssn_1819_csv_df = read.csv.data(path = ssn_1819_path)
save(ssn_1819_csv_df, file = "data/raw/ssn_1819_csv_df.rda")

# this function reads in the "raw" text files
ssn_1819_txt_df = read.txt.data(path = ssn_1819_path)
save(ssn_1819_txt_df, file = "data/raw/ssn_1819_txt_df.rda")

#-------------------------
# read in pilot study on/off and volt/temp data from NAS
#-------------------------
pilot_on_off_df = read.on.off.data(path = pilot_path)
pilot_volt_temp_df = read.volt.temp.data(path = pilot_path)
