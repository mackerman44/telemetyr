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
#-------------------------

# create df of file names
pilot_path = "S:/telemetry/lemhi/fixed_site_downloads/2017_2018"
file_df = get.file.nms(path = pilot_path)

# read in csv format data
pilot_csv_df = read.csv.data(path = pilot_path)
save(pilot_csv_df, file = "data/raw/pilot_csv_df.rda")

# this function reads in the "raw" text files
pilot_txt_df = read.txt.data(path = pilot_path)
save(pilot_txt_df, file = "data/raw/pilot_txt_df.rda")

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


