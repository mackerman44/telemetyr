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
path = "S:/telemetry/lemhi/fixed_site_downloads/2017_2018/"

# list the folders each with a receiver name
receiver_nms = list.files(path)

# keep only those folders with 3 characters i.e. get rid of any hidden or misc folders we don't want to read
receiver_nms = site_nms[nchar(site_nms) == 3]

if(length(receiver_nms) == 0) stop("No folders were found in path")
folders = as.list(receiver_nms)
names(folders) = receiver_nms

library(dplyr)
library(purrr)
file_df = folders %>%
  map(.f = function(x) {
    list.files(paste(path, x[1], sep = "/"))
  }) %>%
  stack() %>%
  select(receiver = ind,
         nm = values) %>%
  tbl_df() %>%
  mutate(file_name = paste(receiver, nm, sep = "/"))

getFileNms = function(path = '.') {

  return(file_df)
}

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


