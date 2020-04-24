#-----------------------------------------------
#
# A script for processing PIT tag observations
#
# Created by: Mike Ackerman & Kevin See
# Date created: 4/24/20
# Last modified:
#
#-----------------------------------------------

#-------------------------
# load necessary libraries
#-------------------------
library(tidyverse)
library(lubridate)
library(magrittr)
library(PITcleanr)

#-----------------------------------------------------
# pull out tag list of all PIT tags to feed to PTAGIS
#-----------------------------------------------------
all_tag_df = list('pilot',
                  '2018_2019',
                  '2019_2020') %>%
  rlang::set_names() %>%
  map_df(.id = 'Year',
         .f = function(x) {
           load(paste0('data/prepped/', x, '/cap_hist.rda'))
           return(cap_hist_list$tag_df)
         })
# save PIT tag codes
all_tag_df %>%
  select(pit_tag_id) %>%
  filter(pit_tag_id != "NA") %>%
  write_delim(path = "data/raw/pit_tags.txt",
              delim = '\n',
              col_names = F)

#-----------------------------------------------------
# process PTAGIS detections with PITcleanr
#-----------------------------------------------------
# build configuration table (requires internet connection)
org_config = buildConfig()


# read in detections from PTAGIS
observations = read_csv('data/raw/PITcleanr_query.csv')

janitor::tabyl(observations, `Event Site Code Value`)

site_codes = unique(observations$`Event Site Code Value`)
configuration = org_config %>%
  filter(SiteID %in% site_codes) %>%
  mutate(Node = SiteID) %>%
  mutate(Node = if_else(RKMTotal < unique(RKMTotal[SiteID == "GRJ"]),
                        'Below_GRJ',
                        Node),
         Node = if_else(RKMTotal < unique(RKMTotal[SiteID == "LLR"]) &
                          RKMTotal > unique(RKMTotal[SiteID == "GRJ"]),
                        'Below_LLR',
                        Node),
         Node = if_else(SiteID == 'LEMHIR',
                        "LEMHIR",
                        Node))

configuration %>%
  select(Node, SiteID, SiteType, RKM, RKMTotal) %>%
  unique() %>%
  arrange(RKMTotal) %>%
  as.data.frame()
