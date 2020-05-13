#-----------------------------------------------
#
# A script for examining habitat availability data
#
# Created by: Mike Ackerman & Kevin See
# Date created: 5/13/20
# Last modified:
#
#-----------------------------------------------

#-------------------------
# load necessary libraries
#-------------------------
library(tidyverse)
library(telemetyr)
library(janitor)
library(sf)

theme_set(theme_bw())

#-------------------------
# read in habitat availability data
#-------------------------
if(.Platform$OS.type != 'unix') {
  hab_data_path = "S:/data/habitat/lemhi_telemetry/availability/raw/transects/"
}
if(.Platform$OS.type == 'unix') {
  hab_data_path = "~/../../Volumes/ABS/data/habitat/lemhi_telemetry/availability/raw/transects/"
}

my_crs = 32612
xs_sf = st_read(paste0(hab_data_path, "XS_Sin_Low.shp")) %>%
  st_transform(crs = my_crs) %>%
  rbind(st_read(paste0(hab_data_path, "XS_Sin_Med.shp")) %>%
          st_transform(crs = my_crs)) %>%
  rbind(st_read(paste0(hab_data_path, "XS_Sin_High.shp")) %>%
          st_transform(crs = my_crs)) %>%
  st_zm()


center_pts = read_csv(paste0(hab_data_path, "Habitat_Avail_1.csv")) %>%
  clean_names(case = 'snake') %>%
  select(-parent_global_id,
         -object_id,
         -creation_date,
         -creator,
         -edit_date,
         -editor) %>%
  rename(parent_global_id = global_id)

xs_pts = read_csv(paste0(hab_data_path, "Point_Number_2.csv")) %>%
  clean_names(case = 'snake') %>%
  select(-creation_date,
         -creator,
         -edit_date,
         -editor)

sc_pts = read_csv(paste0(hab_data_path, "Side_Channel_Info_3.csv")) %>%
  clean_names(case = 'snake') %>%
  select(-creation_date,
         -creator,
         -edit_date,
         -editor)

test = read_csv(paste0(hab_data_path, "surveyPoint_0.csv")) %>%
  clean_names(case = 'snake') %>%
  select(-creation_date,
         -creator,
         -edit_date,
         -editor)

# join center points and associated transect points
xs_avail = center_pts %>%
  full_join(xs_pts %>%
              mutate(point_source = 'MainChannel')) %>%
  bind_rows(sc_pts %>%
              mutate(point_source = 'SideChannel',
                     channel_unit_type = 'SC') %>%
              inner_join(center_pts)) %>%
  mutate_at(vars(channel_unit_type, bank_type_condition_closest, dominant_cover_type_1_5m_radius, dominant_substrate_1mx1m, substrate_concealment, point_source),
            list(as.factor))


#-------------------------
# Examine xs_avail for QA/QC issues
#-------------------------

# these points are not in the Habitat_Avail_1 file
xs_avail %>%
  anti_join(center_pts)
# these points are not in the Point_Number_2 file
xs_avail %>%
  filter(point_source == 'MainChannel') %>%
  anti_join(xs_pts)

summary(xs_avail)
xs_avail %>%
  filter(is.na(cross_section_number)) %>%
  as.data.frame()
xs_avail %>%
  filter(is.na(object_id))
xs_avail %>%
  filter(!is.na(cross_section_number)) %>%
  filter(is.na(xs_point_number)) %>%
  tabyl(point_source)

xs_avail %>%
  filter(!is.na(cross_section_number)) %>%
  filter(is.na(channel_unit_type)) %>%
  summary()
  # tabyl(point_source)

xs_avail %>%
  filter(!is.na(cross_section_number)) %>%
  filter(is.na(dominant_cover_type_1_5m_radius)) %>%
  # summary()
  tabyl(point_source)


tabyl(xs_avail,
      channel_unit_type)

xs_avail %>%
  filter(!is.na(other_bank_type)) %>%
  tabyl(xs_point_number)

xs_avail %>%
  filter(!is.na(cross_section_number)) %>%
  # filter(is.na(distance_to_cover_round_to_nearest_0_1m)) %>%
  filter(distance_to_cover_round_to_nearest_0_1m == 0) %>%
  tabyl(dominant_cover_type_1_5m_radius)


xs_avail %>%
  filter(!is.na(cross_section_number)) %>%
  filter(is.na(dominant_substrate_1mx1m)) %>%
  # as.data.frame()
  tabyl(point_source)

xs_avail %>%
  filter(!is.na(cross_section_number)) %>%
  filter(dominant_cover_type_1_5m_radius == 'NoCover') %>%
  tabyl(distance_to_cover_round_to_nearest_0_1m)

xs_avail %>%
  filter(!is.na(cross_section_number)) %>%
  filter(is.na(substrate_concealment)) %>%
  as.data.frame()

xs_avail %>%
  filter(!is.na(cross_section_number)) %>%
  summary()


xs_avail %>%
  filter(!is.na(cross_section_number)) %>%
  select(cross_section_number, channel_unit_type, point_source) %>%
  distinct() %>%
  tabyl(channel_unit_type) %>%
  adorn_totals()

xs_avail %>%
  filter(!is.na(cross_section_number)) %>%
