#-----------------------------------------------
#
# A script for examining habitat availability data
#
# Created by: Mike Ackerman & Kevin See
# Date created: 5/13/20
# Last modified:
#
#-----------------------------------------------

# TO DO
# calculate length of each sinuosity class by reading in XS_JenksBreaks_SinClass and tallying up number of transects in each sinuosity class. Transects were taken 1 m apart, so number of transects = meters of stream
# take XS_Low/Med/High_Sample transects, join into one file, pull out intersection of each transect with LemhiCenterline_prj
# turn 0 points of sampled transects into shapefile, join with closest point from file above, to identify which sinuosity class each sampled transect came from

#-------------------------
# load necessary libraries
#-------------------------
library(tidyverse)
library(sf)
library(janitor)
#library(telemetyr)

theme_set(theme_bw())

#-------------------------
# set NAS prefix, depending on operating system
#-------------------------
if(.Platform$OS.type != 'unix') {
  nas_prefix = "S:"
}
if(.Platform$OS.type == 'unix') {
  nas_prefix = "~/../../Volumes/ABS"
}

#-------------------------
# read in centerline x xs intersections
#-------------------------
xs_pts = st_read(paste(nas_prefix,
                          "data/habitat/lemhi_telemetry/availability/raw/Centerline_XS_Intersects.shp",
                          sep = "/")) %>%
  st_zm()
my_crs = st_crs(xs_pts)

#-------------------------
# read in habitat availability data
#-------------------------
hab_avail_path = paste(nas_prefix, "data/habitat/lemhi_telemetry/availability/prepped/", sep = '/')
avail_pts = read_csv(paste0(hab_avail_path, "Habitat_Avail_1.csv")) %>%
  clean_names(case = "snake") %>%
  select(global_id, adjacent_side_channel, x, y)

# convert to sf object
avail_pts_sf = avail_pts %>%
  st_as_sf(coords = c("x", "y"),
           crs = "+proj=longlat +datum=WGS84") %>%
  st_transform(my_crs)

# plot them to make sure we're good
xs_avail_p = xs_pts %>%
  ggplot() +
  geom_sf(aes(color = Category,
              fill = Category),
          size = 0.25) +
  geom_sf(data = avail_pts_sf,
          size = 0.25) +
  labs(fill = "Sinuosity\nCategory",
       color = "Sinuosity\nCategory")
xs_avail_p

# for each availability pt, what is nearest xs?
xs_2_avail = avail_pts_sf %>%
  st_join(xs_pts,
          join = st_nearest_feature,
          left = T) %>%
  st_drop_geometry()

# left_join the availability global_ids to the xs_pts
avail_2_xs = xs_pts %>%
  left_join(xs_2_avail %>%
              select(Name, avail_global_id = global_id))
# make sure there's still 204
sum(!is.na(avail_2_xs$avail_global_id))

#-------------------------
# read in habitat use data
#-------------------------
hab_use_path = paste(nas_prefix, "data/habitat/lemhi_telemetry/use/", sep = '/')
use_pts = read_csv(paste0(hab_use_path, "Fish_Info_Master_NPedits.csv")) %>%
  clean_names(case = "snake")

# convert to sf object
use_pts_sf = use_pts %>%
  st_as_sf(coords = c("long", "lat"),
           crs = "+proj=longlat +datum=WGS84") %>%
  st_transform(my_crs)

# plot again for verification
xs_avail_p = xs_pts %>%
  ggplot() +
  geom_sf(aes(color = Category,
              fill = Category),
          size = 0.25) +
  geom_sf(data = avail_pts_sf,
          size = 0.25) +
  geom_sf(data = use_pts_sf,
          fill = "white",
          color = "white",
          size = 0.25) +
  labs(fill = "Sinuosity\nCategory",
       color = "Sinuosity\nCategory")
xs_avail_p

# for each use pt, what is nearest xs?
xs_2_use = use_pts_sf %>%
  st_join(xs_pts,
          join = st_nearest_feature,
          left = T) %>%
  st_drop_geometry()

use_avail_2_xs = avail_2_xs %>%
  left_join(xs_2_use %>%
              select(Name, use_global_id = global_id))
sum(!is.na(use_avail_2_xs$avail_global_id)) # 204
sum(!is.na(use_avail_2_xs$use_global_id))   # 212
# I lost one record!? Need to sleuth

# write results
st_write(use_avail_2_xs,
         paste(nas_prefix,
               "data/habitat/lemhi_telemetry/prepped/use_avail_2_xs.shp",
               sep = "/"))

paste(nas_prefix, "data/habitat/lemhi_telemetry/prepped/use_avail_2_xs.shp", sep = '/')

#-------------------------
# read in all possible transects
#-------------------------
xs_all = st_read(paste(nas_prefix, "data/habitat/lemhi_telemetry/availability/raw/XS_JenksBreaks_SinClass.shp", sep = "/")) %>%
  st_drop_geometry() %>%
  as_tibble()

sin_cls_tot = xs_all %>%
  group_by(Category) %>%
  summarise(n_xs = n())

#-------------------------
# read in center points of all possible sample transects
#-------------------------
xs_sample = st_read(paste(nas_prefix, "data/habitat/lemhi_telemetry/availability/cross_sections/XS_High_center_points.shp", sep = "/")) %>%
  rbind(st_read(paste(nas_prefix, "data/habitat/lemhi_telemetry/availability/cross_sections/XS_Med_center_points.shp", sep = "/"))) %>%
  rbind(st_read(paste(nas_prefix, "data/habitat/lemhi_telemetry/availability/cross_sections/XS_Low_center_points.shp", sep = "/")))

xs_sample %>%
  select(Name:Category) %>%
  select(Category) %>%
  ggplot(aes(color = Category)) +
  geom_sf()

#-------------------------
# read in habitat availability data
#-------------------------
hab_data_raw = paste(nas_prefix, "data/habitat/lemhi_telemetry/availability/raw/transects/", sep = '/')
hab_data_path = paste(nas_prefix, "data/habitat/lemhi_telemetry/availability/prepped/", sep = '/')


my_crs = 32612
xs_sf = st_read(paste0(hab_data_raw, "XS_Sin_Low.shp")) %>%
  st_transform(crs = my_crs) %>%
  rbind(st_read(paste0(hab_data_raw, "XS_Sin_Med.shp")) %>%
          st_transform(crs = my_crs)) %>%
  rbind(st_read(paste0(hab_data_raw, "XS_Sin_High.shp")) %>%
          st_transform(crs = my_crs)) %>%
  st_zm()

xs_sf %>%
  st_drop_geometry() %>%
  as_tibble() %>%
  group_by(Category) %>%
  summarise(n_xs = n())

center_pts = read_csv(paste0(hab_data_path, "Habitat_Avail_1.csv")) %>%
  clean_names(case = 'snake') %>%
  select(parent_global_id = global_id,
         cross_section_number, adjacent_side_channel, x, y)

# add sinuosity class by joining to xs_sample
center_pts %>%
  st_join(xs_sample %>%
            st_transform(st_crs(center_pts)) %>%
            select(Name, Sinuosity, Category))



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
  mutate_at(vars(channel_unit_type,
                 bank_type_condition_closest,
                 dominant_cover_type_1_5m_radius,
                 dominant_substrate_1mx1m,
                 substrate_concealment,
                 point_source),
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
  filter(is.na(distance_to_cover_round_to_nearest_0_1m)) %>%
  # filter(distance_to_cover_round_to_nearest_0_1m == 0) %>%
  tabyl(dominant_cover_type_1_5m_radius)

xs_avail %>%
  filter(dominant_cover_type_1_5m_radius == 'NoCover') %>%
  xtabs(~ is.na(distance_to_cover_round_to_nearest_0_1m), .)

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
