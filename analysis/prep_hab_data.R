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
library(lubridate)
library(raster)

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

##########################
# AVAILABILITY DATA PREP #
##########################
#-------------------------
# read in centerline x xs intersections
#-------------------------
xs_center_pts = st_read(paste(nas_prefix,
                          "data/habitat/lemhi_telemetry/availability/raw/Centerline_XS_Intersects.shp",
                          sep = "/")) %>%
  st_zm() %>%
  mutate(Category = fct_relevel(Category, "High", after = Inf))
my_crs = st_crs(xs_center_pts)

# quick summary by sinuosity Category
xs_center_pts %>%
  st_drop_geometry() %>%
  as_tibble() %>%
  group_by(Category) %>%
  summarise(n_pts = n(),
            min_sin = min(Sinuosity),
            max_sin = max(Sinuosity)) %>%
  ungroup() %>%
  mutate(perc = n_pts / sum(n_pts))

#-------------------------
# read in habitat availability data that have been QA/QC'd
#-------------------------
hab_avail_path = paste(nas_prefix, "data/habitat/lemhi_telemetry/availability/", sep = '/')

# "0" points on transects, meant to match the xs_center_pts
avail_pts_org = read_csv(paste0(hab_avail_path, "prepped/Habitat_Avail_1.csv")) %>%
  clean_names(case = "snake") %>%
  mutate_at(vars(creation_date, edit_date),
            list(mdy_hm))

# some duplicated cross sections. Filter these out
# Based on conversations with Richie, we decided to keep the first "creation_date".
# There were two more duplicates, with slightly different x/y coordinates. I just grabbed the first
avail_pts = avail_pts_org %>%
  group_by(cross_section_number) %>%
  filter(creation_date == min(creation_date)) %>%
  slice(1) %>%
  ungroup() %>%
  select(parent_global_id = global_id,
         cross_section_number,
         adjacent_side_channel, x, y)

# convert to sf object
avail_pts_sf = avail_pts %>%
  st_as_sf(coords = c("x", "y"),
           crs = "+proj=longlat +datum=WGS84") %>%
  st_transform(my_crs)

# plot the availability transect points along with all cross-section points
xs_avail_p = xs_center_pts %>%
  ggplot() +
  geom_sf(aes(color = Category,
              fill = Category),
          size = 0.25) +
  geom_sf(data = avail_pts_sf,
          size = 0.25) +
  labs(fill = "Sinuosity\nCategory",
       color = "Sinuosity\nCategory",
       title = "Availability Transect Points")
xs_avail_p

# for each availability pt, what is nearest xs?
xs_2_avail = avail_pts_sf %>%
  st_join(xs_center_pts,
          join = st_nearest_feature,
          left = T) %>%
  st_drop_geometry()

# left_join the availability global_ids to the xs_center_pts
avail_2_xs = xs_center_pts %>%
  left_join(xs_2_avail %>%
              select(Name, avail_global_id = parent_global_id))
# make sure there's still 179
sum(!is.na(avail_2_xs$avail_global_id))

# how many transects in each sinuosity category?
xs_2_avail %>%
  group_by(Category) %>%
  summarise(n_xs = n_distinct(parent_global_id))

#-------------------------
# read in habitat availability data
#-------------------------
xs_pts = read_csv(paste0(hab_avail_path, "prepped/Point_Number_2.csv")) %>%
  clean_names(case = 'snake') %>%
  select(-object_id,
         -creation_date,
         -creator,
         -edit_date,
         -editor)

sc_pts = read_csv(paste0(hab_avail_path, "prepped/Side_Channel_Info_3.csv")) %>%
  clean_names(case = 'snake') %>%
  select(-object_id,
         -creation_date,
         -creator,
         -edit_date,
         -editor)

# test = read_csv(paste0(hab_avail_path, "surveyPoint_0.csv")) %>%
#   clean_names(case = 'snake') %>%
#   select(-creation_date,
#          -creator,
#          -edit_date,
#          -editor)

# join center points and associated transect points
xs_avail = xs_2_avail %>%
  left_join(xs_pts %>%
              mutate(point_source = 'MainChannel')) %>%
  bind_rows(sc_pts %>%
              mutate(point_source = 'SideChannel',
                     channel_unit_type = 'SC') %>%
              inner_join(xs_2_avail)) %>%
  mutate(bank_type_condition_closest = recode(bank_type_condition_closest,
                                             "riprap" = "Riprap",
                                             "Bar_Island" = "Bar/Island"),
         dominant_cover_type_1_5m_radius = recode(dominant_cover_type_1_5m_radius,
                                                  'other' = "Other",
                                                  'Aqu_Veg_I' = "Aquatic Veg.",
                                                  "Terr_Veg" = 'Terrestrial Veg.',
                                                  'UC_bank' = 'Undercut Bank',
                                                  'LargeWood' = 'Large Wood',
                                                  'SmallWood' = 'Small Wood',
                                                  'NoCover' = 'No Cover'),
         channel_unit_type = recode(channel_unit_type,
                                    "Rapid_Plus" = "Rapid+")) %>%
  mutate_at(vars(channel_unit_type,
                 bank_type_condition_closest,
                 dominant_cover_type_1_5m_radius,
                 dominant_substrate_1mx1m,
                 substrate_concealment,
                 point_source),
            list(as.factor)) %>%
  mutate(dominant_cover_type_1_5m_radius = fct_relevel(dominant_cover_type_1_5m_radius,
                                                       "Large Wood",
                                                       after = 5),
         dominant_cover_type_1_5m_radius = fct_relevel(dominant_cover_type_1_5m_radius,
                                                       "Other", "No Cover",
                                                       after = Inf),
         channel_unit_type = factor(channel_unit_type,
                                    levels = c('Pool', 'Riffle', 'Run', 'Rapid+', 'SC', 'OCA')))

xs_avail %>%
  group_by(Category) %>%
  summarise(n_xs = n_distinct(parent_global_id))

# save for later
save(xs_avail,
     avail_pts_sf,
     file = paste(nas_prefix,
                  "data/habitat/lemhi_telemetry/availability/prepped/habitat_available.rda",
                  sep = "/"))

avail_pts_sf %>%
  left_join(xs_avail)

#-------------------------
# Examine xs_avail for QA/QC issues
#-------------------------
summary(xs_avail)

#-------------------------
# Make a few plots
#-------------------------
xs_avail %>%
  group_by(Category, channel_unit_type) %>%
  summarise(n_pts = n()) %>%
  group_by(Category) %>%
  mutate(perc = n_pts / sum(n_pts)) %>%
  select(-n_pts) %>%
  pivot_wider(names_from = 'Category',
              values_from = 'perc')

xs_avail %>%
  ggplot(aes(x = Category,
             fill = channel_unit_type)) +
  geom_bar(position = position_fill()) +
  scale_fill_brewer(palette = 'Set1',
                    name = 'CU Type') +
  labs(x = 'Sinuousity Class',
       y = 'Percentage')


xs_avail %>%
  ggplot(aes(x = Category,
             fill = substrate_concealment)) +
  geom_bar(position = position_fill()) +
  scale_fill_brewer(palette = 'Set1',
                    name = 'Concealment') +
  labs(x = 'Sinuousity Class',
       y = 'Percentage')

xs_avail %>%
  ggplot(aes(x = Category,
             fill = dominant_cover_type_1_5m_radius)) +
  geom_bar(position = position_fill()) +
  scale_fill_brewer(palette = 'Set1',
                    name = 'Cover') +
  labs(x = 'Sinuousity Class',
       y = 'Percentage')

xs_avail %>%
  ggplot(aes(x = Category,
             fill = dominant_substrate_1mx1m)) +
  geom_bar(position = position_fill()) +
  scale_fill_brewer(palette = 'Set1',
                    name = 'Substrate') +
  labs(x = 'Sinuousity Class',
       y = 'Percentage')


xs_avail %>%
  select(parent_global_id:Category,
          bank_type_condition_closest) %>%
  distinct() %>%
  ggplot(aes(x = Category,
             fill = bank_type_condition_closest)) +
  geom_bar(position = position_fill()) +
  scale_fill_brewer(palette = 'Set1',
                    name = 'Bank Cover') +
  labs(x = 'Sinuousity Class',
       y = 'Percentage')

#-------------------------
# Extract depth and velocity values from 2d numerical model results
# by sinuosity category
#-------------------------
# depth & velocity rasters from 2d numerical model, lemhi winter scenario
d_raster <- raster(paste0(nas_prefix, "data/habitat/HSI/MRA_d_v_tifs/lemhi/d_jan_v2.tif"))
v_raster <- raster(paste0(nas_prefix, "data/habitat/HSI/MRA_d_v_tifs/lemhi/v_jan_v2.tif"))

# read in the polygons that designate the Low/Med/High sinuosity reaches
sin_polygons = st_read(paste0(hab_avail_path, "raw/Sin_polys.shp"))

sin_polygons %>%
  ggplot() +
  geom_sf(aes(fill = sin_class,
              color = sin_class)) +
  labs(fill = "Sinuosity\nClass",
       color = "Sinuosity\nClass") +
  theme_bw()

# create one sf object for each sinuosity class
sin_names = unique(sin_polygons$sin_class)
for(sin in sin_names) {
  assign(as.character(sin), filter(sin_polygons, sin_class == sin))
}

# extract depth raster values according to sinuosity polygons
# for(sin in sin_names) {
#   sf_tmp = raster::extract(d_raster,
#                            get(as.character(sin)),
#                            fun = NULL,
#                            df = TRUE,
#                            na.rm = TRUE) %>%
#     drop_na() %>%
#     mutate(sin_class = as.character(sin))
#   assign(paste0("d_extract_", sin), sf_tmp)
#   rm(sf_tmp)
# }
#
# depth_extract = bind_rows(lapply(ls(pattern = "^d_extract"), function(x) get(x))) %>%
#   rename(value = d_jan_v2)
# depth_extract$metric = "depth"

# extract velocity raster values according to sinuosity polygons
# for(sin in sin_names) {
#   sf_tmp = raster::extract(v_raster,
#                            get(as.character(sin)),
#                            fun = NULL,
#                            df = TRUE,
#                            na.rm = TRUE) %>%
#     drop_na() %>%
#     mutate(sin_class = as.character(sin))
#   assign(paste0("v_extract_", sin), sf_tmp)
#   rm(sf_tmp)
# }

velocity_extract = bind_rows(lapply(ls(pattern = "^v_extract"), function(x) get(x))) %>%
  rename(value = v_jan_v2)
velocity_extract$metric = "velocity"

d_v_avail_sin = bind_rows(depth_extract, velocity_extract)
save(d_v_avail_sin,
     file = paste0(hab_avail_path, "prepped/d_v_avail_sin.rda"))

#########################
# HABITAT USE DATA PREP #
#########################
hab_use_path = paste(nas_prefix, "data/habitat/lemhi_telemetry/use/", sep = '/')
use_pts = read_csv(paste0(hab_use_path, "raw/Fish_Info_Master_NPedits.csv")) %>%
  clean_names(case = "snake")

# convert to sf object
use_pts_sf = use_pts %>%
  st_as_sf(coords = c("long", "lat"),
           crs = "+proj=longlat +datum=WGS84") %>%
  st_transform(my_crs)

# plot again for verification
xs_avail_p = xs_center_pts %>%
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
       color = "Sinuosity\nCategory",
       title = "Use and Availability Pts")
xs_avail_p

# for each use pt, what is nearest xs?
xs_2_use = use_pts_sf %>%
  st_join(xs_center_pts,
          join = st_nearest_feature,
          left = T) %>%
  st_drop_geometry()

# use_avail_2_xs = avail_2_xs %>%
#   left_join(xs_2_use %>%
#               select(Name, use_global_id = global_id))
# sum(!is.na(use_avail_2_xs$avail_global_id)) # 179
# sum(!is.na(use_avail_2_xs$use_global_id))   # 212
# # I lost one record!? Need to sleuth

# # write results
# st_write(use_avail_2_xs,
#          paste(nas_prefix,
#                "data/habitat/lemhi_telemetry/prepped/use_avail_2_xs.shp",
#                sep = "/"))


xs_use = xs_2_use %>%
  rename(bank_type_condition_closest = bank_type_condition,
         dominant_cover_type_1_5m_radius = dominant_cover_1_5m_radius,
         distance_to_cover_round_to_nearest_0_1m = distance_to_cover_m) %>%
  dplyr::select(global_id,
                date:habitat_selected,
                one_of(names(xs_avail)),
                depth_m:notes,
                x, y) %>%
  mutate(channel_unit_type = recode(channel_unit_type,
                                    "Rapid Plus" = "Rapid+",
                                    "Off Channel Area" = 'OCA',
                                    'Side Channel' = 'SC'),
         channel_unit_type = factor(channel_unit_type,
                                    levels = c('Pool', 'Riffle', 'Run', 'Rapid+', 'SC', 'OCA')),
         substrate_concealment = recode(substrate_concealment,
                                        'no' = 'N',
                                        'No' = 'N',
                                        'yes' = 'Y',
                                        'Yes' = 'Y'),
         bank_type_condition_closest = recode(bank_type_condition_closest,
                                              "riprap" = "Riprap",
                                              "Bar_Island" = "Bar/Island",
                                              'Bar Island' = "Bar/Island"),
         dominant_cover_type_1_5m_radius = recode(dominant_cover_type_1_5m_radius,
                                                  'Aquatic Vegetation' = 'Aquatic Veg.',
                                                  'Terrestrial Vegetation' = 'Terrestrial Veg.'),
         habitat_selected = recode(habitat_selected,
                                   'Not selected' = 'Not Selected')) %>%
  mutate(date = mdy(date)) %>%
  mutate_at(vars(channel_unit_type,
                 bank_type_condition_closest,
                 dominant_cover_type_1_5m_radius,
                 dominant_substrate_1mx1m,
                 substrate_concealment),
            list(as.factor)) %>%
  mutate(dominant_cover_type_1_5m_radius = factor(dominant_cover_type_1_5m_radius,
                                                  levels = c(levels(xs_avail$dominant_cover_type_1_5m_radius), "Unknown"))) %>%
  arrange(radio_frequency, date)

# save for later
save(xs_use,
     use_pts_sf,
     file = paste0(hab_use_path,
                  "prepped/habitat_use.rda"))

# make spatial again
xs_use_sf = xs_use %>%
  left_join(use_pts_sf %>%
              dplyr::select(global_id)) %>%
  st_as_sf()

xs_use_sf %>%
  ggplot() +
  geom_sf(aes(color = habitat_selected))

xs_use %>%
  group_by(radio_frequency) %>%
  summarise(n_dets = n_distinct(date)) %>%
  arrange(desc(n_dets))

xs_use %>%
  group_by(Category) %>%
  summarise(n_fish = n_distinct(radio_frequency))

xs_use %>%
  tabyl(Category, tag_status) %>%
  adorn_totals(where = 'col') %>%
  adorn_percentages() %>%
  adorn_pct_formatting()

xs_use %>%
  filter(tag_status != 'Mortality') %>%
  ggplot(aes(x = Category,
             fill = channel_unit_type)) +
  geom_bar(position = position_fill()) +
  scale_fill_brewer(palette = 'Set1',
                    name = 'CU Type') +
  labs(x = 'Sinuousity Class',
       y = 'Percentage')

xs_use %>%
  ggplot(aes(x = Category,
             fill = tag_status)) +
  geom_bar(position = position_fill()) +
  scale_fill_brewer(palette = 'Set1',
                    name = 'Tag Status') +
  labs(x = 'Sinuousity Class',
       y = 'Percentage')

xs_all = xs_use %>%
  mutate(source = 'Use') %>%
  bind_rows(xs_avail %>%
              mutate(source = 'Avail.')) %>%
  mutate(dominant_cover_type_1_5m_radius = factor(dominant_cover_type_1_5m_radius,
                                                  levels = levels(xs_use$dominant_cover_type_1_5m_radius))) %>%
  mutate(dominant_substrate_1mx1m = fct_recode(dominant_substrate_1mx1m,
                                               Boulder = "boulder",
                                               Cobble = "cobble",
                                               Fines = "silt_fines",
                                               Sand = "sand",
                                               Gravel = 'gravel'),
         dominant_substrate_1mx1m = fct_relevel(dominant_substrate_1mx1m,
                                                "Gravel",
                                                after = 2))


# some preliminary plots comparing use and availability
xs_all %>%
  # filter(source == 'Avail.' |  tag_status != 'Mortality') %>%
  filter(source == 'Avail.' |  habitat_selected == 'Selected') %>%
  ggplot(aes(x = source,
             fill = channel_unit_type)) +
  geom_bar(position = position_fill()) +
  scale_fill_brewer(palette = 'Set1',
                    name = 'CU Type') +
  facet_wrap(~ Category) +
  labs(x = 'Data Set',
       y = 'Percentage')


xs_all %>%
  # filter(source == 'Avail.' |  tag_status != 'Mortality') %>%
  filter(source == 'Avail.' |  habitat_selected == 'Selected') %>%
  filter(!is.na(substrate_concealment)) %>%
  ggplot(aes(x = source,
             fill = substrate_concealment)) +
  geom_bar(position = position_fill()) +
  scale_fill_brewer(palette = 'Set1',
                    name = 'Concealment') +
  facet_wrap(~ Category) +
  labs(x = 'Data Set',
       y = 'Percentage')


xs_all %>%
  # filter(source == 'Avail.' |  tag_status != 'Mortality') %>%
  filter(source == 'Avail.' |  habitat_selected == 'Selected') %>%
  filter(!is.na(dominant_cover_type_1_5m_radius)) %>%
  ggplot(aes(x = source,
             fill = dominant_cover_type_1_5m_radius)) +
  geom_bar(position = position_fill()) +
  scale_fill_brewer(palette = 'Set1',
                    name = 'Cover Type') +
  facet_wrap(~ Category) +
  labs(x = 'Data Set',
       y = 'Percentage')


xs_all %>%
  # filter(source == 'Avail.' |  tag_status != 'Mortality') %>%
  filter(source == 'Avail.' |  habitat_selected == 'Selected') %>%
  filter(!is.na(dominant_substrate_1mx1m)) %>%
  ggplot(aes(x = source,
             fill = dominant_substrate_1mx1m)) +
  geom_bar(position = position_fill()) +
  scale_fill_brewer(palette = 'Set1',
                    name = 'Substrate') +
  facet_wrap(~ Category) +
  labs(x = 'Data Set',
       y = 'Percentage')

xs_all %>%
  # filter(source == 'Avail.' |  tag_status != 'Mortality') %>%
  filter(source == 'Avail.' |  habitat_selected == 'Selected') %>%
  filter(!is.na(bank_type_condition_closest)) %>%
  ggplot(aes(x = source,
             fill = bank_type_condition_closest)) +
  geom_bar(position = position_fill()) +
  scale_fill_brewer(palette = 'Set1',
                    name = 'Bank Type') +
  facet_wrap(~ Category) +
  labs(x = 'Data Set',
       y = 'Percentage')


xs_all %>%
  filter(source == 'Use',
         habitat_selected == 'Selected') %>%
  tabyl(Category) %>%
  adorn_pct_formatting()

xs_center_pts %>%
  st_drop_geometry() %>%
  tabyl(Category) %>%
  adorn_pct_formatting()


