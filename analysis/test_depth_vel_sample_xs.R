#-----------------------------------------------
#
# A script for testing depth and velocity samples (power analysis)
#
# Created by: Mike Ackerman & Kevin See
# Date created: 5/20/20
#
#-----------------------------------------------

#-------------------------
# load necessary libraries
#-------------------------
library(tidyverse)
library(sf)

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
# read in depth & velocities
# from sampled transects
#-------------------------
xs_raster = crossing(metric = c('depth', 'velocity'),
         sin_class = c('Low', 'Med', 'High')) %>%
  mutate(sample_xs = map2(metric,
                          sin_class,
                          .f = function(x, y) {
                            st_read(paste0(nas_prefix, "/data/habitat/lemhi_telemetry/availability/cross_sections/DV_extract/", y, "_", str_sub(x, 0, 1), ".shp")) %>%
                              select(Name:Sinuosity, Category,
                                     raster_val = RASTERVALU) %>%
                              mutate_at(vars(raster_val),
                                        list(~ if_else(. == -9999,
                                                       NA_real_,
                                                       .))) %>%
                              mutate(metric = x)
                          }))

dv_sf = xs_raster %>%
  select(-metric) %>%
  unnest(cols = sample_xs) %>%
  st_as_sf()

dv_sf %>%
  filter(metric == 'velocity') %>%
  filter(!is.na(raster_val)) %>%
  ggplot(aes(color = raster_val,
             shape = Category)) +
  scale_color_viridis_c() +
  geom_sf() +
  theme_bw()

dv_df = dv_sf %>%
  st_drop_geometry() %>%
  as_tibble()

dv_df %>%
  ggplot(aes(x = raster_val,
             color = Category,
             fill = Category)) +
  geom_density(alpha = 0.2) +
  # geom_histogram(position = 'dodge') +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  facet_wrap(~ metric,
             scales = 'free',
             nrow = 2) +
  theme_classic() +
  labs(x = 'Raster Value')

#-------------------------
# read in depth & velocities
# from entire raster
#-------------------------
load(paste0(nas_prefix, "/data/habitat/lemhi_telemetry/prepped/depth_extract.rda"))

depth_extract %>%
  ggplot(aes(x = value,
             color = sin_class,
             fill = sin_class)) +
  geom_density(alpha = 0.2) +
  # geom_histogram(position = 'dodge') +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  theme_classic() +
  labs(x = 'Raster Value')


dv_df %>%
  filter(metric == 'depth') %>%
  select(sin_class, raster_val) %>%
  mutate(source = 'Sampled') %>%
  bind_rows(depth_extract %>%
              as_tibble() %>%
              select(-ID) %>%
              rename(raster_val = value) %>%
              mutate(source = 'All')) %>%
  ggplot(aes(x = raster_val,
             color = source,
             fill = source)) +
  geom_density(alpha = 0.2) +
  # geom_histogram(position = 'dodge') +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  facet_wrap(~ sin_class,
             scales = 'free_y') +
  theme_classic() +
  labs(x = 'Raster Value')


dv_df %>%
  filter(metric == 'depth') %>%
  select(sin_class, raster_val) %>%
  mutate(source = 'Sampled') %>%
  bind_rows(depth_extract %>%
              as_tibble() %>%
              select(-ID) %>%
              rename(raster_val = value) %>%
              mutate(source = 'All')) %>%
  group_by(sin_class, source) %>%
  summarise_at(vars(raster_val),
               list(min, max),
               na.rm = T)
