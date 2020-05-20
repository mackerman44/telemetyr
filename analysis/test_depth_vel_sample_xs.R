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
