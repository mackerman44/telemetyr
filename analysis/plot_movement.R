#-----------------------------------------------
#
# A script for plotting movement
#
# Created by: Mike Ackerman & Kevin See
# Date created: 5/28/20
# Last modified:
#
#-----------------------------------------------

#-------------------------
# load necessary libraries
#-------------------------
library(telemetyr)
library(tidyverse)
library(readxl)
library(magrittr)

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
# read in RT site metadata
#-------------------------
rec_meta = read_excel(paste0(nas_prefix, '/data/telemetry/lemhi/site_metadata/rt_site_metadata.xlsx'))

rec_df = rec_meta %>%
  filter(site_type %in% c('rt_fixed', 'rst')) %>%
  arrange(desc(rt_rkm)) %>%
  dplyr::select(site = site_code,
                receivers, rt_rkm) %>%
  group_by(site, rt_rkm) %>%
  nest() %>%
  ungroup() %>%
  mutate(receiver = map(data,
                        .f = function(x) {
                          str_split(x, "\\,") %>%
                            extract2(1) %>%
                            str_trim()
                        })) %>%
  dplyr::select(-data) %>%
  unnest(cols = receiver) %>%
  mutate(receiver = if_else(grepl('NA', receiver),
                            NA_character_,
                            receiver)) %>%
  mutate_at(vars(site, receiver),
            list(~ factor(., levels = unique(.))))


# drop a few upstream sites for these analyses
rec_df %<>%
  filter(!site %in% c('PAHTRP', 'DG', 'KP', 'DC', 'HYDTRP')) %>%
  mutate_at(vars(site, receiver),
            list(fct_drop)) %>%
  mutate(site_num = as.integer(site))


#-------------------------
# load long capture history
#-------------------------
ch_long_all = tibble(season = c('17_18',
                                '18_19',
                                '19_20')) %>%
  mutate(cap_hist_list = map(season,
                             .f = function(yr) {
                               load(paste0(nas_prefix, "/Nick/telemetry/raw/cap_hist_", yr, ".rda"))
                               return(cap_hist_list)
                             }),
         cap_hist_long = map(cap_hist_list,
                             .f = "ch_long")) %>%
  dplyr::select(season, cap_hist_long) %>%
  unnest(cols = cap_hist_long) %>%
  mutate(loc = factor(loc,
                      levels = levels(rec_df$site)))




plot_df = ch_long_all %>%
  mutate(loc_num = as.integer(loc)) %>%
  group_by(season, tag_id) %>%
  mutate(n_loc = n_distinct(loc)) %>%
  filter(n_loc > 1) %>%
  mutate(prev_loc = lag(loc_num),
         nxt_loc = lead(loc_num)) %>%
  filter(nxt_loc > loc_num | is.na(nxt_loc)) %>%
  ungroup() %>%
  dplyr::select(-(prev_loc:nxt_loc)) %>%
  mutate_at(vars(loc),
            list(fct_drop)) %>%
  group_by(season) %>%
  mutate(loc_num = as.integer(loc)) %>%
  ungroup()

plot_df %>%
  ggplot(aes(x = first_obs,
             y = loc_num,
             color = as.factor(tag_id))) +
  geom_line() +
  geom_point() +
  scale_y_continuous(name = "Receiver",
                     breaks = 1:max(as.integer(plot_df$loc)),
                     labels = levels(plot_df$loc)) +
  facet_wrap(~ season,
             scales = 'free_x') +
  theme(legend.position = 'none') +
  labs(x = 'First Observation Date')
