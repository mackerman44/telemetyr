#-----------------------------------------------
#
# A script to start playing around with travel time
# and/or movement rate analysis before turning code into
# functions.
#
# Created by: Mike Ackerman
# Date created: 4/24/2020
# Last modified: 5/28/2020
#
#-----------------------------------------------

# load necessary libraries
library(tidyverse)
library(magrittr)
library(readxl)

# set NAS prefix, depending on operating system
if(.Platform$OS.type != 'unix') {
  nas_prefix = "S:"
}
if(.Platform$OS.type == 'unix') {
  nas_prefix = "~/../../Volumes/ABS"
}

#-----------------
# site metadata
#-----------------
site_meta = read_excel(paste0(nas_prefix, '/data/telemetry/lemhi/site_metadata/rt_site_metadata.xlsx'))

site_df = site_meta %>%
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
site_df %<>%
  filter(!site %in% c('PAHTRP', 'DG', 'KP', 'DC', 'HYDTRP')) %>%
  mutate_at(vars(site, receiver),
            list(fct_drop)) %>%
  mutate(site_num = as.integer(site))

#-----------------
# load capture histories
#-----------------
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
                      levels = levels(site_df$site)))

#-------------------------
# final prep, arguments, etc.
#-------------------------
ch_17_18 = ch_long_all %>% filter(season == "17_18")
ch_18_19 = ch_long_all %>% filter(season == "18_19")
ch_19_20 = ch_long_all %>% filter(season == "19_20")

# arguments
which_obs = "first_obs"
#tags = NULL
cap_hist_long = ch_18_19

#-------------------------
# days/hours between sites
#-------------------------
move_df = cap_hist_long %>%
  select_if(names(.) %in% c("tag_id", "loc", "week", as.character(which_obs))) %>%
  mutate(loc_order = as.integer(loc)) %>%
  rename(obs_time = all_of(which_obs)) %>%
  group_by(tag_id) %>%
  mutate(next_loc = lead(loc),
         next_loc_order = lead(loc_order),
         next_loc_obs_time = lead(obs_time)) %>%
  filter(next_loc_order - loc_order == 1) %>%
  mutate(time_hrs = as.numeric(
    difftime(next_loc_obs_time,
             obs_time,
             units = "hours"))) %>%
  mutate(time_days = time_hrs / 24) %>%
  mutate(rt_reach = paste0(loc, "2", next_loc),
         rt_reach = factor(rt_reach,
                           levels = paste0(levels(cap_hist_long$loc)[1:length(levels(cap_hist_long$loc)) -1],
                                           "2",
                                           levels(cap_hist_long$loc)[-1]))) %>%
  filter(time_hrs > 0) %>%
  select(tag_id, rt_reach, week, time_hrs, time_days)

#-------------------------
# plot days/hours between sites
#-------------------------
move_space_p = move_df %>%
  ggplot(aes(x = time_days)) +
  geom_histogram(fill = "steelblue",
                 color = "steelblue",
                 binwidth = 0.25) +
  geom_vline(xintercept = 0,
             linetype = 2) +
  facet_wrap(~ rt_reach,
             scales = "free") +
  theme_bw() +
  labs(x = "Days",
       y = "Count")
move_space_p

move_space_p2 = move_df %>%
  ggplot(aes(x = rt_reach)) +
  geom_boxplot(aes(y = time_hrs),
               fill = "lightblue") +
  theme_bw() +
  labs(x = "Reach",
       y = "Hours")
move_space_p2

#-------------------------
# plot days/hours by study week
#-------------------------
move_time_p = move_df %>%
  ggplot(aes(x = time_days)) +
  geom_histogram(fill = "orange",
                 color = "orange",
                 binwidth = 0.25) +
  geom_vline(xintercept = 0,
             linetype = 2) +
  facet_wrap(~ week,
             scales = "free_y") +
  theme_bw() +
  labs(x = "Days",
       y = "Count")
move_time_p

move_time_p2 = move_df %>%
  ggplot(aes(x = week)) +
  geom_boxplot(aes(group = week,
                   y = time_hrs),
               fill = "orange") +
  theme_bw() +
  labs(x = "Week",
       y = "Hours")
move_time_p2

#-------------------------
# summarise movement rates/times
#-------------------------
move_summ_space = move_df %>%
  group_by(rt_reach) %>%
  summarise(n_tags = n_distinct(tag_id),
            mn_hours = mean(time_hrs),
            md_hours = median(time_hrs),
            min_hours = min(time_hrs),
            max_hours = max(time_hrs),
            mn_days = mean(time_days),
            md_days = median(time_days),
            min_days = min(time_days),
            max_days = max(time_days)) %>%
  ungroup()

# move_summ = move_df %>%
#   group_by(rt_reach, week) %>%
#   summarise(n_tags = n_distinct(tag_id),
#             mn_hours = mean(time_hrs),
#             md_hours = median(time_hrs),
#             min_hours = min(time_hrs),
#             max_hours = max(time_hrs),
#             mn_days = mean(time_days),
#             md_days = median(time_days),
#             min_days = min(time_days),
#             max_days = max(time_days)) %>%
#   ungroup()

