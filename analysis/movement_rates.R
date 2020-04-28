#-----------------------------------------------
#
# A script to start playing around with travel time
# and/or movement rate analysis before turning code into
# functions.
#
# Created by: Mike Ackerman
# Date created: 4/24/2020
# Last modified:
#
#-----------------------------------------------

#-------------------------
# load necessary libraries
#-------------------------
library(dplyr)
library(ggplot2)

#-------------------------
# load compressed data
#-------------------------
load("data/prepped/2018_2019/cap_hist.rda")
head(cap_hist_list$ch_long)
head(cap_hist_list$tag_df)

#-------------------------
# how many days/hours between consecutive sites?
#-------------------------
# arguments
cap_hist_df = cap_hist_list$ch_long
tags = NULL
which_obs = "first_obs"

move_rate_df = cap_hist_df %>%
  select(-n) %>%
  mutate(loc_order = as.integer(loc)) %>%
  group_by(tag_id) %>%
  mutate(next_loc = lead(loc),
         next_loc_order = lead(loc_order),
         next_loc_first_obs = lead(first_obs),
         next_loc_last_obs  = lead(last_obs)) %>%
  filter(next_loc_order - loc_order == 1) %>%
  mutate(time_hours = as.numeric(difftime(next_loc_first_obs, first_obs, units = "hours"))) %>%
  mutate(time_days = time_hours / 24) %>%
  mutate(rt_reach = paste0(loc, "2", next_loc),
         rt_reach = factor(rt_reach,
                           levels = paste0(levels(cap_hist_df$loc)[1:length(levels(cap_hist_df$loc)) -1],
                                           "2",
                                           levels(cap_hist_df$loc)[-1]))) %>%
  filter(time_hours > 0)

# TO-DO:
# 1. Add option to use first_obs or last_obs information
# 2. Add week so that travel time can be summarised spatially or temporally

#-------------------------
# plot times between sites
#-------------------------
move_rate_p = move_rate_df %>%
  ggplot(aes(x = time_days)) +
  geom_histogram(fill = "steelblue",
                 color = "steelblue",
                 binwidth = 0.25) +
  # geom_dotplot(fill = "steelblue") +
  # geom_density(fill = "steelblue",
  #              alpha = 0.5) +
  geom_vline(xintercept = 0,
             linetype = 2) +
  facet_wrap(~ rt_reach,
             scales = "free") +
  theme_bw() +
  labs(x = "Days",
       y = "Count")
move_rate_p

#-------------------------
# boxplot of movement times
#-------------------------
move_rate_p2 = move_rate_df %>%
  ggplot(aes(x = rt_reach)) +
  geom_boxplot(aes(y = time_hours),
               fill = "lightblue") +
  theme_bw() +
  labs(x = "Reach",
       y = "Hours")
move_rate_p2

# TO-DO:
# 3. Modify so that fill or color can easily be specified by user

#-------------------------
# summarise movement times
#-------------------------
move_rate_summ = move_rate_df %>%
  group_by(loc, next_loc, rt_reach) %>%
  summarise(n_tags = n_distinct(tag_id),
            mn_hours = mean(time_hours),
            md_hours = median(time_hours),
            min_hours = min(time_hours),
            max_hours = max(time_hours),
            mn_days = mean(time_days),
            md_days = median(time_days),
            min_days = min(time_days),
            max_days = max(time_days)) %>%
  ungroup()

#-------------------------
# plot summary of movement times
#-------------------------
move_rate_summ_p = move_rate_summ %>%
  ggplot(aes(x = rt_reach)) +
  geom_col(aes(y = md_hours),
           fill = "purple") +
  theme_bw() +
  labs(x = "Reach",
       y = "Median Hours")
move_rate_summ_p

# TO-DO:
# 4. Add distances for each reach to calculate movement rates (km/day)

# Example from pilotStudy.R:
# travel_speed = travel_summ %>%
#   left_join(tibble(next_site = c('LH','MB','TR','RR','BG','NF','DW','LR','SR','CC','VC','SB','TB'),
#                    km = c(6.6, 9, 7.2, 5.5, 6.7, 4.6, 9.2, 14.6, 34.2, 18.2, 124.8, 39.4, 36.6))) %>%
#   mutate_at(vars(matches('Days$')),
#             funs(km / .)) %>%
#   select(site, next_site,
#          km,
#          n_tags,
#          min_speed = max_days,
#          median_speed = median_days,
#          mean_speed = mean_days,
#          max_speed = min_days) %>%
#   left_join(travel_summ)
# travel_speed
