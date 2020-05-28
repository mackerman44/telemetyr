#' @title Summarize Movement Rates and Times
#'
#' @description Summarise movement times and rates between sites. Currently, the function is only built to
#' evaluate movement times/rates, spatially, but may be modified in the future to evaluate times/rates
#' temporally as well.
#'
#' @author Mike Ackerman and Kevin See
#'
#' @inheritParams parse_tag_list
#'
#' @param cap_hist_long a data frame containing capture histories in long format. Typically, the \code{ch_long} object
#' in the \code{cap_hist_list} returned from \code{prep_capture_history}.
#' @param which_obs the column in \code{cap_hist_long} containing the dates and times in POSIXct format the user
#' would like to use to calculate movement times. The default is "first_obs", but also currently allows "last_obs".
#' @param hr_max an optional argument to force the ylim of \code{reach_hours_box_p}. Warning: by setting \code{hr_max}
#' to a value less than the range of the data, outlier values will be excluded from plotting.
#'
#' @import dplyr ggplot2 magrittr purrr
#' @export
#' @return a list containing summaries of movement rates and times

summarise_movement = function(cap_hist_long = NULL,
                              tags = NULL,
                              which_obs = c("first_obs", "last_obs"),
                              hr_max = NULL) {

  stopifnot(!is.null(cap_hist_long))

  which_obs = match.arg(which_obs)

  # set tag_list
  tag_list = unique(cap_hist_long$tag_id) # the NULL - all tags
  # if user provide a list of tags
  if(!is.null(tags)) {
    tag_list = tag_list[tag_list %in% tags]
  }

  #-------------------------
  # summarize sequential observations by tag_id
  #-------------------------
  tmp = cap_hist_long %>%
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

  # set columns and string to remove - no longer needed
  # col_rmv = case_when(
  #   which_obs == "first_obs" ~ "last_obs",
  #   which_obs == "last_obs" ~ "first_obs",
  # )
  # rmv = gsub(pattern = "obs",
  #            replacement = "",
  #            x = which_obs)

  #-------------------------
  # plot days/hours between sites
  #-------------------------
  # facetted histogram
  move_space_p = tmp %>%
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

  # boxplot
  my_coord_cartesian = NULL
  if(!is.null(hr_max)) {
    my_coord_cartesian = c(0, hr_max)
  }

  move_space_p2 = tmp %>%
    ggplot(aes(x = rt_reach)) +
    geom_boxplot(aes(y = time_hrs,
                     fill = rt_reach)) +
    coord_cartesian(ylim = my_coord_cartesian) +
    theme_bw() +
    theme(legend.position = "none") +
    labs(x = "Reach",
         y = "Hours")
  move_space_p2

  #-------------------------
  # plot days/hours by study week
  #-------------------------
  move_time_p = tmp %>%
    ggplot(aes(x = time_days)) +
    geom_histogram(fill = "red",
                   color = "red",
                   binwidth = 0.25) +
    geom_vline(xintercept = 0,
               linetype = 2) +
    facet_wrap(~ week,
               scales = "free_y") +
    theme_bw() +
    labs(x = "Days",
         y = "Count")
  move_time_p

  move_time_p2 = tmp %>%
    ggplot(aes(x = week)) +
    geom_boxplot(aes(group = week,
                     y = time_hrs),
                 fill = "red") +
    coord_cartesian(ylim = my_coord_cartesian) +
    theme_bw() +
    labs(x = "Week",
         y = "Hours")
  move_time_p2


  #-------------------------
  # a quick summary of reach movement times
  #-------------------------
  # a function for summarizing quantiles
  cuts = c(0.25, 0.50, 0.75)
  cut_names = map_chr(cuts, ~paste0(.x*100, "%"))
  cut_funs = map(cuts, ~partial(quantile, probs = .x, na.rm = TRUE)) %>%
    set_names(nm = cut_names)

  tmp_summ = tmp %>%
    group_by(rt_reach) %>%
    summarise(n_tags = n(),
              mn_hrs = mean(time_hrs),
              sd_hrs = sd(time_hrs)) %>%
    left_join(tmp %>%
                group_by(rt_reach) %>%
                summarise_at(vars(time_hrs),
                             funs(!!!cut_funs))) %>%
    ungroup()

  #-------------------------
  # a plot of tmp_summ
  #-------------------------
  tmp_summ_p = tmp_summ %>%
    ggplot(aes(x = rt_reach)) +
    geom_col(aes(y = `50%`),
             fill = "purple") +
    theme_bw() +
    labs(x = "Reach",
         y = "Median Hours")
  tmp_summ_p

  #-------------------------
  # objects to return
  #-------------------------
  tmp_list = list(movement_df = tmp,
                  movement_summ = tmp_summ,
                  spatial_p = move_space_p,
                  spatial_box_p = move_space_p2,
                  temporal_p = move_time_p,
                  temporal_box_p = move_time_p2)
  return(tmp_list)

} # end summarise_movement()


