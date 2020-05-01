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
    select(-n) %>%
    filter(tag_id %in% tag_list) %>%
    arrange(tag_id) %>%
    mutate(loc_order = as.integer(loc)) %>%
    group_by(tag_id) %>%
    mutate(next_loc = lead(loc),
           next_loc_order = lead(loc_order),
           next_loc_first_obs = lead(first_obs),
           next_loc_last_obs = lead(last_obs)) %>%
    filter(next_loc_order - loc_order == 1) %>%
    mutate(rt_reach = paste0(loc, "2", next_loc),
           rt_reach = factor(rt_reach,
                             levels = paste0(levels(cap_hist_long$loc)[1:length(levels(cap_hist_long$loc)) -1],
                                             "2",
                                             levels(cap_hist_long$loc)[-1])))

  # set columns and string to remove
  col_rmv = case_when(
    which_obs == "first_obs" ~ "last_obs",
    which_obs == "last_obs" ~ "first_obs",
  )
  rmv = gsub(pattern = "obs",
             replacement = "",
             x = which_obs)

  # select columns in tmp based on which_obs
  tmp %<>%
    select(-ends_with(col_rmv)) %>%
    rename_at(.vars = vars(contains(rmv)),
              .funs = funs(sub(rmv, "", .))) %>%
    select(tag_id,
           rt_reach,
           loc,
           loc_order,
           obs,
           next_loc,
           next_loc_order,
           next_loc_obs) %>%
    # calculate travel times
    mutate(hours = as.numeric(difftime(next_loc_obs, obs, units = "hours"))) %>%
    mutate(days = hours / 24) %>%
    filter(hours > 0)

  # TO-DO: Consider functionality to include week here so that travel time could be summarised temporally

  #-------------------------
  # plot times in reaches
  #-------------------------
  tmp_p = tmp %>%
    ggplot(aes(x = hours)) +
    geom_histogram(fill = "dodgerblue4",
                   color = "dodgerblue4") +
    # geom_dotplot(fill = "dodgerblue4") +
    # geom_density(fill = "dodgerblue4",
    #              alpha = 0.5) +
    geom_vline(xintercept = 0,
               linetype = 2) +
    facet_wrap(~ rt_reach,
               scales = "free") +
    theme_bw() +
    labs(x = "Days",
         y = "Count")

  #-------------------------
  # boxplot of reach migration times
  #-------------------------
  # to forced ylim
  my_coord_cartesian = NULL
  if(!is.null(hr_max)) {
    my_coord_cartesian = c(0, hr_max)
  }

  tmp_p2 = tmp %>%
    ggplot(aes(x = rt_reach)) +
    geom_boxplot(aes(y = hours,
                     fill = rt_reach)) +
    coord_cartesian(ylim = my_coord_cartesian) +
    theme_bw() +
    theme(legend.position = "none") +
    labs(x = "Reach",
         y = "Hours")
  tmp_p2

  # TO-DO: Modify so that fill or color can easily be specified by user

  #-------------------------
  # a quick summary of reach movement times
  #-------------------------
  # a function for summarizing quantiles
  cuts = c(0.25, 0.50, 0.75)
  cut_names = map_chr(cuts, ~paste0(.x*100, "%"))
  cut_funs = map(cuts, ~partial(quantile, probs = .x, na.rm = TRUE)) %>%
    set_names(nm = cut_names)

  tmp_summ = tmp %>%
    group_by(loc, next_loc, rt_reach) %>%
    summarise(n_tags = n(),
              mn_hrs = mean(hours),
              sd_hrs = sd(hours)) %>%
    left_join(tmp %>%
                group_by(loc, next_loc, rt_reach) %>%
                summarise_at(vars(hours),
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
                  reach_hours_p = tmp_p,
                  reach_hours_box_p = tmp_p2,
                  median_hours_p = tmp_summ_p)
  return(tmp_list)

  # TO-DO: Add distances for each reach to calculate movement rates (km/day)
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

} # end summarise_movement()


