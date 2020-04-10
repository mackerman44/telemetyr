#' @title Summarise raw RT observations
#'
#' @description Summarises cleaned detections, showing the first and last detection on each receiver for each tag, and how many times it was detected on that receiver in that window.
#'
#' @author Kevin See
#'
#' @param data data.frame containing all valid observations, output from \code{read.txt.data()}, followed by \code{clean.raw.data()}, followed by \code{round.tag.codes()}
#'
#' @import dplyr purrr readr stringr lubridate
#' @export
#' @return a data frame containing a summary of the raw data

summarise.txt.data = function(data = NULL) {

  stopifnot(!is.null(data))

  prep_data = data %>%
    arrange(tag_id, date_time) %>%
    group_by(tag_id) %>%
    mutate(prev_time = lag(date_time),
           diff = as.numeric(difftime(date_time, prev_time, units = 'mins')),
           same_grp = if_else(diff > 5 | is.na(diff),
                             F, T)) %>%
    select(receiver:tag_id, prev_time,
           obs_time = date_time,
           diff, same_grp) %>%
    mutate(next_grp = lead(same_grp),
           grp = NA) %>%
    ungroup()

  tmp = prep_data
  # prep_data = tmp

  test = prep_data %>%
    mutate(lag_tag = lag(tag_id)) %>%
    mutate(grp = if_else(is.na(lag_tag) | lag_tag != tag_id,
                         1,
                         NA_real_)) %>%
    slice(1:9) %>%
    mutate(grp = if_else((tag_id == lag_tag | is.na(lag_tag)) & is.na(grp),
                         if_else(!same_grp,
                                 lead(grp),
                                 grp),
                         NA_real_))
    select(-lag_tag)


    # for(i in 2:nrow(prep_data)) {
    #   if(prep_data$tag_id[i] == prep_data$tag_id[i - 1] ) {
    #     prep_data$grp[i] = if_else(prep_data$same_grp[i],
    #                                prep_data$grp[i - 1],
    #                                prep_data$grp[i - 1] + 1)
    #
    #   }
    # } goal_df = prep_data


}
