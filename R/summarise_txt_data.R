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
           new_grp = if_else(diff > 5 | is.na(diff),
                             T, F)) %>%
    select(receiver:tag_id, prev_time,
           obs_time = date_time,
           diff, new_grp) %>%
    ungroup()

  grp_df = prep_data %>%
    filter(new_grp) %>%
    group_by(tag_id) %>%
    mutate(grp_num = 1:n())

  prep_data = prep_data %>%
    left_join(grp_df) %>%
    fill(grp_num)




}
