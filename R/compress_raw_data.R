#' @title Compress raw RT observations
#'
#' @description Cleans detections, rounds observed tag codes and compresses the detections, showing the first and last detection on each receiver for each tag, and how many times it was detected on that receiver in that window.
#'
#' @author Kevin See and Mike Ackerman
#'
#' @inheritParams clean_raw_data
#' @inheritParams round_tag_codes
#' @inheritParams compress_txt_data
#'
#' @import dplyr stringr lubridate tidyr
#' @importFrom magrittr %<>%
#' @export
#' @return a data.frame containing a summary of the raw data

compress_raw_data = function(data_df = NULL,
                             filter_valid = T,
                             round_to = 10,
                             max_min = 2,
                             assign_week = T,
                             week_base = "0901",
                             append_week = c('first', 'last')) {

  compress_df = data_df %>%
    clean_raw_data(filter_valid = filter_valid) %>%
    round_tag_codes(round_to = round_to) %>%
    compress_txt_data(max_min = max_min,
                      assign_week = assign_week,
                      week_base = week_base,
                      append_week = append_week)

  return(compress_df)
}
