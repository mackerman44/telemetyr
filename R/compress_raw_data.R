#' @title Clean and Compress Raw Telemetry Data
#'
#' @description A wrapper function for \code{clean_raw_data()}, \code{round_tag_codes()},
#' and \code{compress_txt_data()}. Cleans records, rounds tag codes, and then compresses the detections,
#' showing the first and last detection on each receiver for each tag, and how many times it was
#' detected on that receiver in that window. \code{compress_raw_data} is similar to the routine
#' Tracker uses to compress .txt format data to .csv format.
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
#' @return a data frame containing a compressed format of the telemetry receiver records

compress_raw_data = function(data_df = NULL,
                             min_yr = 2017,
                             max_yr = NA,
                             filter_valid = T,
                             round_to = 10,
                             max_min = 2,
                             assign_week = T,
                             week_base = "0901",
                             append_week = c('first', 'last')) {

  # cat("Cleaning data and fixing dates.\n")

  clean_df = data_df %>%
    clean_raw_data(min_yr = min_yr,
                   max_yr = max_yr,
                   filter_valid = filter_valid)

  cat('Rounding tag codes.\n')

  round_df = clean_df %>%
    round_tag_codes(round_to = round_to)

  cat("Compressing observations.\n")

  compress_df = round_df %>%
    compress_txt_data(max_min = max_min,
                      assign_week = assign_week,
                      week_base = week_base,
                      append_week = append_week)

  return(compress_df)

}
