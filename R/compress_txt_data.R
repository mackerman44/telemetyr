#' @title Compress rounded RT observations
#'
#' @description Compresses cleaned detections, showing the first and last detection on each receiver for each tag, and how many times it was detected on that receiver in that window.
#'
#' @author Kevin See and Mike Ackerman
#'
#' @param data_df data.frame containing all valid observations, output from \code{read_txt_data()}, followed by \code{clean_raw_data()}, followed by \code{round_tag_codes()}
#' @param max_min maximum number of minutes between detections of a tag before it's considered a different "group" of detections. Default is 2.
#' @param assign_week Should this function assign a week number to the output? Default is \code{TRUE}
#' @param week_base If assigning week numbers, the date when the numbering should start in MMDD format
#' @param append_week If assigning weeks, should the week be assigned based on the \code{first} or \code{last} time a tag was detected on that receiver? Default value is \code{first}.
#'
#'
#' @import dplyr lubridate tidyr
#' @importFrom magrittr %<>%
#' @export
#' @return a data.frame containing a summary of the raw data

compress_txt_data = function(data_df = NULL,
                              max_min = 2,
                              assign_week = T,
                              week_base = "0901",
                              append_week = c('first', 'last')) {

  stopifnot(!is.null(data_df))

  append_week = match.arg(append_week)

  prep_data = data_df %>%
    arrange(tag_id, date_time) %>%
    group_by(tag_id) %>%
    mutate(prev_time = lag(date_time),
           diff = as.numeric(difftime(date_time, prev_time, units = 'mins')),
           new_grp = if_else(diff > max_min | is.na(diff),
                             T, F)) %>%
    select(receiver:tag_id, prev_time,
           obs_time = date_time,
           diff, new_grp) %>%
    ungroup()

  grp_df = prep_data %>%
    filter(new_grp) %>%
    group_by(tag_id) %>%
    mutate(grp_num = 1:n())

  prep_data %<>%
    left_join(grp_df) %>%
    tidyr::fill(grp_num)


  summ_data = prep_data %>%
    group_by(receiver, tag_id, grp_num) %>%
    summarise(start = min(obs_time),
              end = max(obs_time),
              n = n()) %>%
    ungroup() %>%
    select(-grp_num) %>%
    mutate(valid = as.integer(1)) %>%
    select(receiver, valid, tag_id, start:n)

  if(assign_week) {
    start_date = ymd(paste(year(min(summ_data$start)), week_base))
    if(append_week == 'first') {
      summ_data %<>%
        mutate(week = difftime(start, start_date, units = 'weeks'),
               week = as.numeric(week),
               week = floor(week) + 1)
    }
    if(append_week == 'last') {
      summ_data %<>%
        mutate(week = difftime(end, start_date, units = 'weeks'),
               week = as.numeric(week),
               week = floor(week) + 1)
    }
  }


  return(summ_data)

}
