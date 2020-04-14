#' @title Summarize Receiver Operation Times
#'
#' @description Summarize receiver operational times using data from the timer tags at each site
#'
#' @author Mike Ackerman and Kevin See
#'
#' @param timer_data the data frame containing the timer tag data e.g., from parse_timer()
#' @param receiver_codes character vector of receiver codes to query for. Default is \code{NULL} which will keep all receiver codes found in the path folder
#' @param season_start format "%Y-%m-%d" the first day of the season that the receivers were considered turned on
#' @param season_end format "%Y-%m-%d" the final day of the season that the receivers were all turned off
#'
#' @import dplyr lubridate
#' @export
#' @return a data frame with T/F summarizing operational times for each receiver

summarise_timer_data = function(timer_data = NULL,
                                receiver_codes = NULL,
                                season_start = NULL,
                                season_end = NULL) {

  # range of time among timer tags
  hr_range = lubridate::floor_date(range(timer_data$start), unit = "hours")
  if(!is.null(season_start)) hr_range[1] <- season_start
  if(!is.null(season_end))   hr_range[2] <- season_end

  # hours in hr_range
  n_hrs = difftime(hr_range[2],
                   hr_range[1],
                   units = "hours") %>%
    as.integer()

  # get list of all unique receivers in timer_data
  receiver_nms = sort(unique(timer_data$receiver))

  # if user provides a list of receiver codes
  if(!is.null(receiver_codes)) {
    receiver_nms = receiver_nms[receiver_nms %in% receiver_codes]
  }

  tmp = timer_data %>%
    mutate(hr = lubridate::floor_date(start,
                                      unit = "hours")) %>%
    # include all hours in the hr_range. This adds a record to timer_data for each instance that no timer
    # tag data exists for a receiver i.e. adds a record for each hour that a receiver was not operational
    dplyr::full_join(expand.grid(list(receiver = receiver_nms,
                                      hr = hr_range[1] + lubridate::dhours(seq(0, n_hrs)))) %>%
                       tbl_df()) %>%
    # when did each site first come online?
    dplyr::left_join(timer_data %>%
                       group_by(receiver) %>%
                       summarise(receiver_start_hr = min(lubridate::floor_date(start,
                                                                               unit = "hours"),
                                                na.rm = T)) %>%
                       ungroup()) %>%
    mutate(operational = ifelse(!is.na(n), 1, 0)) %>%
    arrange(receiver, hr) %>%
    # for each hour, is the receiver opeational?
    group_by(receiver, receiver_start_hr, hr) %>%
    summarise_at(vars(operational),
                 funs(max)) %>%
    mutate(operational = ifelse(operational == 1, T, F)) %>%
    ungroup() %>%
    mutate(receiver = factor(receiver,
                             levels = receiver_nms))

  return(tmp)

} # end rcvr_ops_summary()
