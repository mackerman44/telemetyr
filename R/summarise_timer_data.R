#' @title Summarize Receiver Operation Times
#'
#' @description Summarize receiver operational times using data from the timer tags at each site
#'
#' @author Mike Ackerman and Kevin See
#'
#' @inheritParams prep_capture_history
#' @inheritParams get_file_nms
#'
#' @param season_start character with format "YYYYMMDD" the first day of the season that the receivers were considered turned on
#' @param season_end character with format "YYYYMMDD" the final day of the season that the receivers were all turned off
#'
#' @import dplyr lubridate ggplot2 forcats
#' @export
#' @return summaries of operational times for each receiver

summarise_timer_data = function(compress_df = NULL,
                                receiver_codes = NULL,
                                season_start = NULL,
                                season_end = NULL) {

  cat("Parsing out timer tag data.\n")

  timer_df = compress_df %>%
    parse_code_ending(code_ending = "575$")

  # range of time among timer tags in timer_df
  hr_range = lubridate::floor_date(range(timer_df$start, na.rm = T), unit = "hours")

  # if the season_start or season_end parameters are provided, use them to overwrite values in hr_range
  if(!is.null(season_start)) hr_range[1] <- lubridate::ymd(season_start)
  if(!is.null(season_end))   hr_range[2] <- lubridate::ymd(season_end)

  # hours in hr_range
  n_hrs = difftime(hr_range[2],
                   hr_range[1],
                   units = "hours") %>%
    as.integer()

  # get list of all unique receivers in timer_data
  receiver_nms = sort(unique(timer_df$receiver))

  # if user provides a list of receiver codes
  if(!is.null(receiver_codes)) {
    receiver_nms = receiver_nms[receiver_nms %in% receiver_codes]
  }

  cat("Summarizing hours that receivers are operational.\n")

  # data frame summarizing timer_df
  tmp = suppressWarnings(timer_df %>%
                         filter(receiver %in% receiver_nms) %>%
                         mutate(hr = lubridate::floor_date(start,
                                                           unit = "hours")) %>%
                         filter(hr >= hr_range[1] & hr <= hr_range[2]) %>%
                         # include all hours in the hr_range. This adds a record to timer_data for each instance that no timer
                         # tag data exists for a receiver i.e. adds a record for each hour that a receiver was not operational
                         dplyr::full_join(expand.grid(list(receiver = receiver_nms,
                                                           hr = hr_range[1] + lubridate::dhours(seq(0, n_hrs)))) %>%
                                            tbl_df()) %>%
                         # when did each site first come online?
                         dplyr::left_join(timer_df %>%
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
                                                  levels = receiver_nms)))

  cat("Plotting operation times.\n")

  # receiver operations time plot
  tmp_plot = tmp %>%
    ggplot2::ggplot(aes(x = hr,
                        y = forcats::fct_rev(receiver),
                        color = operational)) +
    geom_line(size = 2,
              color = "black") +
    geom_point(data = tmp %>%
                 filter(!operational),
               size = 1.5,
               color = "palegreen2") +
    theme_bw() +
    labs(x = "Time",
         y = "Receiver")

  cat("What proportion of season was each receiver operational?\n")

  # calculate the proportion of time from season_start (min_hr) to season_end (max_hr) that each receiver
  # was operational
  tmp_p_op = suppressWarnings(tmp %>%
                              mutate(site = substr(receiver, 1, 2)) %>%
                              left_join(tmp %>%
                                          dplyr::filter(operational == T) %>%
                                          dplyr::group_by(receiver) %>%
                                          dplyr::summarise(receiver_end_hr = max(lubridate::floor_date(hr,
                                                                                                       unit = "hours"),
                                                                                 na.rm = T)) %>%
                                          ungroup()) %>%
                              filter(hr >= receiver_start_hr & hr <= receiver_end_hr) %>%
                              group_by(receiver) %>%
                              summarise(p_operational = sum(operational == T) / length(operational)) %>%
                              ungroup())

  tmp_list = list(operations_summ = tmp,
                  operations_plot = tmp_plot,
                  p_operational = tmp_p_op)
  return(tmp_list)

} # end summarise_timer_data()

