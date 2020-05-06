#' @title Fix bad dates
#'
#' @description This function corrects those dates at the beginning of a raw text file that are incorrect because the timer has not been reset
#'
#' @author Kevin See and Mike Ackerman
#'
#' @param raw_data data.frame of raw text records, as returned by \code{read_txt_data()}.
#'
#' @import dplyr lubridate
#' @export
#' @return a data frame matching the original, with corrected dates and a new column, \code{orig_date}, showing the original date (character) from the file.

fix_bad_dates = function(raw_data = NULL) {

  stopifnot(!is.null(detect_df))

  detect_fix = suppressWarnings(detect_df %>%
                                  mutate(hr = lubridate::hour(time),
                                         lead_hr = lead(hr),
                                         lead_date = lead(date),
                                         lead_date_prev = lead_date - lubridate::ddays(1)))

  # how many times has clock crossed midnight when dates are missing?
  n_new_days = detect_fix %>%
    filter(is.na(date),
           lead_hr < hr) %>%
    nrow()

  if(n_new_days <= 1) {
    detect_fix = detect_fix %>%
      mutate(new_day = if_else(lead_hr >= hr | is.na(lead_hr),
                               F, T)) %>%
      mutate(date = if_else(new_day,
                            lead_date_prev,
                            if_else(is.na(lead_date),
                                    date,
                                    lead_date))) %>%
      tidyr::fill(date, .direction = "up") %>%
      select(-new_day)
  }

  detect_fix = detect_fix %>%
    select(-(hr:lead_date_prev))

  return(detect_fix)
}
