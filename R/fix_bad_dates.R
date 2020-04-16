#' @title Fix bad dates
#'
#' @description This function corrects those dates at the beginning of a raw text file that are incorrect because the timer has not been reset
#'
#' @author Kevin See and Mike Ackerman
#'
#' @param detect_df data.frame of raw text records, as returned by \code{read_txt_data()}.
#'
#' @import dplyr lubridate
#' @export
#' @return a data frame matching the original, with corrected dates and a new column, \code{orig_date}, showing the original date (character) from the file.

fix_bad_dates = function(detect_df = NULL) {

  stopifnot(!is.null(detect_df))

  detect_fix = suppressWarnings(detect_df %>%
                                  mutate(orig_date = date,
                                         date = dmy(date)) %>%
                                  # deal with dates like "01/01/00", or some that are from the 80's
                                  mutate(date = if_else(year(date) < 2017, NA, date)) %>%
                                  select(file_name, file, orig_date,
                                         everything()) %>%
                                  mutate(hr = hour(time),
                                         lead_hr = lead(hr),
                                         lead_date = lead(date),
                                         lead_date_prev = lead_date - ddays(1)))

  if(sum(is.na(detect_fix$date)) > 0) {

    for(i in rev(seq_along(which(is.na(detect_fix$date))))) {
      detect_fix$date[i] = if_else(detect_fix$lead_hr[i] < detect_fix$hr[i],
                                   detect_fix$lead_date_prev[i],
                                   detect_fix$lead_date[i])
      detect_fix$lead_date[i-1] = detect_fix$date[i]
      detect_fix$lead_date_prev[i-1] = detect_fix$lead_date[i-1] - ddays(1)
    }

  }

  detect_fix = detect_fix %>%
    select(-(hr:lead_date_prev))

  return(detect_fix)
}
