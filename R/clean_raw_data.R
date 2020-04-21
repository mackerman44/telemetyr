#' @title Clean raw RT observations
#'
#' @description Clean up raw data, including formating dates, deleting invalid rows, etc.
#'
#' @author Kevin See and Mike Ackerman
#'
#' @param raw_data data.frame containing raw data, output from \code{read_txt_data()}
#' @param min_yr minimum acceptable year (e.g. 2018)
#' @param max_yr maximum acceptable year (e.g. 2018)
#' @param filter_valid should only valid records (\code{valid == 1}) be returned? Default is \code{TRUE}
#'
#' @import dplyr stringr lubridate
#' @export
#' @return a data frame containing a summary of the raw data

clean_raw_data = function(raw_data = NULL,
                          min_yr = 2017,
                          max_yr = NA,
                          filter_valid = T) {

  stopifnot(!is.null(raw_data))

  cat("Formating dates \n")

  raw_data = suppressWarnings(raw_data %>%
                                mutate(orig_date = date,
                                       # used this function to force the parsing, even when almost all the dates are incorrect format
                                       date = lubridate::parse_date_time2(date,
                                                                          orders = "dmy",
                                                                          tz = ""),
                                       date = as.Date(date)) %>%
                                # deal with dates like "01/01/00", or some that are from the 80's
                                rowwise() %>%
                                mutate(date = if_else(lubridate::year(date) < min_yr | lubridate::year(date) > max_yr | is.na(date),
                                                      as.Date(NA),
                                                      date)) %>%
                                ungroup() %>%
                                # mutate(date = if_else(lubridate::year(date) < 2017, as.Date(NA), date)) %>%
                                select(file_name, file, orig_date,
                                       everything()))

  cat("Removing files with all bad dates \n")

  # drop records from files with all bad dates
  bad_files = raw_data %>%
    group_by(file_name, file) %>%
    summarise(n_obs = n(),
              n_bad_date = sum(lubridate::year(date) < min_yr |
                                 lubridate::year(date) > max_yr,
                               na.rm = T),
              n_0_date = sum(is.na(date))) %>%
    ungroup() %>%
    filter(n_obs == n_0_date |
             n_obs == n_bad_date)

  raw_data = raw_data %>%
    anti_join(bad_files,
              by = c('file_name', 'file'))

  cat("Fixing incorrect dates \n")

  # fix incorrect dates when possible
  clean_data = raw_data %>%
    filter(!is.na(time)) %>%
    split(list(.$file_name)) %>%
    map_df(.f = function(x) {
             res = try(fix_bad_dates(x))
             if(class(res)[1] == 'try-error') {
               cat(paste('Something went wrong with file', unique(x$file), '.\n'))
               return(NULL)
             }
             return(res)
           })


  cat("Finishing up")

  clean_data = clean_data %>%
    arrange(date, time) %>%
    mutate(receiver = if_else(receiver == "000",
                              as.character(stringr::str_sub(file_name, 1, 3)),
                              receiver)) %>%
    mutate(date_time = lubridate::ymd_hms(paste(lubridate::year(date),
                                                lubridate::month(date),
                                                lubridate::day(date),
                                                time))) %>%
    select(orig_date, date, time, date_time,
           receiver,
           valid, frequency, tag_code, signal_strength)

  if(filter_valid) {
    clean_data = clean_data %>%
      filter(valid == 1)
  }

  return(clean_data)

}
