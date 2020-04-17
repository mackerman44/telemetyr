#' @title Clean raw RT observations
#'
#' @description Clean up raw data, including formating dates, deleting invalid rows, etc.
#'
#' @author Kevin See and Mike Ackerman
#'
#' @param raw_data data.frame containing raw data, output from \code{read_txt_data()}
#' @param filter_valid should only valid records (\code{valid == 1}) be returned? Default is \code{TRUE}
#'
#' @import dplyr stringr lubridate
#' @export
#' @return a data frame containing a summary of the raw data

clean_raw_data = function(raw_data = NULL,
                          filter_valid = T) {

  stopifnot(!is.null(raw_data))

  # drop records from files with all bad dates
  bad_files = raw_data %>%
    mutate(date = lubridate::parse_date_time2(date,
                                              orders = "dmy",
                                              tz = "")) %>%
    group_by(file_name, file) %>%
    summarise(n_obs = n(),
              n_0_date = sum(is.na(date))) %>%
    ungroup() %>%
    filter(n_obs == n_0_date)

  raw_data = raw_data %>%
    anti_join(bad_files)

  clean_data = raw_data %>%
    filter(!is.na(time)) %>%
    split(list(.$file_name)) %>%
    map_df(.f = function(x) {
             res = try(fix_bad_dates(x))
             if(class(res)[1] == 'try-error') {
               return(NULL)
             }
             return(res)
           })

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
