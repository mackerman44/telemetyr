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

  # clean_data = raw_data %>%
  #   rename(orig_date = date) %>%
  #   filter(orig_date != "00/00/00") %>%
  #   mutate(date = lubridate::dmy(orig_date))
  #
  # if(sum(raw_data$date == "00/00/00") > 0) {
  #   clean_data = clean_data %>%
  #     bind_rows(raw_data %>%
  #                 rename(orig_date = date) %>%
  #                 filter(orig_date == "00/00/00") %>%
  #                 mutate(nums = stringr::str_extract(file, "[:digit:]+"),
  #                        jday = if_else(nchar(nums) == 3,
  #                                       nums,
  #                                       stringr::str_sub(nums, 3, 5)),
  #                        jday = as.integer(jday),
  #                        yr = if_else(nchar(nums) == 5,
  #                                     stringr::str_sub(nums, 1, 2),
  #                                     NA_character_),
  #                        file_date = lubridate::ymd(paste0("20", yr, "0101")) + lubridate::days(jday - 1)) %>%
  #                 mutate(date = file_date) %>%
  #                 select(orig_date, one_of(names(raw_data))))
  # }

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
