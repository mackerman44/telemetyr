#' @title Read in raw .txt Format Tracker Data
#'
#' @description Reads in raw telemetry observation data stored in the .txt format from the Tracker software
#'
#' @author Kevin See
#'
#' @param path the directory containing the folders with observation data for each telemetry receiver for a study season
#'
#' @import dplyr purrr readr stringr lubridate
#' @export
#' @return a data frame containing all of the .txt data

read.txt.data = function(path = ".") {

  # df of the .txt files
  file_df = get.file.nms(path) %>%
    filter(grepl(".txt$", nm),
           !grepl("\\$", nm))

  file_df = file_df %>%
    # extract the julian day and yr information from nm and turn into a date
    mutate(nums = str_extract(nm, "[:digit:]+"),
           jday = if_else(nchar(nums) == 3,
                          nums,
                          str_sub(nums, 3, 5)),
           jday = as.integer(jday),
           yr = if_else(nchar(nums) == 5,
                        str_sub(nums, 1, 2),
                        NA_character_),
           date = ymd(paste0("20", yr, "0101")) + days(jday - 1)) %>%
    arrange(receiver, date)

  raw_file = file_df %>%
    split(list(.$file_name)) %>%
    map(.f = function(x) {
      # read in each .txt file and assign col_names
      tmp = try(read_table2(paste(path, x$file_name, sep = "/"),
                            skip = 3,
                            col_types = c("ctciiii"),
                            col_names = c("date",
                                          "time",
                                          "receiver",
                                          "valid",
                                          "frequency",
                                          "tag_code",
                                          "signal_strength")))

      # in nrow of tmp (a single txt file) is 0 or class(tmp) is error or first item is <END>...
      # return error message and nothing
      if(nrow(tmp) == 0 | class(tmp)[1] == "try-error" | tmp[1,1] == "<END>") {
        cat(paste("Problem reading in receiver", x$receiver, ", file", x$nm))
        return(NULL)
      }

      # tmp = tmp %>%
      #   filter(!is.na(valid)) %>%
      #   rename(orig_date = date) %>%
      #   filter(orig_date != "00/00/00") %>%
      #   mutate(date = dmy(orig_date)) %>%
      #   bind_rows(tmp %>%
      #               filter(!is.na(valid)) %>%
      #               rename(orig_date = date) %>%
      #               filter(orig_date == "00/00/00") %>%
      #               mutate(date = x$date)) %>%
      #   filter(!is.na(time)) %>%
      #   arrange(date, time) %>%
      #   mutate(receiver = if_else(receiver == "000",
      #                             as.character(x$receiver),
      #                             receiver)) %>%
      #   mutate(date_time = ymd_hms(paste(year(date), month(date), day(date), time)),
      #          tmp = paste0(frequency, if_else(nchar(tag_code) == 2,
      #                                          paste0(0, tag_code),
      #                                          as.character(tag_code))),
      #          tag_id = if_else(str_sub(tmp, -1) %in% c(0, 1, 2),
      #                           as.integer(paste0(str_sub(tmp, 1, -2), 0)),
      #                           if_else(str_sub(tmp, -1) %in% c(8, 9),
      #                                   as.integer(ceiling(as.numeric(tmp) / 10) * 10),
      #                                   as.integer(paste0(str_sub(tmp, 1, -2), 5))))) %>%
      #   select(receiver, valid, tag_id, date_time, frequency, signal_strength) %>%
      #   arrange(tag_id, date_time)

      return(tmp)

    })

  raw_file = raw_file[which(!sapply(raw_file, is.null))]
  raw_df = map_df(raw_file, .f = identity)
  return(raw_df)

}
