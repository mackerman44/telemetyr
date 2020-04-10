#' @title Read in .csv Format Tracker Data
#'
#' @description Reads in telemetry observation data stored in the .csv format from the Tracker software
#'
#' @author Kevin See
#'
#' @inheritParams get.file.nms
#'
#' @import dplyr purrr readr stringr lubridate
#' @export
#' @return a data frame containing all of the .csv data

read.csv.data = function(path = ".",
                         receiver_codes = NULL) {

  # df of the .csv files
  file_df = get.file.nms(path,
                         receiver_codes) %>%
    filter(grepl(".csv$", nm))

  raw_csv = file_df %>%
    # how many files for each receiver?
    group_by(receiver) %>%
    mutate(file_num = 1:n(),
           n_files = n()) %>%
    ungroup() %>%
    # make a list of the file_name(s)
    split(list(.$file_name)) %>%
    map_df(.id = NULL,
           .f = function(x) {
             # read in each .csv file and assign col_names
             cat(paste('Reading file', x$file_num, 'of', x$n_files, 'from receiver', x$receiver, '\n'))

             tmp = try(read_csv(paste(path, x$file_name, sep = "/"),
                                col_types = c("cccccc"),
                                col_names = c("receiver",
                                              "valid",
                                              "tag_id",
                                              "start",
                                              "end",
                                              "n")))

             # if n of columns in tmp (a single csv file) is 0 or if class of tmp is error, return nothing
             if(ncol(tmp) == 0 | class(tmp)[1] == 'try-error') return(NULL)

             tmp = tmp %>%
               # check and fix dates and times in start and end
               mutate_at(vars(start, end),
                         list(~ if_else(str_count(., '\\:') == 1,
                                        paste(., '00', sep = ':'),
                                        .))) %>%
               # standardize date time information
               mutate_at(vars(start, end),
                         list(lubridate::mdy_hms)) %>%
               # make valid, tag_id, and n integers
               mutate_at(vars(valid, tag_id, n),
                         list(as.integer))

             return(tmp)

           })

  return(raw_csv)

} # end read.csv.data()
