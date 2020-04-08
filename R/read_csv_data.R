#' @title Read in .csv Format Tracker Data
#'
#' @description Reads in telemetry observation data stored in the .csv format from the Tracker software
#'
#' @author Kevin See
#'
#' @param path the directory containing the folders with observation data for each telemetry receiver for a study season
#'
#' @import dplyr purr readr stringr lubridate
#' @export
#' @return a data frame containing all of the .csv data

read.csv.data = function(path = ".") {

  # list all of the .csv files
  file_df = get.file.nms(path) %>%
    filter(grepl(".csv$", nm))

  file_df %>%
    select(file_name) %>%
    as.matrix() %>%
    as.character() %>%
    as.list() %>%
    map_df(.f = function(x) {
      tmp = try(read_csv(paste(path, x[1], sep = "/"),
                         col_types = c("cccccc"),
                         col_names = c("receiver",
                                       "valid",
                                       "tag_id",
                                       "start",
                                       "end",
                                       "n")))

      if(ncol(tmp) == 0 | class(tmp)[1] == 'try-error') return(NULL)

      tmp = tmp %>%
        mutate_at(vars(start, end),
                  list(~ if_else(str_count(., '\\:') == 1,
                                 paste(., '00', sep = ':'),
                                 .))) %>%
        mutate_at(vars(start, end),
                  list(mdy_hms)) %>%
        mutate_at(vars(valid, tag_id, n),
                  list(as.integer))

      return(tmp)

    })
} # end read.csv.data()
