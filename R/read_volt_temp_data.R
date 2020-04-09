#' @title Read in Receiver Volt and Temperature Info from Tracker Software
#'
#' @description Reads in volt and temperature receiver data from Tracker software
#'
#' @author Mike Ackerman and Kevin see
#'
#' @param path the directory containing the folders with observation data for each telemetry receiver for a study season
#'
#' @import dplyr readr
#' @export
#' @return a data frame containing receiver volt and temperature information

read.volt.temp.data = function(path = ".") {

  # list all of the files with name ending with double $$
  file_df = get.file.nms(path) %>%
    filter(grepl("\\$\\$", file_name)) %>%
    select(receiver, file_name)

  volt_temp_df = NULL
  for(i in 1:nrow(file_df)) {

    volt_temp_df = volt_temp_df %>%
      bind_rows(readr::read_table(paste(path, file_df$file_name[i], sep = "/"),
                                  col_names = T,
                                  skip = 1) %>%
                  dplyr::slice(-1) %>%
                  dplyr::rename(date = DATE,
                                time = TIME,
                                volt_avg = AVG,
                                volt_min = MIN,
                                volt_max = MAX,
                                temp_avg = AVG_1,
                                temp_min = MIN_1,
                                temp_max = MAX_1) %>%
                  dplyr::mutate(receiver = file_df$receiver[i]) %>%
                  dplyr::select(receiver, everything()))

  }
  return(volt_temp_df)
} # end read.volt.temp.data()
