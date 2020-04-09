#' @title Read in Receiver On/Off Times from Tracker Software
#'
#' @description Reads in information on receiver on/off times from the Tracker software
#'
#' @author Mike Ackerman and Kevin See
#'
#' @param path the directory containing the folders with observation data for each telemetry receiver for a study season
#'
#' @import dplyr readr
#' @export
#' @return a data frame containing receiver on/off information

read.on.off.data = function(path = ".") {

  # list all of the files with name ending with a single $
  file_df = get.file.nms(path) %>%
    filter(grepl("\\$", file_name),
           !grepl("\\$\\$", file_name)) %>%
    select(receiver, file_name)

  on_off_df = NULL
  for(i in 1:nrow(file_df)) {
    on_off_df = on_off_df %>%
      bind_rows(readr::read_table(paste(path, file_df$file_name[i], sep = "/"),
                                  col_names = F) %>%
                  mutate(receiver = file_df$receiver[i]))
  }

  on_off_df = on_off_df %>%
    dplyr::select(receiver,
                  on_off = X1,
                  date = X2,
                  time = X3)

  return(on_off_df)

} # end read.on.off.data()
