#' @title Read in Receiver On/Off Times from Tracker Software
#'
#' @description Reads in information on receiver on/off times from the Tracker software
#'
#' @author Mike Ackerman and Kevin See
#'
#' @inheritParams get_file_nms
#'
#' @import dplyr readr
#' @export
#' @return a data frame containing receiver on/off information

read_on_off_data = function(path = ".",
                            receiver_codes = NULL) {

  # list all of the files with name ending with a single $
  file_df = get_file_nms(path,
                         receiver_codes) %>%
    filter(grepl("\\$", nm),
           !grepl("\\$\\$", nm))

  on_off_df = file_df %>%
    # how many files for each receiver
    group_by(receiver) %>%
    mutate(file_num = 1:n(),
           n_files = n()) %>%
    ungroup() %>%
    # make a list of the file_name(s)
    split(list(.$file_name)) %>%
    map_df(.id = NULL,
           .f = function(x) {

             # console message
             cat(paste('Reading on/off file', x$file_num, 'of', x$n_files, 'from receiver', x$receiver, '\n'))

             # read in each .txt file and assign col_names
             tmp = try(suppressWarnings(read_table2(paste(path, x$file_name, sep = "/"),
                                                    skip = 0,
                                                    col_types = c("cct"),
                                                    col_names = c("on_off",
                                                                  "date",
                                                                  "time"))))

             # if n of columns in tmp (a single csv file) is 0 or if class of tmp is error, return nothing
             if(ncol(tmp) == 0 | class(tmp)[1] == 'try-error') return(NULL)

             tmp = tmp %>%
               mutate(receiver = x$receiver)

             return(tmp)

           })

  on_off_df = on_off_df %>%
    filter(!is.na(time))

  return(on_off_df)

} # end read.on.off.data()
