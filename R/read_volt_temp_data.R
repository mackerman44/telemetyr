#' @title Read in Receiver Volt and Temperature Info from Tracker Software
#'
#' @description Reads in volt and temperature data from the Tracker software
#'
#' @author Mike Ackerman and Kevin See
#'
#' @inheritParams get_file_nms
#'
#' @import dplyr purrr readr
#' @export
#' @return a data frame containing receiver volt and temperature information

read_volt_temp_data = function(path = ".",
                               receiver_codes = NULL) {

  # list all of the files with name ending with double $$
  file_df = get_file_nms(path,
                         receiver_codes) %>%
    filter(grepl("\\$\\$", nm))

  vt_df = file_df %>%
    # how many files for each receiver?
    group_by(receiver) %>%
    mutate(file_num = 1:n(),
           n_files = n()) %>%
    ungroup() %>%
    # make a list of the file_name(s)
    split(list(.$file_name)) %>%
    map_df(.id = 'file_name',
           .f = function(x) {

             cat(paste('Reading volt/temp file', x$file_num, 'of', x$n_files, 'from receiver', x$receiver, '\n'))

             # read in each volt/temp file and assign col_names
             tmp = try(suppressWarnings(read_table2(paste(path, x$file_name, sep = "/"),
                                                    skip = 3,
                                                    col_types = c("ctnnniii"),
                                                    col_names = c("date",
                                                                  "time",
                                                                  "volt_avg",
                                                                  "volt_min",
                                                                  "volt_max",
                                                                  "temp_avg",
                                                                  "temp_min",
                                                                  "temp_max"))))

             # in nrow of tmp (a single txt file) is 0 or class(tmp) is error or first item is <END>...
             # return error message and nothing
             if(nrow(tmp) == 0 | class(tmp)[1] == "try-error") {
               cat(paste("Problem reading volt/temp info from receiver", x$receiver, ", file", x$nm, "\n"))
               return(NULL)
             }

             tmp = tmp %>%
               mutate(file = x$nm) %>%
               select(file, everything())

             return(tmp)

           })

  return(vt_df)

} # end read_volt_temp_data()
