#' @title Get File Names
#'
#' @description Create a data frame of all downloaded files for a telemetry season
#'
#' @author Kevin See and Mike Ackerman
#'
#' @param path a path to the directory containing all of the files downloaded from radio telemetry receivers
#' for a study season and using the Tracker software
#' @param receiver_codes character vector of receiver codes to download files for.
#' The default is \code{NULL}, which will keep all receiver codes found in the path folder
#'
#' @import dplyr purrr
#' @export
#' @return a data frame of all files contained in \code{path}

get_file_nms = function(path = ".",
                        receiver_codes = NULL) {

  # list the folders in path, each with a receiver name
  receiver_nms = list.files(path)

  # keep only those folders with 3 characters i.e. get rid of any hidden or misc folders we don't want to read
  receiver_nms = receiver_nms[nchar(receiver_nms) == 3]

  if(!is.null(receiver_codes)) {
    receiver_nms = receiver_nms[receiver_nms %in% receiver_codes]
  }

  if(length(receiver_nms) == 0) stop("No folders were found in path")
  folders = as.list(receiver_nms)
  names(folders) = receiver_nms

  file_df = folders %>%
    map(.f = function(x) {
      list.files(paste(path, x[1], sep = "/"))
    }) %>%
    stack() %>%
    select(receiver = ind,
           nm = values) %>%
    tbl_df() %>%
    mutate(file_name = paste(receiver, nm, sep = "/"))

  return(file_df)

} # end get.file.nms
