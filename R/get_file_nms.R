#' @title Get File Names
#'
#' @description Generates a data frame containing all the files within each telemetry receiver's folder
#'
#' @author Kevin See and Mike Ackerman
#'
#' @param path the directory containing the folders with observation data for each telemetry receiver for a study season
#' @param receiver_codes character vector of receiver codes to query for. Default is \code{NULL} which will keep all receiver codes found in the path folder
#'
#' @import dplyr purrr
#' @export
#' @return a data frame containing all the files within each telemetry receiver's folder

get.file.nms = function(path = ".",
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
