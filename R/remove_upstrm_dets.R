#' @title Remove Upstream Detections
#'
#' @description Drops detections from an upstream receiver
#'
#' @author Kevin See and Mike Ackerman
#'
#' @param ch_long capture history in long format, with columns named \code{tag_id}, \code{loc}, \code{first_obs}, \code{last_obs} and \code{n}. The \code{loc} column must be a factor with the levels corresponding to site codes in order from upstream to downstream
#'
#' @import dplyr
#' @export
#' @return a data frame containing all records in the .txt receiver downloads in \code{path}

remove_upstrm_dets <- function(ch_long) {

  y = ch_long %>%
    mutate(loc_num = as.integer(loc),
           last_loc = lag(loc_num)) %>%
    mutate(drop_loc = if_else(last_loc <= loc_num | is.na(last_loc),
                              F, T))

  if(sum(y$drop_loc) == 0) {
    y %>%
      select(-loc_num, -last_loc, -drop_loc) %>%
      return()
  } else {
    y %>%
      filter(!drop_loc) %>%
      select(-loc_num, -last_loc, - drop_loc) %>%
      remove_upstrm_dets()
  }

}
