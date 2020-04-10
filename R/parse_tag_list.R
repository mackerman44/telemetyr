#' @title Parse tag data
#'
#' @description Filter the summarised data to extract observations related to tags, defined in a tag list
#'
#' @author Kevin See and Mike Ackerman
#'
#' @param summ_data data.frame containing summarised data, output from \code{summarise_txt_data()}
#' @param tags vector of tag IDs that the user would like to extract data for
#'
#' @import dplyr
#' @export
#' @return a data.frame containing a summary of the tag data for the listed tags

parse_tag_list = function(summ_data = NULL,
                          tags = NULL) {

  stopifnot(!is.null(summ_data),
            !is.null(tags))

  tag_df = summ_data %>%
    filter(tag_id %in% tags)

  return(tag_df)
}
