#' @title Parse Data Using a Tag List
#'
#' @description Filter a data frame to extract observations for a list tags defined by \code{tags}
#'
#' @author Kevin See and Mike Ackerman
#'
#' @param data_to_parse a data frame to be "parsed" or filtered containing a column \code{tag_id}
#' @param tags a vector of tag IDs that the user would like to extract data for
#'
#' @import dplyr
#' @export
#' @return a data frame containing only records with a \code{tag_id} \%in\% \code{tags}

parse_tag_list = function(data_to_parse = NULL,
                          tags = NULL) {

  stopifnot(!is.null(data_to_parse),
            !is.null(tags))

  tag_df = data_to_parse %>%
    filter(tag_id %in% tags)

  return(tag_df)

}
