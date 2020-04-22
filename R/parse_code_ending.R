#' @title Parse Data Using a Code "Ending"
#'
#' @description Filter a data frame to extract observations for tag codes ending in \code{code_ending};
#' useful for extracting noise, timer, etc information.
#'
#' @author Kevin See and Mike Ackerman
#'
#' @inheritParams parse_tag_list
#'
#' @param code_ending the ending of the tag code to filter for. Some examples include: "575$" for timer tags,
#' "995$" for noise tags, or "525$" for sentinel tags
#'
#' @import dplyr
#' @export
#' @return a data frame containing only records for the desired \code{code_ending}

parse_code_ending = function(data_to_parse = NULL,
                             code_ending = NULL) {

  stopifnot(!is.null(data_to_parse),
            !is.null(code_ending))

  timer_df = data_to_parse %>%
    filter(grepl(code_ending, tag_id))

  return(timer_df)

}
