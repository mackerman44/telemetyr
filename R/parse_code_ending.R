#' @title Parse tags by tag code ending
#'
#' @description Filter the summarised data to extract observations related to timer tags
#'
#' @author Kevin See and Mike Ackerman
#'
#' @param summ_data data.frame containing summarised data, output from \code{summarise_txt_data()}
#' @param code_ending the ending of the tag code to filter for. Some examples include: "575$" for timer tags, "995$" for noise tags or "525$" for sentinel tags
#'
#' @import dplyr
#' @export
#' @return a data.frame containing a only records with the desired code ending

parse_code_ending = function(summ_data = NULL,
                             code_ending = NULL) {

  stopifnot(!is.null(summ_data),
            !is.null(code_ending))

  timer_df = summ_data %>%
    filter(grepl(code_ending, tag_id))

  return(timer_df)
}
