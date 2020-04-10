#' @title Parse timer tag data
#'
#' @description Filter the summarised data to extract observations related to timer tags
#'
#' @author Kevin See and Mike Ackerman
#'
#' @param summ_data data.frame containing summarised data, output from \code{summarise_txt_data()}
#' @param noise_code code for the tag ID that distinguishes the timer tag data
#'
#' @import dplyr
#' @export
#' @return a data.frame containing a summary of the timer tag data

parse_timer = function(summ_data = NULL,
                       timer_code = "575$") {

  stopifnot(!is.null(summ_data))

  timer_df = summ_data %>%
    filter(grepl(timer_code, tag_id))

  return(timer_df)
}
