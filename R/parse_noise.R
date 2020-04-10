#' @title Parse noise data
#'
#' @description Filter the summarised data to extract observations related to noise
#'
#' @author Kevin See and Mike Ackerman
#'
#' @param summ_data data.frame containing summarised data, output from \code{summarise_txt_data()}
#' @param noise_code code for the tag ID that distinguishes the noise data
#'
#' @import dplyr
#' @export
#' @return a data.frame containing a summary of the noise data

parse_noise = function(summ_data = NULL,
                       noise_code = "995$") {

  stopifnot(!is.null(summ_data))

  noise_df = summ_data %>%
    filter(grepl(noise_code, tag_id))

  return(noise_df)
}
