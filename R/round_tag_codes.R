#' @title Round Tag Codes
#'
#' @description Round the tag IDs from a receiver to the nearest 5 or 0 to better match possible tag codes
#'
#' @author Kevin See and Mike Ackerman
#'
#' @param data_df any data frame with a column named \code{tag_code}
#' @param round_to the integer that the user would like to round \code{tag_code} to. Currently supports
#' either 5 or 10. If \code{round_to = 5}, will round to the nearest 5. If \code{round_to = 10}, will round
#' to the nearest 0.
#'
#' @import dplyr stringr tidyr
#' @export
#' @return a data frame containing all the original columns, plus an additional column \code{tag_id}
#' containing the fixed tag IDs

round_tag_codes = function(data_df = NULL,
                           round_to = 5) {

  stopifnot(!is.null(data_df))

  if(!round_to %in% c(10, 5)) {
    stop('Currently only designed to round to 10 or 5.')
  }

  prep_data = data_df %>%
    mutate(tag_code = stringr::str_pad(tag_code,
                                       width = 3,
                                       pad = '0'),
           other_digits = stringr::str_sub(tag_code, 1, -2),
           last_digit = stringr::str_sub(tag_code, start = -1))

  if(round_to == 5) {
    rnd_data = prep_data %>%
      mutate(last_round = if_else(last_digit %in% c(0, 1, 2),
                                  0,
                                  if_else(last_digit %in% c(3, 4, 5, 6, 7),
                                          5,
                                          if_else(last_digit %in% c(8, 9),
                                                  0,
                                                  NA_real_)))) %>%
      unite(tag_id, frequency, other_digits, last_round,
            sep = '',
            remove = F) %>%
      mutate_at(vars(tag_id),
                list(as.numeric)) %>%
      mutate(tag_id = if_else(last_digit %in% c(8, 9),
                              tag_id + 10,
                              tag_id)) %>%
      select(-other_digits,
             -last_digit,
             -last_round)
  }

  if(round_to == 10) {
    rnd_data = prep_data %>%
      unite(tag_id, frequency, tag_code,
            sep = '',
            remove = F) %>%
      mutate_at(vars(tag_id),
                list(as.numeric)) %>%
      mutate(tag_id = janitor::round_half_up(tag_id / 10) * 10) %>%
      select(-other_digits,
             -last_digit)
  }

  return(rnd_data)
}
