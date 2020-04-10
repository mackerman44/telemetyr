#' @title Summarize Receiver Noise
#'
#' @description Summarise receiver noise in observation data
#'
#' @author Mike Ackerman and Kevin See
#'
#' @param noise_data the data frame containing the noise data e.g., from \code{parse_noise()}
#' @param receiver_codes character vector of receiver codes to query for. Default is \code{NULL} which will keep all receiver codes found in the path folder
#'
#' @import dplyr
#' @export
#' @return a data frame summarising the noise information

summarise_noise_data = function(noise_data = NULL,
                                receiver_codes = NULL,
                                operations_summary = NULL) {

  # get list of all unique receivers in noise_data
  receiver_nms = sort(unique(noise_data$receiver))

  # if user provides a list of receiver codes
  if(!is.null(receiver_codes)) {
    receiver_nms = receiver_nms[receiver_nms %in% receiver_codes]
  }

  noise_summ = noise_data %>%
    filter(receiver %in% receiver_nms) %>%
    mutate(channel = as.numeric(substr(tag_id, 1, 1))) %>%
    group_by(receiver, channel) %>%
    summarise(n_noise = sum(n)) %>%
    spread(channel, n_noise)

  if(!is.null(operations_summary)) {

    noise_summ = noise_summ %>%
      left_join(operations_summary %>%
                  group_by(receiver) %>%
                  summarise(hours = sum(operational)) %>%
                  ungroup()) %>%
      ungroup() %>%
      mutate_at(vars('1':'9'), funs(. / hours)) %>%
      mutate_at(2:10, round, 1)

  } # end operations_summary argument

  noise_summ = noise_summ %>%
    ungroup() %>%
    mutate(receiver = factor(receiver,
                             levels = receiver_nms)) %>%
    arrange(receiver)

  return(noise_summ)

} # end summarise_noise_data()
