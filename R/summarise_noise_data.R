#' @title Summarize Receiver Noise Information
#'
#' @description Summarize receiver noise records in observation data
#'
#' @inheritParams prep_capture_history
#' @inheritParams get_file_nms
#'
#' @param operations_summary an optional summary of receiver operation times typically the \code{operations_summ} object
#' from \code{summarise_timer_data()}
#'
#' @import dplyr ggplot2 viridis
#' @export
#' @return summaries of noise information

summarise_noise_data = function(compress_df = NULL,
                                receiver_codes = NULL,
                                operations_summary = NULL) {

  cat("Parsing out noise records.\n")

  noise_df = compress_df %>%
    parse_code_ending(code_ending = "575$")

  # get list of all unique receivers in noise_df
  receiver_nms = sort(unique(noise_df$receiver))

  # if user provides a list of receiver codes
  if(!is.null(receiver_codes)) {
    receiver_nms = receiver_nms[receiver_nms %in% receiver_codes]
  }

  cat("Summarizing noise by receiver and channel.\n")

  # summarizing # of noise records by receiver and channel
  tmp = noise_df %>%
    filter(receiver %in% receiver_nms) %>%
    mutate(channel = as.numeric(substr(tag_id, 1, 1))) %>%
    group_by(receiver, channel) %>%
    summarise(n_noise = sum(n)) %>%
    spread(channel, n_noise) %>%
    ungroup()

  # if operations_summary is provided, convert number of noise observations to a rate
  if(!is.null(operations_summary)) {

    cat("Converting noise records to rates.\n")

    tmp = tmp %>%
      left_join(operations_summary %>%
                  group_by(receiver) %>%
                  summarise(hours = sum(operational)) %>%
                  ungroup()) %>%
      ungroup() %>%
      mutate_at(vars('1':'9'), funs(. / hours)) %>%
      mutate_at(2:10, round, 1)

  } # end operations_summary argument

  # change receiver to factor
  tmp = tmp %>%
    mutate(receiver = factor(receiver,
                             levels = receiver_nms)) %>%
    arrange(receiver)

  # add average nose across channels for each receiver
  tmp = tmp %>%
    mutate(mn_noise = tmp %>%
             select(`1`:`9`) %>%
             rowMeans() %>%
             round(1))

  # plot noise
  tmp_plot = tmp %>%
    select(receiver, `1`:`9`) %>%
    gather(channel,
           value,
           -receiver) %>%
    ggplot(aes(x = channel,
               y = fct_rev(receiver))) +
    geom_tile(aes(fill = value)) +
    scale_fill_viridis(option = "A",
                       direction = -1) +
    # scale_color_gradientn(colors = terrain.colors(10)) +
    # scale_fill_gradient2(low = "white",
    #                      high = "red") +
    theme_bw() +
    labs(x = "Channel",
         y = "Receiver",
         fill = "Noise Rate")

  tmp_list = list(noise_tbl = tmp,
                  noise_plot = tmp_plot)
  return(tmp_list)

} # end summarise_noise_data()
