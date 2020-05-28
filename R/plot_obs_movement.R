#' @title Plot Movement of Observed Tags
#'
#' @description Plots movement through time based on first observations of tags at each receiver. Tags that were never detected after release are not included.
#'
#' @author Mike Ackerman and Kevin See
#'
#' @inheritParams get_file_nms
#'
#' @param ch_long A long form capture history, produced by the function \code{prep_capture_history()}
#'
#' @import ggplot2 dplyr forcats
#' @export
#' @return plot of when each tag was first detected at each receiver

plot_obs_movement = function(ch_long = NULL,
                             receiver_codes = NULL) {

  stopifnot(!is.null(ch_long))

  if(!is.null(receiver_codes)) {
    ch_long <- ch_long %>%
      mutate(loc = forcats::fct_relevel(loc,
                                        receiver_codes),
             loc = forcats::fct_drop(loc))
  }

  plot_df = ch_long %>%
    dplyr::arrange(tag_id, first_obs) %>%
    dplyr::mutate(loc_num = as.integer(loc)) %>%
    dplyr::group_by(tag_id) %>%
    dplyr::mutate(n_loc = n_distinct(loc)) %>%
    dplyr::filter(n_loc > 1) %>%
    dplyr::mutate(prev_loc = lag(loc_num),
                  nxt_loc = lead(loc_num)) %>%
    dplyr::filter(nxt_loc > loc_num | is.na(nxt_loc)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-(prev_loc:nxt_loc)) %>%
    dplyr::mutate_at(vars(loc),
                     list(forcats::fct_drop)) %>%
    dplyr::mutate(loc_num = as.integer(loc))

  move_p = plot_df %>%
    ggplot(aes(x = first_obs,
               y = loc_num,
               color = as.factor(tag_id))) +
    geom_line() +
    geom_point() +
    scale_y_continuous(name = "Receiver",
                       breaks = 1:max(as.integer(plot_df$loc)),
                       labels = levels(plot_df$loc)) +
    theme(legend.position = 'none') +
    labs(x = 'First Observation Date')

  return(move_p)

}
