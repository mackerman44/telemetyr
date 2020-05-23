#' @title Prepare Bayesian CJS Model
#'
#' @description Prepares a list of data to be fed into a Bayesian CJS model
#'
#' @author Kevin See and Mike Ackerman
#'
#' @param cap_hist_wide wide capture history, one row per tag, one column per detection site. Includes columns named \code{tag_id} and \code{cap_hist}. Part of the output from \code{prep_capture_history}.
#' @param tag_meta metadata for each tag, including a column named \code{tag_id}.
#' @param drop_col_nm name of column in \code{tag_meta} to be used to filter out any particular tags.
#' @param drop_values character vector of values in \code{drop_col_nm} column of \code{tag_meta} that should be excluded from the CJS model.
#'
#' @import dplyr
#' @importFrom tidyr nest
#' @importFrom purrr map_dbl
#' @export
#' @return list to be fed into JAGS model

prep_jags_cjs = function(cap_hist_wide = NULL,
                         tag_meta = NULL,
                         drop_col_nm = 'duty_cycle',
                         drop_values = c('batch_2', 'batch_3')) {

  stopifnot(!is.null(cap_hist_wide),
            !is.null(tag_meta))

  if(!is.null(drop_col_nm)) {

    tag_df = tag_meta
    names(tag_df)[grep(drop_col_nm, names(tag_df))] = "drop_col"

    # drop batch 2 and batch 3 tags that were never detected after release
    drop_tags = tag_df %>%
      dplyr::filter(drop_col %in% drop_values) %>%
      dplyr::select(tag_id) %>%
      dplyr::distinct() %>%
      dplyr::left_join(cap_hist_wide) %>%
      dplyr::group_by(tag_id, cap_hist) %>%
      tidyr::nest() %>%
      dplyr::mutate(n_dets = purrr::map_dbl(data,
                                            .f = rowSums)) %>%
      dplyr::filter(n_dets == 1) %>%
      dplyr::pull(tag_id)

    if(length(drop_tags) == 0) {
      y = cap_hist_wide %>%
        dplyr::select(-tag_id, -cap_hist) %>%
        as.matrix()
    } else {
      y = cap_hist_wide %>%
        dplyr::filter(! tag_id %in% drop_tags) %>%
        dplyr::select(-tag_id, -cap_hist) %>%
        as.matrix()
    }
  }

  if(is.null(drop_col_nm)) {
    y = cap_hist_wide %>%
      dplyr::select(-tag_id, -cap_hist) %>%
      as.matrix()
  }


  jags_data = list(
    N = nrow(y),
    J = ncol(y),
    y = y,
    z = known_alive(y),
    f = first_alive(y)
  )

  return(jags_data)

}
