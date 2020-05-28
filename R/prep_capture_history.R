#' @title Prepare Capture Histories
#'
#' @description Generates a data frame containing all the necessary information for capture histories of tagged fish
#'
#' @author Kevin See and Mike Ackerman
#'
#' @inheritParams compress_txt_data
#'
#' @param compress_df compressed observational data. Either the output of \code{read_csv_data()} or \code{read_txt_data()} and \code{compress_raw_data()}.
#' @param tag_data dataframe with metadata for each tag, including columns named \code{tag_id}, \code{tag_purpose} (with some tags having "\code{fish}" in this column), \code{release_site} (which is one of the sites in the \code{rec_site} input) and \code{release_time}.
#' @param n_obs_valid minumum number of observations made at a location to be considered valid.
#' @param rec_site data.frame with 2 columns, \code{site} and \code{receiver}, with sites and and receivers as factors with levels in order.
#' @param delete_upstream should detections that indicate upstream movement be deleted?
#' @param location should detections be grouped by site or receiver?
#' @param output_format return either wide or long format, or both plus tag metadata
#'
#' @import dplyr tidyr readxl lubridate
#' @importFrom janitor excel_numeric_to_date
#' @importFrom magrittr %<>%
#' @export
#' @return a list containing a capture history in wide format, one in long format, and metadata for each tag

prep_capture_history = function(compress_df = NULL,
                                tag_data = NULL,
                                n_obs_valid = 3,
                                rec_site = NULL,
                                delete_upstream = T,
                                location = c('site', 'receiver'),
                                output_format = c('all', 'wide', 'long'),
                                assign_week = T,
                                week_base = "0901",
                                append_week = c('first', 'last')) {

  stopifnot(!is.null(compress_df),
            !is.null(tag_data),
            !is.null(rec_site))

  location = match.arg(location)
  output_format = match.arg(output_format)
  append_week = match.arg(append_week)

  # pull out tags that were put in fish
  fish_tags = tag_data %>%
    filter(tag_purpose == 'fish') %>%
    pull(tag_id)

  fish_df = parse_tag_list(compress_df,
                           tags = fish_tags) %>%
    mutate(site = str_sub(receiver, 1, 2)) %>%
    select(site, everything()) %>%
    # filter out observations prior to release date
    left_join(tag_data %>%
                filter(tag_purpose == 'fish') %>%
                select(tag_id, release_time)) %>%
    filter(start >= release_time) %>%
    select(-release_time) %>%
    arrange(tag_id, start)

  # only keep observations with a minimum number of detections within the maximum minute period
  fish_df %<>%
    filter(n >= n_obs_valid)

  # check to see if all receiver names left in data are included in the input argument
  obs_rec = fish_df %>%
    pull(receiver) %>%
    unique()

  if(sum(!obs_rec %in% rec_site$receiver) > 0) {
    cat(paste(paste(obs_rec[!obs_rec %in% rec_site$receiver], collapse = ", "), "are not in the rec_site dataframe, and will be dropped.\n"))
  }

  # turn receivers and sites into factors
  fish_df %<>%
    inner_join(rec_site,
               by = c('site', 'receiver')) %>%
    mutate(site = factor(site,
                         levels = levels(rec_site$site)),
           receiver = factor(receiver,
                             levels = levels(rec_site$receiver)))

  if(location == "receiver") {
    # combine detections by receiver
    first_last = fish_df %>%
      full_join(fish_df %>%
                  mutate(nxt_tag = lead(tag_id),
                         nxt_rec = lead(receiver),
                         nxt_site = lead(site)) %>%
                  mutate(new_grp = if_else(tag_id != nxt_tag |
                                             receiver != nxt_rec,
                                           T, F)) %>%
                  filter(new_grp) %>%
                  mutate(grp = 1:n())) %>%
      tidyr::fill(grp, .direction = 'up')

    if("week" %in% names(first_last)) {
      first_last %<>%
        group_by(tag_id, site, receiver, grp) %>%
        summarise(first_obs = min(start),
                  last_obs = max(end),
                  n = sum(n),
                  week = min(week)) %>%
        ungroup() %>%
        select(-grp) %>%
        rename(loc = site) %>%
        arrange(tag_id, first_obs)
    } else {
      first_last %<>%
        group_by(tag_id, site, receiver, grp) %>%
        summarise(first_obs = min(start),
                  last_obs = max(end),
                  n = sum(n)) %>%
        ungroup() %>%
        select(-grp) %>%
        rename(loc = site) %>%
        arrange(tag_id, first_obs)
    }

    # add release site
    first_last %<>%
      bind_rows(tag_data %>%
                  filter(release_site != "NA" & !is.na(release_site)) %>%
                  select(tag_id, loc = release_site,
                         first_obs = release_time) %>%
                  mutate(last_obs = first_obs,
                         n = 1)) %>%
      mutate(loc = factor(loc,
                          levels = levels(rec_site$receiver))) %>%
      arrange(tag_id, first_obs)

    # remove detections that indicate upstream movement?
    if(delete_upstream) {
      first_last %<>%
        group_by(tag_id) %>%
        mutate(loc_num = as.integer(site),
               next_loc = lead(loc_num)) %>%
        ungroup() %>%
        filter(next_loc >= loc_num | is.na(next_loc))
    }

  }

  if(location == 'site') {
    # combine detections by site
    first_last = fish_df %>%
      full_join(fish_df %>%
                  mutate(nxt_tag = lead(tag_id),
                         nxt_rec = lead(receiver),
                         nxt_site = lead(site)) %>%
                  mutate(new_grp = if_else(tag_id != nxt_tag |
                                             site != nxt_site,
                                           T, F)) %>%
                  filter(new_grp) %>%
                  mutate(grp = 1:n())) %>%
      tidyr::fill(grp, .direction = 'up')

    if("week" %in% names(first_last)) {
      first_last %<>%
        group_by(tag_id, site, grp) %>%
        summarise(first_obs = min(start),
                  last_obs = max(end),
                  n = sum(n),
                  week = min(week)) %>%
        ungroup() %>%
        select(-grp) %>%
        rename(loc = site) %>%
        arrange(tag_id, first_obs)
    } else {
      first_last %<>%
        group_by(tag_id, site, grp) %>%
        summarise(first_obs = min(start),
                  last_obs = max(end),
                  n = sum(n)) %>%
        ungroup() %>%
        select(-grp) %>%
        rename(loc = site) %>%
        arrange(tag_id, first_obs)
    }

    # add release site
    first_last %<>%
      bind_rows(tag_data %>%
                  filter(release_site != "NA" & !is.na(release_site)) %>%
                  select(tag_id, loc = release_site,
                         first_obs = release_time) %>%
                  mutate(last_obs = first_obs,
                         n = 1)) %>%
      mutate(loc = factor(loc,
                          levels = levels(rec_site$site))) %>%
      arrange(tag_id, first_obs)

    # remove detections that indicate upstream movement?
    if(delete_upstream) {
      first_last %<>%
        group_by(tag_id) %>%
        mutate(loc_num = as.integer(loc),
               next_loc = lead(loc_num)) %>%
        ungroup() %>%
        filter(next_loc >= loc_num | is.na(next_loc))
    }
  }

  # wide format
  cap_hist_wide = first_last %>%
    select(tag_id, loc) %>%
    distinct() %>%
    mutate(seen = 1) %>%
    spread(loc, seen,
           fill = 0) %>%
    unite(cap_hist,
          -tag_id,
          sep = "",
          remove = F)

  # long format
  cap_hist_long = first_last %>%
    select(-loc_num, -next_loc)

  if(assign_week) {
    start_date = lubridate::ymd(paste(lubridate::year(min(cap_hist_long$first_obs, na.rm = T)), week_base))
    if(append_week == 'first') {
      cap_hist_long %<>%
        mutate(week = difftime(first_obs, start_date, units = 'weeks'),
               week = as.numeric(week),
               week = floor(week) + 1)
    }
    if(append_week == 'last') {
      cap_hist_long %<>%
        mutate(week = difftime(last_obs, start_date, units = 'weeks'),
               week = as.numeric(week),
               week = floor(week) + 1)
    }
  }

  # what to return?
  if(output_format == 'wide') {
    return(cap_hist_wide)
  }


  if(output_format == 'long') {
    return(cap_hist_long)
  }

  if(output_format == 'all') {
    list(ch_wide = cap_hist_wide,
         ch_long = cap_hist_long,
         tag_df = tag_data %>%
           filter(tag_id %in% fish_tags)) %>%
      return()
  }

}
