#' @title Prepare Capture Histories
#'
#' @description Generates a data frame containing all the necessary information for capture histories of tagged fish
#'
#' @author Kevin See and Mike Ackerman
#'
#' @param compress_df compressed observational data. Either the output of \code{read_csv_data()} or \code{read_txt_data()} and \code{compress_raw_data()}.
#' @param tag_data_path file path to the file containing all the metadata for all tags.
#' @param n_obs_valid minumum number of observations made at a location to be considered valid.
#' @param receiver_nms character vector of receiver names, in order, to be used in the capture histories.
#' @param delete_upstream should detections that indicate upstream movement be deleted?
#' @param output_format Either wide or long format
#'
#' @import dplyr tidyr readxl lubridate
#' @export
#' @return a dataframe to be used as capture histories

prep_capture_history = function(compress_df = NULL,
                                tag_data_path = 'data/prepped/tag_release/lemhi_winter_telemetry_tag_info.xlsx',
                                n_obs_valid = 3,
                                # remove_sites = c('MT','AC','TT'),
                                receiver_nms,
                                delete_upstream = T,
                                output_format = c('wide', 'long')) {

  stopifnot(!is.null(compress_df),
            !is.null(tag_data_path))

  output_format = match.arg(output_format)

  # what season?
  yr_label = paste(lubridate::year(min(compress_df$start, na.rm = T)),
                   lubridate::year(max(compress_df$end, na.rm = T)),
                   sep = "_") %>%
    str_remove_all("20")

  # get data about each released tag, including code
  tag_df = read_excel(tag_data_path) %>%
    filter(season == yr_label) %>%
    mutate(tag_id = str_extract(radio_tag_id, "[:digit:]*"),
           tag_id = as.numeric(tag_id)) %>%
    mutate_at(vars(activation_time, release_time),
              list(as.numeric)) %>%
    mutate_at(vars(activation_time, release_time),
              list(excel_numeric_to_date),
              include_time = T)

  # pull out tags that were put in fish
  fish_tags = tag_df %>%
    filter(tag_purpose == 'fish') %>%
    pull(tag_id)

  fish_df = parse_tag_list(compress_df,
                           tags = fish_tags) %>%
    mutate(site = str_sub(receiver, 1, 2)) %>%
    select(site, everything()) %>%
    # filter out observations prior to release date
    left_join(tag_df %>%
                filter(tag_purpose == 'fish') %>%
                select(tag_id, release_time)) %>%
    filter(start >= release_time) %>%
    select(-release_time)

  # remove aberrent sites (mobile, activation, test)
  fish_df %<>%
    # filter(!site %in% remove_sites) %>%
    arrange(tag_id, start)

  # only keep observations with a minimum number of detections within the maximum minute period
  fish_df %<>%
    filter(n >= n_obs_valid)

  # check to see if all receiver names left in data are included in the input argument
  obs_rec = fish_df %>%
    pull(receiver) %>%
    unique()

  if(sum(!obs_rec %in% receiver_nms) > 0) {
    cat(paste(paste(obs_rec[!obs_rec %in% receiver_nms], collapse = ", "), "are not in the receiver_nms argument, and will be dropped.\n"))
  }

  # turn receivers and sites into factors
  fish_df %<>%
    filter(receiver %in% receiver_nms) %>%
    mutate(receiver = factor(receiver,
                             levels = receiver_nms),
           site = factor(site,
                         levels = unique(str_sub(receiver_nms, 1, 2))))


  # combine detections by receiver
  rec_grp = fish_df %>%
    full_join(fish_df %>%
                mutate(nxt_tag = lead(tag_id),
                       nxt_rec = lead(receiver),
                       nxt_site = lead(site)) %>%
                mutate(new_grp = if_else(tag_id != nxt_tag |
                                           receiver != nxt_rec,
                                         T, F)) %>%
                filter(new_grp) %>%
                mutate(grp = 1:n())) %>%
    tidyr::fill(grp, .direction = 'up') %>%
    group_by(tag_id, site, receiver, grp) %>%
    summarise(first_obs = min(start),
              last_obs = max(end),
              n = sum(n)) %>%
    ungroup() %>%
    select(-grp) %>%
    arrange(tag_id, first_obs)

  # site_grp = fish_df %>%
  #   full_join(fish_df %>%
  #               mutate(nxt_tag = lead(tag_id),
  #                      nxt_rec = lead(receiver),
  #                      nxt_site = lead(site)) %>%
  #               mutate(new_grp = if_else(tag_id != nxt_tag |
  #                                          site != nxt_site,
  #                                        T, F)) %>%
  #               filter(new_grp) %>%
  #               mutate(grp = 1:n())) %>%
  #   tidyr::fill(grp, .direction = 'up') %>%
  #   group_by(tag_id, site, grp) %>%
  #   summarise(first_obs = min(start),
  #             last_obs = max(end),
  #             n = sum(n)) %>%
  #   ungroup() %>%
  #   select(-grp) %>%
  #   arrange(tag_id, first_obs)

  # remove detections that indicate upstream movement
  if(delete_upstream) {
    rec_grp = rec_grp %>%
      group_by(tag_id) %>%
      mutate(site_num = as.integer(site),
             nxt_site = lead(site_num)) %>%
      ungroup() %>%
      filter(nxt_site >= site_num | is.na(nxt_site))
      # filter(nxt_site < site_num) %>%
      # select(tag_id) %>%
      # distinct()

    # site_grp = site_grp %>%
    #   group_by(tag_id) %>%
    #   mutate(site_num = as.integer(site),
    #          nxt_site = lead(site_num)) %>%
    #   ungroup() %>%
    #   filter(nxt_site >= site_num | is.na(nxt_site))
  }

  # wide format
  if(output_format == 'wide') {
    cap_hist = rec_grp %>%
      select(tag_id, site) %>%
      distinct() %>%
      mutate(seen = 1) %>%
      spread(site, seen,
             fill = 0) %>%
      unite(cap_hist,
            -tag_id,
            sep = "",
            remove = F)
  }

  # long format
  if(output_format == 'long') {
    cap_hist = rec_grp %>%
      select(-site_num, -nxt_site)
  }

  return(cap_hist)

}
