#' @title Summarize Test Tag Information
#'
#' @description Summarize test tag information including battery life
#'
#' @author Mike Ackerman and Kevin See
#'
#' @inheritParams prep_capture_history
#'
#' @import lubridate dplyr ggplot2 stringr
#' @export
#' @return summaries of test tag data

summarise_test_data = function(compress_df = NULL,
                               tag_data = 'data/prepped/tag_release/lemhi_winter_telemetry_tag_info.xlsx') {

  stopifnot(!is.null(compress_df),
            !is.null(tag_data))

  cat("Parsing out test tag data.\n")

  # what season?
  yr_label = paste(str_sub(lubridate::year(min(compress_df$start, na.rm = T)), -2),
                   str_sub(lubridate::year(max(compress_df$start, na.rm = T)), -2),
                   sep = "_")

  # get information about test tags including code and duty cycle
  test_tag_ids = read_excel(tag_data) %>%
    filter(season == yr_label,
           tag_purpose == "test") %>%
    select(radio_tag_id, duty_cycle) %>%
    mutate(tag_id = stringr::str_extract(radio_tag_id, "[:digit:]*"),
           tag_id = as.numeric(tag_id)) %>%
    select(-radio_tag_id)

  # filter out test tag data from compress_df
  tmp = compress_df %>%
    filter(tag_id %in% test_tag_ids$tag_id) %>%
    group_by(tag_id) %>%
    summarise(activation = min(start),
              dead = max(end)) %>%
    ungroup() %>%
    mutate(tag_life_days = as.numeric(difftime(dead,
                                               activation,
                                               units = "days"))) %>%
    left_join(test_tag_ids) %>%
    arrange(duty_cycle, tag_life_days)

  cat("Summarizing battery life data for test tags, by duty cycle.\n")

  # a function for summarizing quantiles
  cuts = c(0.25, 0.50, 0.75)
  cut_names = map_chr(cuts, ~paste0(.x*100, "%"))
  cut_funs = map(cuts, ~partial(quantile, probs = .x, na.rm = TRUE)) %>%
    set_names(nm = cut_names)

  # estimates of battery life for test tags, by duty cycle
  tag_life = tmp %>%
    group_by(duty_cycle) %>%
    summarise(n_tags = n(),
              mn_days = mean(tag_life_days),
              sd_days = sd(tag_life_days)) %>%
    left_join(tmp %>%
                group_by(duty_cycle) %>%
                summarise_at(vars(tag_life_days),
                             funs(!!!cut_funs))) %>%
    ungroup() %>%
    t()

  # plot test tag battery life
  # tmp_p = tmp %>%
  #   ggplot(aes(x = duty_cycle,
  #              y = tag_life_days,
  #              fill = duty_cycle)) +
  #   geom_dotplot(binaxis = "y",
  #                stackdir = "center",
  #                dotsize = 0.6) +
  #   theme_bw() +
  #   theme(legend.position = "none") +
  #   stat_summary(fun.y = mean,
  #                geom = "point",
  #                shape = 18,
  #                size = 3,
  #                color = "red") +
  #   labs(x = "Duty Cycle",
  #        y = "Tag Life (Days)")

  tmp_p = tmp %>%
    ggplot() +
    geom_boxplot(aes(x = duty_cycle,
                     y = tag_life_days,
                     color = duty_cycle)) +
    theme_bw() +
    labs(x = "Duty Cycle",
         y = "Tag Life (Days)",
         color = "Duty Cycle")

  # return objects
  tmp_list = list(test_tag_ids = test_tag_ids,
                  test_df = tmp,
                  tag_life = tag_life,
                  tag_life_p = tmp_p)
  return(tmp_list)


} # end summarise_test_data()

