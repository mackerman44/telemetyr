#' @title Plot Receiver Volt and Temperature Info from Tracker Software
#'
#' @description Plots volt and temperature information from the Tracker software
#'
#' @author Mike Ackerman and Kevin See
#'
#' @inheritParams get_file_nms
#'
#' @param volt_temp_df A data frame from \code{read_volt_temp_data()}
#' @param column A column from volt_temp_df to plot for each receiver. Options from \code{read_volt_temp_data()} include
#' "volt_avg", "volt_min", "volt_max", "temp_avg", "temp_min", "temp_max".
#'
#' @import ggplot2
#' @export
#' @return time-series plots of volt or temp faceted by receiver

plot_volt_temp_data = function(volt_temp_df,
                               column = "volt_avg",
                               receiver_codes = NULL) {

  # get list of all unique receivers in volt_temp_df
  receiver_nms = sort(unique(volt_temp_df$receiver))

  # if user provides a list of receiver codes
  if(!is.null(receiver_codes)) {
    receiver_nms = receiver_nms[receiver_nms %in% receiver_codes]
  }

  # still need to filter for receiver_codes
  vt_p = volt_temp_df %>%
    select(-file_name, -file) %>%
    filter(receiver %in% receiver_nms) %>%
    mutate(date_time = as.POSIXct(paste(date, time), format = "%d/%m/%y %H:%M")) %>%
    select(-date, -time) %>%
    ggplot(aes(x = date_time)) +
    geom_line(aes(y = get(column))) +
    theme_bw() +
    labs(y = column) +
    facet_wrap(~ receiver,
               scales = "free")

  return(vt_p)

} # end plot_volt_temp_data()
