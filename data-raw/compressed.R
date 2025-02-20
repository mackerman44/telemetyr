## code to prepare `compressed` dataset goes here

library(telemetyr)
library(dplyr)
library(readr)

# set Biomark NAS prefix, depending on operating system
if(.Platform$OS.type != 'unix') {
  nas_prefix = "S:"
}
if(.Platform$OS.type == 'unix') {
  nas_prefix = "~/../../Volumes/ABS"
}

# Biomark NAS mapped to S:/
download_path = paste0(nas_prefix, "/data/telemetry/lemhi/fixed_site_downloads/2018_2019")

rec_nms = c("LH1", "CA1", "TR1", "RR1", "NF1")
compressed = read_txt_data(path = download_path,
                           receiver_codes = rec_nms) %>%
  # use only frequency 5
  filter(frequency == 5) %>%
  compress_raw_data(min_yr = 2018,
                    max_yr = 2019,
                    filter_valid = T,
                    round_to = 5,
                    max_min = 2,
                    assign_week = T,
                    week_base = "0901",
                    append_week = "first")

write_csv(compressed, "data-raw/compressed.csv")
usethis::use_data(compressed, overwrite = TRUE)
