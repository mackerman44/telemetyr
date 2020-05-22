## code to prepare `raw` dataset goes here

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
raw = read_txt_data(path = download_path,
                    receiver_codes = rec_nms) %>%
  filter(frequency == 5)

write_csv(raw, "data-raw/raw.csv")
usethis::use_data(raw, overwrite = TRUE)
