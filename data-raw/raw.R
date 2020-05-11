## code to prepare `raw` dataset goes here

library(telemetyr)
library(dplyr)
library(readr)

# Biomark NAS mapped to S:/
download_path = "S:/data/telemetry/lemhi/fixed_site_downloads/2018_2019"

rec_nms = c("LH1", "CA1", "TR1", "RR1", "NF1")
raw = read_txt_data(path = download_path,
                    receiver_codes = rec_nms) %>%
  #filter(frequency %in% c(3,7))
  filter(frequency == 5)

write_csv(raw, "data-raw/raw.csv")
usethis::use_data(raw, overwrite = TRUE)
