## code to prepare `volt_temp` dataset goes here

library(telemetyr)
library(readr)

# Biomark NAS mapped to S:/
download_path = "S:/data/telemetry/lemhi/fixed_site_downloads/2018_2019"

volt_temp = read_volt_temp_data(download_path,
                                receiver_codes = c("LH1", "CA1", "TR1", "RR1", "NF1"))

write_csv(volt_temp, "data-raw/volt_temp.csv")
usethis::use_data(volt_temp, overwrite = TRUE)
