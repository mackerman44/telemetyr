## code to prepare `tag_releases` dataset goes here

library(telemetyr)
library(readxl)

tag_releases = read_excel("../data/prepped/tag_release/lemhi_winter_telemetry_tag_info.xlsx")

write_csv(tag_releases, "data-raw/tag_releases.csv")
usethis::use_data(tag_releases, overwrite = TRUE)
