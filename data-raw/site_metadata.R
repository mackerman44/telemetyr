## code to prepare `site_metadata` dataset goes here

library(telemetyr)
library(readxl)

site_metadata = read_excel("../data/prepped/site_metadata/rt_site_metadata.xlsx")

write_csv(site_metadata, "data-raw/site_metadata.csv")
usethis::use_data(site_metadata, overwrite = TRUE)
