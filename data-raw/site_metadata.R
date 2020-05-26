## code to prepare `site_metadata` dataset goes here

library(readxl)
library(dplyr)
library(readr)

# set Biomark NAS prefix, depending on operating system
if(.Platform$OS.type != 'unix') {
  nas_prefix = "S:"
}
if(.Platform$OS.type == 'unix') {
  nas_prefix = "~/../../Volumes/ABS"
}

site_metadata = read_excel(paste0(nas_prefix, "/data/telemetry/lemhi/site_metadata/rt_site_metadata.xlsx")) %>%
  filter(site_code %in% c("LLRTP", "LH", "CA", "TR", "RR", "NF"))

write_csv(site_metadata, "data-raw/site_metadata.csv")
usethis::use_data(site_metadata, overwrite = TRUE)
