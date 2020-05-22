## code to prepare `tag_releases` dataset goes here

library(dplyr)
library(stringr)
library(readxl)
library(readr)

# set Biomark NAS prefix, depending on operating system
if(.Platform$OS.type != 'unix') {
  nas_prefix = "S:"
}
if(.Platform$OS.type == 'unix') {
  nas_prefix = "~/../../Volumes/ABS"
}

tag_releases = read_excel(paste0(nas_prefix, "data/telemetry/lemhi/tag_release/lemhi_winter_telemetry_tag_info.xlsx")) %>%
  # create new tag_id column that removes letter
  mutate(tag_id = str_extract(radio_tag_id, "[:digit:]*"),
         tag_id = as.numeric(tag_id)) %>%
  # only use data from 18/19 season and frequency 5
  filter(season == "18_19") %>%
  filter(str_detect(tag_id, "^5")) %>%
  # some cleaning of dates
  mutate_at(vars(activation_time, release_time),
            list(as.numeric)) %>%
  mutate_at(vars(activation_time, release_time),
            list(janitor::excel_numeric_to_date),
            include_time = T) %>%
  # some final data cleaning
  select(season, matches('tag_id'), everything()) %>%
  select(-fish_name)

write_csv(tag_releases, "data-raw/tag_releases.csv")
usethis::use_data(tag_releases, overwrite = TRUE)
