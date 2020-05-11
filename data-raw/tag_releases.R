## code to prepare `tag_releases` dataset goes here

library(telemetyr)
library(readxl)

tag_releases = read_excel("data/prepped/tag_release/lemhi_winter_telemetry_tag_info.xlsx") %>%
  mutate(tag_id = str_extract(radio_tag_id, "[:digit:]*"),
         tag_id = as.numeric(tag_id)) %>%
  mutate_at(vars(activation_time, release_time),
            list(as.numeric)) %>%
  mutate_at(vars(activation_time, release_time),
            list(janitor::excel_numeric_to_date),
            include_time = T) %>%
  select(season, matches('tag_id'), everything())

write_csv(tag_releases, "data-raw/tag_releases.csv")
usethis::use_data(tag_releases, overwrite = TRUE)
