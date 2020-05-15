## code to prepare `tag_df` dataset goes here

library(telemetyr)
library(tidyverse)
library(magrittr)

data("tag_releases")

tag_df = tag_releases %>%
  filter(season == "18_19") %>%
  filter(str_detect(tag_id, "^5"))

write_csv(tag_df, "data-raw/tag_df.csv")
usethis::use_data(tag_df, overwrite = TRUE)
