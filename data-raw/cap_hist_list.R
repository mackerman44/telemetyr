## code to prepare `ch_wide`, `ch_long`, and `tag_df` datasets goes here

library(telemetyr)
library(tidyverse)
library(magrittr)


data("compressed")
data("tag_releases")
data("site_metadata")

example_tags = tag_releases %>%
  filter(season == "18_19") %>%
  filter(str_detect(tag_id, "^2|^5|^8"))

cjs_sites = site_metadata %>%
  filter(site_code %in% c("LH", "CA", "TR", "RR", "NF")) %>%
  select(site = site_code,
         receivers) %>%
  group_by(site) %>%
  nest() %>%
  ungroup() %>%
  mutate(receiver = purrr::map(data,
                        .f = function(x) {
                          str_split(x, "\\,") %>%
                            magrittr::extract2(1) %>%
                            str_trim()
                        })) %>%
  select(-data) %>%
  unnest(cols = receiver) %>%
  mutate_at(vars(site, receiver),
            list(~ factor(., levels = unique(.))))


# prepare capture histories
cap_hist_list = prep_capture_history(compressed,
                                     tag_data = example_tags,
                                     n_obs_valid = 3,
                                     rec_site = cjs_sites,
                                     delete_upstream = T,
                                     location = "site",
                                     output_format = "all")
# pull out components of list
ch_wide = cap_hist_list$ch_wide
ch_long = cap_hist_list$ch_long
tag_df = cap_hist_list$tag_df %>%
  select(-fish_name)

write_csv(ch_wide, "data-raw/ch_wide.csv")
usethis::use_data(ch_wide, overwrite = TRUE)

write_csv(ch_long, "data-raw/ch_long.csv")
usethis::use_data(ch_long, overwrite = TRUE)

write_csv(tag_df, "data-raw/tag_df.csv")
usethis::use_data(tag_df, overwrite = TRUE)

