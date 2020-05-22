## code to prepare 'ch_long` dataset goes here

library(telemetyr)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

data("compressed")
data("tag_releases")
data("site_metadata")

example_sites = site_metadata %>%
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
ch_long = prep_capture_history(compressed,
                               tag_data = tag_releases,
                               n_obs_valid = 3,
                               rec_site = example_sites,
                               delete_upstream = T,
                               location = "site",
                               output_format = "long")

write_csv(ch_long, "data-raw/ch_long.csv")
usethis::use_data(ch_long, overwrite = TRUE)
