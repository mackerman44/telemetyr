## code to prepare `cjs_post` dataset goes here

library(telemetyr)
library(rjags)
library(readr)

data("ch_wide")
data("tag_releases")

# write JAGS model
file_path = 'data-raw/CJS_model.txt'
write_bayes_cjs(file_path)

# prepare data for JAGS
jags_data = prep_jags_cjs(ch_wide,
                          tag_releases)

# get posterior samples
cjs_post = run_jags_cjs(file_path = file_path,
                        jags_data = jags_data)

cjs_post %>%
  as.matrix(chain = T,
            iter = T) %>%
  as.data.frame() %>%
  write_csv("data-raw/cjs_post.csv")
usethis::use_data(cjs_post, overwrite = TRUE)
