#' @title Fit Bayesian CJS Model
#'
#' @description This wrapper function writes a text file describing a Bayesian Cormack Jolly-Seber model in the JAGS language, prepares the data, runs the MCMC and summarises the results.
#'
#' @author Kevin See and Mike Ackerman
#'
#' @inheritParams write_bayes_cjs
#' @inheritParams prep_jags_cjs
#' @inheritParams run_jags_cjs
#' @inheritParams summarise_jags_cjs
#'
#'
#' @importFrom postpack write_model
#' @import dplyr
#' @import rjags
#' @importFrom tidyr nest
#' @importFrom purrr map_dbl
#' @importFrom postpack post_summ
#'
#'
#' @export
#' @return tibble

fit_bayes_cjs = function(file_path = NULL,
                         cap_hist_wide = NULL,
                         tag_meta = NULL,
                         drop_col_nm = 'duty_cycle',
                         drop_values = c('batch_2', 'batch_3'),
                         n_chains = 4,
                         n_adapt = 1000,
                         n_burnin = 2500,
                         n_iter = 2500,
                         n_thin = 5,
                         params_to_save = c("phi", "p", "survship"),
                         rng_seed = 4,
                         ...) {

  # write the text file in JAGS language
  write_bayes_cjs(file_path)

  # prepare data to be fed to JAGS
  jags_data = prep_jags_cjs(cap_hist_wide,
                            tag_meta,
                            drop_col_nm,
                            drop_values)

  # run the MCMC
  post = run_jags_cjs(file_path,
                      jags_data,
                      n_chains,
                      n_adapt,
                      n_burnin,
                      n_iter,
                      n_thin,
                      params_to_save,
                      rng_seed)

  # extract summary of MCMC
  param_summ = summarise_jags_cjs(post,
                                  p = params_to_save,
                                  ...)

  # add site names to summary tibble
  param_summ = param_summ %>%
    left_join(tibble(site = colnames(jags_data$y)) %>%
                mutate(site = factor(site, levels = site),
                       site_num = as.integer(site))) %>%
    select(param_grp, site_num,
           site,
           param,
           everything())

  return(param_summ)

}
