#' @title Run Bayesian CJS Model
#'
#' @description Generate MCMC samples from the posteriors of a Bayesian Cormack Jolly-Seber model.
#'
#' @author Kevin See and Mike Ackerman
#'
#' @inheritParams write_bayes_cjs
#'
#' @param jags_data list of data to be fed into the JAGS CJS model. Generated from \code{prep_jags_cjs}.
#' @param n_chains the number of parallel chains for the model
#' @param n_adapt the number of iterations for adaptation. If n_adapt = 0 then no adaptation takes place.
#' @param n_burnin the number of iterations of the Markov chain to run during the burn-in phase
#' @param n_iter the number of iterations to monitor
#' @param n_thin the thinning interval for monitors
#' @param params_to_save a character vector giving the names of variables to be monitored
#' @param rng_seed random number generator seed, to make results reproducible
#' @param ... other parameters to be fed to \code{rjags::jags.model, update.jags} or \code{rjags::coda.samples}
#'
#' @import rjags
#' @export
#' @return mcmc.list

run_jags_cjs = function(file_path = NULL,
                        jags_data = NULL,
                        n_chains = 4,
                        n_adapt = 1000,
                        n_burnin = 2500,
                        n_iter = 2500,
                        n_thin = 5,
                        params_to_save = c("phi", "p", "survship"),
                        rng_seed = 4) {

  stopifnot(!is.null(jags_data),
            !is.null(file_path))

  # set the seed for reproducibility
  set.seed(rng_seed)
  # set up and adapt
  jags = rjags::jags.model(file_path,
                           data = jags_data,
                           n.chains = n_chains,
                           n.adapt = n_adapt)
  # burnin
  update(jags,
         n.iter = n_burnin)
  # posterior sampling
  post = rjags::coda.samples(model = jags,
                             variable.names = params_to_save,
                             n.iter = n_iter,
                             thin = n_thin)

  return(post)
}
