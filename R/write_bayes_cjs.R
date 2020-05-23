#' @title Write Bayesian CJS Model
#'
#' @description Creates a text file describing a Bayesian Cormack Jolly-Seber model.
#'
#' @author Kevin See and Mike Ackerman
#'
#' @param file_path name (with file path) to save the model as. Probably a .txt file
#'
#' @importFrom postpack write_model
#' @export
#' @return NULL

write_bayes_cjs = function(file_path = NULL) {

  if(is.null(file_path)) file_path = 'CJS_model.txt'

  # specify model in JAGS
  jags_model = function() {
    # PRIORS
    phi[1] <- 1
    p[1] <- 1
    for(j in 2:J) {
      phi[j] ~ dbeta(1,1) # survival probability between arrays
      p[j] ~ dbeta(1,1)   # detection probability at each array
    }

    # LIKELIHOOD - Here, p and phi are global
    for (i in 1:N) {
      # j = f[i] is the release occasion - known alive; i.e., the mark event
      for (j in (f[i] + 1):J) {
        # survival process: must have been alive in j-1 to have non-zero pr(alive at j)
        z[i,j] ~ dbern(phi[j] * z[i,j-1]) # fish i in period j is a bernoulli trial

        # detection process: must have been alive in j to observe in j
        y[i,j] ~ dbern(p[j] * z[i,j]) # another bernoulli trial
      }
    }

    # DERIVED QUANTITIES
    # survivorship is probability of surviving from release to a detection occasion
    survship[1] <- 1 # the mark event; everybody survived to this point
    for (j in 2:J) { # the rest of the events
      survship[j] <- survship[j-1] * phi[j]
    }
  }

  postpack::write_model(jags_model, file_path)
}
