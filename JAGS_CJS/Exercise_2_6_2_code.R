# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
# ::: EXERCISE 2.6.2: STATE-SPACE CORMACK-JOLLY-SEBER MODELS : #
# ::: OCCASION-SPECIFIC COVARIATES ON P AND PHI :::::::::::::: #
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #

##### STEP 0: INITIALIZE THE SESSION #####
# See presentation
# Two things; model surv differently for reach with/out bird colonies
# Model survival taking into account river width

# SET THE WORKING DIRECTORY TO THE LOCATION OF THIS FILE
# Session > Set Working Directory > To Source File Location
setwd('C:/Users/mikea/Dropbox/BayesianAnalysesWithJAGS/AFS19_2/Exercises/Exercise 2_6/')

# clear the workspace
rm(list = ls(all = T))

# load packages
library(postpack)
library(StatonMisc)

##### STEP 1: READ AND PREPARE DATA #####
dat = read.csv("Exercise_2_6_data.csv")

# a function we will need
  # fills in the true state matrix for elements we know 
  # each individual must have been alive
known_alive = function(y_i) {
  f_alive = min(which(y_i == 1))
  l_alive = max(which(y_i == 1))
  z = rep(NA, length(y_i))
  z[f_alive:l_alive] = 1
  z
}

# compile data into a list for JAGS
jags_data = list(
  n = nrow(dat),
  y = as.matrix(dat[,colnames(dat) != "length"]),
  J = ncol(dat) - 1,
  width = c(NA, 5, 7, 15, 20, 30), # river width
  birds = c(NA, 0, 1, 0,  1,  0)   # are birds present (1) or not (0)
)
# add in the filled-in z matrix
jags_data = append(jags_data, list(z = t(apply(jags_data$y, 1, known_alive))))

##### STEP 2: SPECIFY JAGS MODEL CODE #####
jags_model = function() {
  # PRIORS
  a0 ~ dunif(-5, 5) # for detection; intercept
  a1 ~ dunif(-5, 5) # for detection; slope
  b0 ~ dunif(-5, 5) # for survival; intercept
  b1 ~ dunif(-5, 5) # for survival; slope
  
  # APPLY COEFFICIENTS/COVARIATES
  for (j in 2:J) {
    logit(p[j]) <- a0 + a1 * width[j]    # model detection as a function of width
    logit(phi[j]) <- b0 + b1 * birds[j]  # model surival as a function of bird presence/absence
  }
  
  # LIKELIHOOD
  for (i in 1:n) {
    # j = 1 is the release occasion - known alive
    for (j in 2:J) {
      # survival process
      z[i,j] ~ dbern(phi[j] * z[i,j-1]) # just added the j
      # detection process
      y[i,j] ~ dbern(p[j] * z[i,j])     # just added the j
    }
  }
  
  # DERIVED QUANTITIES
  survship[1] <- 1
  for (j in 2:J) {
    survship[j] <- survship[j-1] * phi[j] # added j here, too
  }
}

# write model to a text file
jags_file = "model.txt"
write_model(jags_model, jags_file)

##### STEP 3: SPECIFY INITIAL VALUES #####
jags_inits = function(nc) {
  inits = list()
  for (c in 1:nc) {
    inits[[c]] = list(
      b0 = logit(runif(1, 0.4, 0.7)), # he started these here because he simulated data (he knew results)
      b1 = runif(1, -0.5, 0.5),
      a0 = runif(1, -2, 2),
      a1 = runif(1, -1, 1)
    )
  }
  return(inits)
}

##### STEP 4: SET NODES TO MONITOR #####
jags_params = c("b0", "b1", "a0", "a1", "phi", "p", "survship")

##### STEP 5: SPECIFY MCMC DIMENSIONS #####
jags_dims = c(
  ni = 5000,  # number of post-burn-in samples per chain
  nb = 1000,  # number of burn-in samples
  nt = 1,     # thinning rate
  nc = 3      # number of chains
)

##### STEP 6: RUN THE MODEL WITH JAGS #####
# should take less than 3 minutes
post = jagsUI::jags.basic(
  data = jags_data,
  model.file = jags_file,
  inits = jags_inits(jags_dims["nc"]),
  parameters.to.save = jags_params,
  n.adapt = 1000,
  n.iter = sum(jags_dims[c("ni", "nb")]),
  n.thin = jags_dims["nt"],
  n.burnin = jags_dims["nb"],
  n.chains = jags_dims["nc"],
  parallel = T
)

##### STEP 7: CONVERGENCE DIAGNOSTICS #####
diag_p = c("^a", "^b", "phi", "^p[")

# view convergence diagnostic summaries
t(post_summ(post, diag_p, ess = T, Rhat = T)[c("Rhat", "ess"),])

# view diagnostic plots
diag_plots(post, diag_p, ext_device = T)

##### STEP 8: MAKE INFERENCE #####

# plot the survivorship curve
survship = post_summ(post, "survship")
ext_device(h = 4, w = 4); par(mar = c(4,4,1,1))
plot(survship["mean",], xlab = "Detection Occasion",
     ylab = "Probability of Survival to Occasion",
     type = "b", pch = 16, ylim = c(0, 1), las = 1)
lines(survship["2.5%",], col = "grey", pch = 16, type = "b")
lines(survship["97.5%",], col = "grey", pch = 16, type = "b")

post_summ(post, "^a", rnd = 2) # both of these do not encompass 0
post_summ(post, "^b", rnd = 2)
post_summ(post, "phi") # survival
post_summ(post, "^p[") # detection

# plot occasion-specific survival and detection probs
ext_device(h = 7, w = 4); par(mfrow = c(2,1), mar = c(2,2,2,2))
boxplot(post_subset(post, "phi", matrix = T), outline = F, main = "Survival",
        col = ifelse(jags_data$birds[2:jags_data$J] == 0, "skyblue", "salmon"))
boxplot(post_subset(post, "^p[", matrix = T), outline = F, main = "Detection",
        col = "grey")
