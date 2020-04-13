# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
# ::: EXERCISE 2.6.3: STATE-SPACE CORMACK-JOLLY-SEBER MODELS : #
# ::: SAME AS EX 2.6.2 BUT WITH INDIVIDUAL PHI COVARIATE ::::: #
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #

##### STEP 0: INITIALIZE THE SESSION #####

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

# apply to one capture history at a time
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
  width = c(NA, 5, 7, 15, 20, 30),
  birds = c(NA, 0, 1, 0,  1,  0),
  length = dat$length,
  pred_length = seq(min(dat$length), max(dat$length), length = 30),
  pred_birds = c(0,1),
  n_pred = 30
)
# add in the filled-in z matrix
jags_data = append(jags_data, list(z = t(apply(jags_data$y, 1, known_alive))))

##### STEP 2: SPECIFY JAGS MODEL CODE #####
jags_model = function() {
  # PRIORS
  a0 ~ dunif(-5, 5) # for detection
  a1 ~ dunif(-5, 5) # for detection
  b0 ~ dunif(-5, 5) # for survival
  b1 ~ dunif(-5, 5) # for survival
  b2 ~ dunif(-0.1, 0.1) # for survival
  for (j in 2:J) {
    logit(psi[j]) <- a0 + a1 * width[j]
  }
  # LIKELIHOOD
  for (i in 1:n) {
    for (j in 2:J) {
      # apply site and individual-based survival
      logit(phi[i,j]) <- b0 + b1 * birds[j] + b2 * length[i]
      # survival process
      z[i,j] ~ dbern(phi[i,j] * z[i,j-1])
      # detection process
      y[i,j] ~ dbern(psi[j] * z[i,j])
    }
  }
  # DERIVED QUANTITIES
  # survival between occasions and at a given length
  for (i in 1:n_pred) {
    for (j in 1:2) {
      logit(pred_phi[i,j]) <- b0 + b1 * pred_birds[j] + b2 * pred_length[i]
    }
  }
  # survivorship for fish of different sizes
  survship_small[1] <- 1
  survship_large[1] <- 1
  for (j in 2:J) {
    survship_small[j] <- survship_small[j-1] * pred_phi[1,birds[j] + 1]
    survship_large[j] <- survship_large[j-1] * pred_phi[30,birds[j] + 1]
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
      b0 = logit(runif(1, 0.4, 0.7)),
      b1 = runif(1, -0.5, 0.5),
      b2 = runif(1, -0.05, 0.05),
      a0 = runif(1, -2, 2),
      a1 = runif(1, -1, 1)
    )
  }
  return(inits)
}

##### STEP 4: SET NODES TO MONITOR #####
jags_params = c("b0", "b1", "b2", "a0", "a1", "psi", "pred_phi", "survship_small", "survship_large")

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
diag_p = c("^a", "^b", "psi"); match_p(post, diag_p, ubase = T)

# view convergence diagnostic summaries for nodes with priors
t(post_summ(post, diag_p, ess = T, Rhat = T)[c("Rhat", "ess"),])

# view diagnostic plots
diag_plots(post, diag_p, ext_device = T)

##### STEP 8: MAKE INFERENCE #####
# plot the survivorship curves
survship_small = post_summ(post, "survship_small")
survship_large = post_summ(post, "survship_large")
ext_device(h = 4, w = 4); par(mar = c(4,4,1,1))
plot(survship_large["mean",], xlab = "Detection Occassion",
     ylab = "Probability of Survival to Occassion",
     type = "b", pch = 16, ylim = c(0, 1))
lines(survship_large["2.5%",], col = "grey", pch = 16, type = "b")
lines(survship_large["97.5%",], col = "grey", pch = 16, type = "b")
# solid lines are large
lines(survship_small["mean",], pch = 1, type = "b", lty = 2)
lines(survship_small["2.5%",], pch = 1, col = "grey", type = "b", lty = 2)
lines(survship_small["97.5%",], pch = 1, col = "grey", type = "b", lty = 2)
# dashed lines are small

# parameter estimates
post_summ(post, "^a")
post_summ(post, "^b")
post_summ(post, "psi")

# plot length-based survival in different occasions
pred_phi = post_summ(post, "pred_phi")
pred_phi_lwr = array_format(pred_phi["2.5%",])
pred_phi_mean = array_format(pred_phi["mean",])
pred_phi_upr = array_format(pred_phi["97.5%",])
pred_length = jags_data$pred_length

ext_device(h = 4, w = 4); par(mar = c(4,4,1,1))
plot(1,1, type = "n", ylim = c(0.5,1), xlim = range(pred_length), xlab = "Fish Length",
     ylab = "Pr(Survival) Between Two Arrays")
polygon(c(pred_length, rev(pred_length)), c(pred_phi_lwr[,1], rev(pred_phi_upr[,1])), col = "skyblue", border = NA)
lines(pred_phi_mean[,1] ~ pred_length, lwd = 2, col = "blue")
polygon(c(pred_length, rev(pred_length)), c(pred_phi_lwr[,2], rev(pred_phi_upr[,2])), col = "salmon", border = NA)
lines(pred_phi_mean[,2] ~ pred_length, lwd = 2, col = "red")
legend("bottomright", legend = c("Absent", "Present"), title = "Birds", 
       pch = 22, col = c("blue", "red"), pt.bg = c("skyblue", "salmon"), bty = "n",
       pt.cex = 2)

# no LENGTH x BIRD interaction
ext_device(h = 4, w = 4); par(mar = c(4,4,1,1))
logit_pred_phi = logit(pred_phi_mean)
plot(logit_pred_phi[,1] ~ pred_length, ylim = range(logit_pred_phi), type = "l", col = "blue")
lines(logit_pred_phi[,2] ~ pred_length, col = "red")
