# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
# ::: EXERCISE 2.6.1: STATE-SPACE CORMACK-JOLLY-SEBER MODELS : #
# ::: SIMPLEST POSSIBLE MODEL: TIME CONSTANT P AND PHI ::::::: #
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
# mark-recap model that estimates survival
# must have multiple recapture periods
# if you didn't see an individual in a period, either it died or you didn't detect it
# false negative is measurement error
# SEE PRESENTATION
# use capture histories; first observation is the mark
# separate survival and detection probability
# i = individual
# n = number of individuals
# j = capture event (mark = j1)
# J = total number of events
# y = observed matrix of captures: n rows by J columns
# z = matrix of true states; alive or dead (estimated)
# p = detection prob (estimated)
# phi = survival prob (estimated)

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
head(dat); tail(dat)

# plot a subset of the capture histories
y = as.matrix(dat[,colnames(dat) != "length"])

ext_device(h = 4, w = 4)
par(mar = c(3,1,2,1), tcl = -0.25, mgp = c(1.5,0.5,0))
n_rand = 30
plot(1,1, type = "n", ylim = c(1,n_rand), xlim = c(1,ncol(y)), yaxt = "n",
     xlab = "Detection Occasion",
     main = paste0(n_rand, " Sample Capture Histories"))
abline(h = 1:n_rand, col = "grey90")
rand_i = sample(1:nrow(y), n_rand)
for (i in 1:length(rand_i)) {
  points(rep(i, ncol(y)) ~ seq(1,ncol(y)), col = "skyblue",
         pch = ifelse(y[rand_i[i],] == 1, 16, NA))
}; box()

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

# example
ex_y_i = c(1, 0, 0, 1, 0, 0)
ex_y_i; known_alive(ex_y_i)

# compile data into a list for JAGS
jags_data = list(
  n = nrow(dat),
  y = as.matrix(dat[,colnames(dat) != "length"]),
  J = ncol(dat) - 1
)
# add in the filled-in z matrix
jags_data = append(jags_data, list(z = t(apply(jags_data$y, 1, known_alive))))

##### STEP 2: SPECIFY JAGS MODEL CODE #####
jags_model = function() {
  # PRIORS
  phi ~ dbeta(1,1) # survival probability between arrays
  p ~ dbeta(1,1)   # detection probability at each array

  # LIKELIHOOD - Here, p and phi are global
  for (i in 1:n) {
    # j = 1 is the release occasion - known alive; i.e., the mark event
    for (j in 2:J) {
      # survival process: must have been alive in j-1 to have non-zero pr(alive at j)
      z[i,j] ~ dbern(phi * z[i,j-1]) # fish i in period j is a bernoulli trial
      
      # detection process: must have been alive in j to observe in j
      y[i,j] ~ dbern(p * z[i,j]) # another bernoulli trial
    }
  }
  
  # DERIVED QUANTITIES
  # survivorship is probability of surviving from release to a detection occasion
  survship[1] <- 1 # the mark event; everybody survived to this point
  for (j in 2:J) { # the rest of the events
    survship[j] <- survship[j-1] * phi
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
      phi = runif(1, 0.5, 1), # these could be lower, set min to 0.5
      p = runif(1, 0.5, 1)
    )
  }
  return(inits)
}

##### STEP 4: SET NODES TO MONITOR #####
jags_params = c("phi", "p", "survship")

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
# view convergence diagnostic summaries for nodes with priors
t(post_summ(post, "^p", ess = T, Rhat = T)[c("Rhat", "ess"),])

# view diagnostic plots
diag_plots(post, "^p", ext_device = T)

##### STEP 8: MAKE INFERENCE #####
post_summ(post, "^p", rnd = 3) # these apply to EACH event; global estimates
# assume detection prob and survivorship is equal among all events

# plot the survivorship curve
survship = post_summ(post, "survship")
ext_device(h = 4, w = 4); par(mar = c(4,4,1,1))
plot(survship["mean",], xlab = "Detection Occasion",
     ylab = "Probability of Survival to Occasion",
     type = "b", pch = 16, ylim = c(0, 1), las = 1)
lines(survship["2.5%",], col = "grey", pch = 16, type = "b")
lines(survship["97.5%",], col = "grey", pch = 16, type = "b")
