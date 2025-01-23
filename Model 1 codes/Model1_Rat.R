library(nimble)
library(MCMCvis)
library(tidyverse)
library(dplyr)

# Function to get the index of the first non-zero value in a row
get_first_non_zero_index <- function(row) {
  non_zero_indices <- which(row != 0)
  if (length(non_zero_indices) > 0) {
    return(non_zero_indices[1])
  } else {
    return(NA)
  }
}

capture_mat <- read.csv("Rat_Capture_Data.csv")

code_m <- nimbleCode({
  # Priors
  phi ~ dbeta(1, 2)   # Survival Probability
  p ~ dbeta(1, 2)     # Capture Probability
  
  # Observation process probabilities
  po[1, 1] <- 1 - p
  po[1, 2] <- p
  
  po[2, 1] <- 1
  po[2, 2] <- 0
  
  # Latent state process probabilities
  px[1, 1] <- phi
  px[1, 2] <- 1 - phi
  
  px[2, 1] <- 0
  px[2, 2] <- 1
  
  # Simulating data
  for (i in 1:N) {
    # Initial state
    for (j in f_1[i]:f_1[i]) {
      z[i,j] <- y[i,j] - 1
    }
    for (j in (f_1[i]+1):nyears) {
      # State transition
      z[i, j] ~ dcat(px[z[i, j - 1], 1:2])
      
      # Observation process
      y[i, j] ~ dcat(po[z[i, j], 1:2])
    }
  }
})

y <- capture_mat

my.data = list(y=y + 1)
# Apply the function to each row
f_1 <- apply(capture_mat, 1, get_first_non_zero_index)

N = nrow(capture_mat)
nyears = ncol(capture_mat)

# Generate inits for the latent states
x.init <- matrix(1, N, nyears)  # Set to 1 or another appropriate value for initial state
for (i in 1:N) {
  if (f_1[i] > 1) x.init[i, 1:(f_1[i]-1)] <- NA
}

# Constants in a list
my.constants <- list(N = N, nyears = nyears, f_1 = f_1)

initial.values <- list(phi = runif(1, 0, 1),
                       p = runif(1, 0, 1),
                       z = x.init)

# Parameters to monitor
parameters.to.save <- c("phi", "p")
#' MCMC settings.
## ---------------------------------------------------------------------------------------------------------
n.iter <- 175000
n.burnin <- 35000
n.chains <- 3


####################################################

mcmc.multistate <- nimbleMCMC(code = code_m, 
                              constants = my.constants,
                              data = my.data,              
                              inits = initial.values,
                              monitors = parameters.to.save,
                              niter = n.iter,
                              nburnin = n.burnin, 
                              nchains = n.chains,
                              summary = TRUE, WAIC = TRUE)

waic <- mcmc.multistate$WAIC
waic
#Model Summary
samples<- mcmc.multistate$samples

pdf(file = "Rat_mp_m1.pdf")
MCMCplot(samples, HPD = T)
dev.off()

s <- MCMCsummary(samples, round = 5)
MCMCtrace(samples,pdf = T,open_pdf = F,filename = "Rat_m1", ind = TRUE,
          Rhat = FALSE, n.eff = FALSE)
write.csv(s, file = "Rat_m1_sum.csv")