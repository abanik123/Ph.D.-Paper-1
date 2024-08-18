library(nimble)
library(tidyverse)
library(MCMCvis)

# Function to get the index of the first non-zero value in a row
get_first_non_zero_index <- function(row) {
  non_zero_indices <- which(row != 0)
  if (length(non_zero_indices) > 0) {
    return(non_zero_indices[1])
  } else {
    return(NA)
  }
}

data <- read.csv("All_River_Capture_Data.csv")
y <- data %>%
  select(Fall_2007:Fall_2020) %>%
  #select(year_2014:year_2020) %>%
  as.matrix()

code_m <- nimbleCode({
  
  for (i in 1:2){
    
    a_f[i] ~ dnorm(0,1/10)
    a_bab[i] ~ dnorm(0,1/10)
    a_big[i] ~ dnorm(0,1/10)
    a_r[i] ~ dnorm(0,1/10)
    a_j[i] ~ dnorm(0, 1/10) 
  }
  
  # Simulating data
  for (i in 1:N) {
    # Initial state
    for (j in f_1[i]:f_1[i]) {
      z[i,j] <- y[i,j] - 1
    }
    for (j in (f_1[i]+1):nyears) {
      # State transition
      z[i, j] ~ dcat(px[z[i, j - 1], 1:2, i])
      
      # Observation process
      y[i, j] ~ dcat(po[z[i, j], 1:2, i])
    }
  }
  # probabilities of state z(t+1) given z(t)
  for (i in 1:N) {
    
    logit(phi[i])<- (a_f[1]*f[i] + a_bab[1]*bab[i] +
                         a_big[1]*big[i] + a_r[1]*r[i] + a_j[1]*j[i])
    logit(p[i])<- (a_f[2]*f[i] + a_bab[2]*bab[i] +
                     a_big[2]*big[i] + a_r[2]*r[i] + a_j[2]*j[i])
    
    # Observation process probabilities
    po[1, 1, i] <- 1 - p[i]
    po[1, 2, i] <- p[i]
    
    po[2, 1, i] <- 1
    po[2, 2, i] <- 0
    
    # Latent state process probabilities
    px[1, 1, i] <- phi[i]
    px[1, 2, i] <- 1 - phi[i]
    
    px[2, 1, i] <- 0
    px[2, 2, i] <- 1
  }
})

#' 
#' Data in a list. Remember to add 1. 
## ---------------------------------------------------------------------------------------------------------
f.st <- as.vector(data$Firth)
bab.st <- as.vector(data$Babbage)
big.st <- as.vector(data$Big)
r.st <- as.vector(data$Rat)
j.st <- as.vector(data$Joe)

my.data <- list(y = y + 1, f = f.st, bab = bab.st, big = big.st,
                r = r.st, j = j.st)

# Apply the function to each row
f_1 <- apply(y, 1, get_first_non_zero_index)

N = nrow(y)
nyears = ncol(y)

# Generate inits for the latent states
x.init <- matrix(1, N, nyears)  # Set to 1 or another appropriate value for initial state
for (i in 1:N) {
  if (f_1[i] > 1) x.init[i, 1:(f_1[i]-1)] <- NA
}

# Constants in a list
my.constants <- list(N = N, nyears = nyears, f_1 = f_1)

initial.values <- list(a_f = runif(2, 0, 1), a_bab = runif(2, 0, 1),
                       a_big = runif(2, 0, 1), a_r = runif(2, 0, 1),
                       a_j = runif(2, 0, 1), z = x.init)

# Parameters to monitor
parameters.to.save <- c("a_f", "a_bab", "a_big", "a_r", "a_j")

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

pdf(file = "All_mp_m1.pdf")
MCMCplot(samples, HPD = T)
dev.off()

s <- MCMCsummary(samples, round = 5)
MCMCtrace(samples,pdf = T,open_pdf = F,filename = "All_m1", ind = TRUE,
          Rhat = FALSE, n.eff = FALSE)
write.csv(s, file = "All_m1_sum.csv")

