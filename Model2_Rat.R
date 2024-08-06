library(tidyverse)
library(nimble)
library(MCMCvis)

#' Read in the data. 
## ---------------------------------------------------------------------------------------------------------
data <- read.csv("Rat_Breeding_Data.csv")
y <- data %>%
  select(Fall_2007:Fall_2020) %>%
  #select(year_2014:year_2020) %>%
  as.matrix()

#y <- as.matrix(data)
#head(y)

#' 
#' Get occasion of first capture.
## ---------------------------------------------------------------------------------------------------------
get.first <- function(x) min(which(x != 0))
first <- apply(y, 1, get.first)
#first


#' Actually, the initial state of the dolly varden is known exactly. It is alive at site of initial capture. Therefore, we don't need to try and estimate the probability of initial states. 
## ---------------------------------------------------------------------------------------------------------
code_m <- nimbleCode({
  
  # -------------------------------------------------
  # Parameters:
  # phiB: survival probability while B
  # phiNB: survival probability while NB
  # psiB_NB: movement probability from B to NB
  # psiNB_B: movement probability from NB to B
  # pB: recapture probability while B
  # pNB: recapture probability while NB
  # -------------------------------------------------
  # States (z):
  # 1 alive while B
  # 2 alive while NB
  # 3 dead
  # Observations (y):  
  # 1 not seen
  # 2 seen while B 
  # 3 seen while NB
  # -------------------------------------------------
  
  # priors
  phiB ~ dbeta(1, 1)
  phiNB ~ dbeta(1, 1)
  psiB_NB ~ dbeta(1, 1)
  psiNB_B ~ dbeta(1, 1)
  pB ~ dbeta(1, 1)
  pNB ~ dbeta(1, 1)
  
  # probabilities of state z(t+1) given z(t)
  gamma[1,1] <- phiB * (1 - psiB_NB)
  gamma[1,2] <- phiB * psiB_NB
  gamma[1,3] <- 1 - phiB
  gamma[2,1] <- phiNB * psiNB_B
  gamma[2,2] <- phiNB * (1 - psiNB_B)
  gamma[2,3] <- 1 - phiNB
  gamma[3,1] <- 0
  gamma[3,2] <- 0
  gamma[3,3] <- 1
  
  # probabilities of y(t) given z(t)
  omega[1,1] <- 1 - pB     # Pr(alive B t -> not-captured t)
  omega[1,2] <- pB         # Pr(alive B t -> captured B t)
  omega[1,3] <- 0          # Pr(alive B t -> captured NB t)
  omega[2,1] <- 1 - pNB     # Pr(alive NB t -> not-captured t)
  omega[2,2] <- 0          # Pr(alive NB t -> captured B t)
  omega[2,3] <- pNB         # Pr(alive NB t -> captured NB t)
  omega[3,1] <- 1          # Pr(dead t -> not-captured t)
  omega[3,2] <- 0          # Pr(dead t -> captured B t)
  omega[3,3] <- 0          # Pr(dead t -> captured NB t)
  
  # likelihood 
  for (i in 1:N){
    # latent state at first capture
    z[i,first[i]] <- y[i,first[i]] - 1 # if seen while B (y = 2), state is alive while B(y - 1 = z = 1) with prob = 1
    for (t in (first[i]+1):K){         # if seen while NB (y = 3), state is alive while B (y - 1 = z = 2) with prob = 1
      # draw z(t) given z(t-1)
      z[i,t] ~ dcat(gamma[z[i,t-1],1:3])
      # draw y(t) given z(t)
      y[i,t] ~ dcat(omega[z[i,t],1:3])
    }
  }
})

#' 
#' Data in a list. Remember to add 1. 
## ---------------------------------------------------------------------------------------------------------
my.data <- list(y = y + 1)


#' Constants in a list. 
## ---------------------------------------------------------------------------------------------------------
my.constants <- list(first = first, 
                     K = ncol(y), 
                     N = nrow(y))


#' 
#' Initial values without $p_B$. 
## ---------------------------------------------------------------------------------------------------------
zinits <- y
zinits[zinits==0] <- sample(c(1,2), sum(zinits==0), replace = TRUE)
initial.values <- function(){list(phiB = runif(1, 0, 1), 
                                  phiNB = runif(1, 0, 1), 
                                  psiB_NB = runif(1, 0, 1), 
                                  psiNB_B = runif(1, 0, 1), 
                                  pB = runif(1, 0, 1), 
                                  pNB = runif(1, 0, 1), 
                                  z = zinits)}

#' 
#' Parameters to monitor. 
## ---------------------------------------------------------------------------------------------------------
parameters.to.save <- c("phiB", "phiNB","psiB_NB", "psiNB_B", "pB", "pNB")
parameters.to.save

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

pdf(file = "Rat_mp_m2.pdf")
MCMCplot(samples, HPD = T)
dev.off()

s <- MCMCsummary(samples, round = 5)
MCMCtrace(samples,pdf = T,open_pdf = F,filename = "Rat_m2", ind = TRUE,
          Rhat = FALSE, n.eff = FALSE)
write.csv(s, file = "Rat_m2_sum.csv")