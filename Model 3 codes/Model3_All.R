library(nimble)
library(tidyverse)
library(MCMCvis)

#' Read in the data. 
## ---------------------------------------------------------------------------------------------------------
data <- read.csv("All_River_Breeding_Sex_Data.csv")
y <- data %>%
  select(Fall_2007:Fall_2020) %>%
  #select(year_2014:year_2020) %>%
  as.matrix()

#' 
#' Get occasion of first capture.
## ---------------------------------------------------------------------------------------------------------
get.first <- function(x) min(which(x != 0))
first <- apply(y, 1, get.first)
#first

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
  #phiB ~ dunif(0, 1)
  #phiNB ~ dunif(0, 1)
  #psiB_NB ~ dunif(0, 1)
  #psiNB_B ~ dunif(0, 1)
  
  for (i in 1:4){
    
    b_f[i] ~ dnorm(0,1/10)
    b_bab[i] ~ dnorm(0,1/10)
    b_big[i] ~ dnorm(0,1/10)
    b_r[i] ~ dnorm(0,1/10)
    b_j[i] ~ dnorm(0, 1/10)
    
    a_f[i] ~ dnorm(0,1/10)
    a_bab[i] ~ dnorm(0,1/10)
    a_big[i] ~ dnorm(0,1/10)
    a_r[i] ~ dnorm(0,1/10)
    a_j[i] ~ dnorm(0, 1/10) 
  }
  
  for (i in 1:2) {
    cb_f[i] ~ dnorm(0, 1/10)
    cb_bab[i] ~ dnorm(0, 1/10)
    cb_big[i] ~ dnorm(0, 1/10)
    cb_r[i] ~ dnorm(0, 1/10)
    cb_j[i] ~ dnorm(0, 1/10)
    
  }
  
  # likelihood 
  for (i in 1:N){
    
    sex[i] ~ dbern(0.5)
    
    # latent state at first capture
    z[i,first[i]] <- y[i,first[i]] - 1 # if seen while B (y = 2), state is alive while B(y - 1 = z = 1) with prob = 1
    for (t in (first[i]+1):K){         # if seen while NB (y = 3), state is alive while B (y - 1 = z = 2) with prob = 1
      # draw (t) given z(t-1)
      z[i,t] ~ dcat(gamma[z[i,t-1],1:3,i])
      # draw y(t) given z(t)
      y[i,t] ~ dcat(omega[z[i,t],1:3, i])
    }
  }
  # probabilities of state z(t+1) given z(t)
  for (i in 1:N) {
    
    logit(phiB[i])<- (sex[i]*(a_f[1]*f[i] + a_bab[1]*bab[i] +
                             a_big[1]*big[i] + a_r[1]*r[i] + a_j[1]*j[i]) +
                    (1-sex[i])*(b_f[1]*f[i] + b_bab[1]*bab[i] +
                             b_big[1]*big[i] + b_r[1]*r[i] + b_j[1]*j[i]))
    
    logit(psiB_NB[i]) <- (sex[i]*(a_f[2]*f[i] + a_bab[2]*bab[i] +
                                  a_big[2]*big[i] + a_r[2]*r[i] + a_j[2]*j[i]) +
                        (1-sex[i])*(b_f[2]*f[i] + b_bab[2]*bab[i] +
                                  b_big[2]*big[i] + b_r[2]*r[i] + b_j[2]*j[i]))
    
    logit(phiNB[i])<- (sex[i]*(a_f[3]*f[i] + a_bab[3]*bab[i] +
                               a_big[3]*big[i] + a_r[3]*r[i] + a_j[3]*j[i]) +
                      (1-sex[i])*(b_f[3]*f[i] + b_bab[3]*bab[i] +
                               b_big[3]*big[i] + b_r[3]*r[i] + b_j[3]*j[i]))
    
    logit(psiNB_B[i])<- (sex[i]*(a_f[4]*f[i] + a_bab[4]*bab[i] +
                                 a_big[4]*big[i] + a_r[4]*r[i] + a_j[4]*j[i]) +
                        (1-sex[i])*(b_f[4]*f[i] + b_bab[4]*bab[i] +
                                 b_big[4]*big[i] + b_r[4]*r[i] + b_j[4]*j[i]))
    
    logit(pB[i]) <- f[i]*cb_f[1] + bab[i]*cb_bab[1] + big[i]*cbig_f[1] +
                    r[i]*cb_r[1] + j[i]*cb_j[1]
    
    logit(pNB[i]) <- f[i]*cb_f[2] + bab[i]*cb_bab[2] + big[i]*cbig_f[2] +
      r[i]*cb_r[1] + j[i]*cb_j[2]
    
    gamma[1,1,i] <- phiB[i] * (1 - psiB_NB[i])
    gamma[1,2,i] <- phiB[i] * psiB_NB[i]
    gamma[1,3,i] <- 1 - phiB[i]
    gamma[2,1,i] <- phiNB[i] * psiNB_B[i]
    gamma[2,2,i] <- phiNB[i] * (1 - psiNB_B[i])
    gamma[2,3,i] <- 1 - phiNB[i]
    gamma[3,1,i] <- 0
    gamma[3,2,i] <- 0
    gamma[3,3,i] <- 1
  
  # probabilities of y(t) given z(t)
  omega[1,1,i] <- 1 - pB[i]     # Pr(alive B t -> not-captured t)
  omega[1,2,i] <- pB[i]         # Pr(alive B t -> captured B t)
  omega[1,3,i] <- 0          # Pr(alive B t -> captured NB t)
  omega[2,1,i] <- 1 - pNB[i]     # Pr(alive NB t -> not-captured t)
  omega[2,2,i] <- 0          # Pr(alive NB t -> captured B t)
  omega[2,3,i] <- pNB[i]         # Pr(alive NB t -> captured NB t)
  omega[3,1,i] <- 1          # Pr(dead t -> not-captured t)
  omega[3,2,i] <- 0          # Pr(dead t -> captured B t)
  omega[3,3,i] <- 0          # Pr(dead t -> captured NB t)
  }
})

#' 
#' Data in a list. Remember to add 1. 
## ---------------------------------------------------------------------------------------------------------

sex.st<-as.vector(data$Sex)
f.st <- as.vector(data$Firth)
bab.st <- as.vector(data$Babbage)
big.st <- as.vector(data$Big)
r.st <- as.vector(data$Rat)
j.st <- as.vector(data$Joe)

my.data <- list(y = y + 1, sex = sex.st,
                f = f.st, bab = bab.st, big = big.st,
                r = r.st, j = j.st)

#' Constants in a list. 
## ---------------------------------------------------------------------------------------------------------
my.constants <- list(first = first, 
                     K = ncol(y), 
                     N = nrow(y))

#' 
#' Initial values without $p_B$. 
## ---------------------------------------------------------------------------------------------------------

s_inits <- sex.st #ifelse(!is.na(Rat_sex)==NA, 1)

s_inits[is.na(sex.st)] <- sample(c(0,1),sum(is.na(sex.st)), replace = TRUE)
s_inits[!is.na(sex.st)] <- NA 
s_inits

zinits <- y
zinits[zinits==0] <- sample(c(1,2), sum(zinits==0), replace = TRUE)

initial.values <- list(cb_f = runif(2, 0, 1), cb_bab = runif(2,0,1),
                       cb_big = runif(2,0,1), cb_r = runif(2,0,1),
                       cb_j = runif(2,0,1), a_f = runif(4,0,1), a_bab = runif(4,0,1),
                       a_big = runif(4,0,1), a_r = runif(4,0,1),
                       a_j = runif(4,0,1), b_f = runif(4,0,1),
                       b_bab = runif(4,0,1), b_big = runif(4,0,1), 
                       b_r = runif(4,0,1), b_j = runif(4,0,1),
                       z = zinits,
                       sex = s_inits)

#' 
#' Parameters to monitor. 
## ---------------------------------------------------------------------------------------------------------
parameters.to.save <- c("cb_f", "cb_bab", "cb_big", "cb_r", "cb_j",
                        "a_f", "a_bab", "a_big", "a_r", "a_j",
                        "b_f", "b_bab", "b_big", "b_r", "b_j")
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

pdf(file = "All_mp_m3.pdf")
MCMCplot(samples, HPD = T)
dev.off()

s <- MCMCsummary(samples, round = 5)
MCMCtrace(samples,pdf = T,open_pdf = F,filename = "All_m3", ind = TRUE,
          Rhat = FALSE, n.eff = FALSE)
write.csv(s, file = "All_m3_sum.csv")

