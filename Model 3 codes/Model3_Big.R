library(nimble)
library(tidyverse)
library(MCMCvis)

#' Read in the data. 
## ---------------------------------------------------------------------------------------------------------
data <- read.csv("Big_Breeding_Sex_Data.csv")
y <- data %>%
  select(Fall_2009:Fall_2020) %>%
  #select(year_2014:year_2020) %>%
  as.matrix()

#' 
#' Get occasion of first capture.
## ---------------------------------------------------------------------------------------------------------
get.first <- function(x) min(which(x != 0))
first <- apply(y, 1, get.first)
#first

code_m <- nimbleCode({
  
  # -------------------------------------------------#
  
  #delta_m ~ dbeta(1,1)
  
  pB ~ dbeta(1, 1)
  pNB ~ dbeta(1, 1)
  
  beta[1] ~ dnorm(0,1/100)
  beta[2] ~ dnorm(0,1/100)
  beta[3] ~ dnorm(0,1/100)
  beta[4] ~ dnorm(0,1/100)
  
  alpha[1] ~ dnorm(0,1/100)
  alpha[2] ~ dnorm(0,1/100)
  alpha[3] ~ dnorm(0,1/100)
  alpha[4] ~ dnorm(0,1/100)
  
  # likelihood 
  for (i in 1:N){
    
    sex[i] ~ dbern(0.3)
    
    # latent state at first capture
    z[i,first[i]] <- y[i,first[i]] - 1 # if seen while B (y = 2), state is alive while B(y - 1 = z = 1) with prob = 1
    for (t in (first[i]+1):K){         # if seen while NB (y = 3), state is alive while B (y - 1 = z = 2) with prob = 1
      # draw (t) given z(t-1)
      z[i,t] ~ dcat(gamma[z[i,t-1],1:3,i])
      # draw y(t) given z(t)
      y[i,t] ~ dcat(omega[z[i,t],1:3])
    }
  }
  # probabilities of state z(t+1) given z(t)
  for (i in 1:N) {
    
    logit(phiB[i])<- (alpha[1]*sex[i])+(beta[1]*(1-sex[i]))
    logit(psiB_NB[i]) <- (alpha[2]*sex[i])+(beta[2]*(1-sex[i]))
    logit(phiNB[i])<- (alpha[3]*sex[i])+(beta[3]*(1-sex[i]))
    logit(psiNB_B[i])<- (alpha[4]*sex[i])+(beta[4]*(1-sex[i]))
    
    gamma[1,1,i] <- phiB[i] * (1 - psiB_NB[i])
    gamma[1,2,i] <- phiB[i] * psiB_NB[i]
    gamma[1,3,i] <- 1 - phiB[i]
    gamma[2,1,i] <- phiNB[i] * psiNB_B[i]
    gamma[2,2,i] <- phiNB[i] * (1 - psiNB_B[i])
    gamma[2,3,i] <- 1 - phiNB[i]
    gamma[3,1,i] <- 0
    gamma[3,2,i] <- 0
    gamma[3,3,i] <- 1
  }
  
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
})

#' 
#' Data in a list. Remember to add 1. 
## ---------------------------------------------------------------------------------------------------------

sex.st<-as.vector(data$Sex)

my.data <- list(y = y + 1, sex = sex.st)


#' Constants in a list. 
## ---------------------------------------------------------------------------------------------------------
my.constants <- list(first = first, 
                     K = ncol(y), 
                     N = nrow(y))


#' 
#' Initial values without $p_B$. 
## ---------------------------------------------------------------------------------------------------------

s_inits <- sex.st #ifelse(!is.na(Big_sex)==NA, 1)

s_inits[is.na(sex.st)] <- sample(c(0,1),sum(is.na(sex.st)), replace = TRUE)
s_inits[!is.na(sex.st)] <- NA 
s_inits

zinits <- y
zinits[zinits==0] <- sample(c(1,2), sum(zinits==0), replace = TRUE)
initial.values <- list(pB = runif(1, 0, 1), 
                       pNB = runif(1, 0, 1),
                       alpha = runif(4,0,1),
                       beta = runif(4,0,1),
                       z = zinits,
                       sex = s_inits)

#' 
#' Parameters to monitor. 
## ---------------------------------------------------------------------------------------------------------
parameters.to.save <- c("pB", "pNB","alpha","beta")
parameters.to.save

#' MCMC settings.
## ---------------------------------------------------------------------------------------------------------
n.iter <- 300000
n.burnin <- 40000
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

pdf(file = "Big_mp_m3.pdf")
MCMCplot(samples, HPD = T)
dev.off()

s <- MCMCsummary(samples, round = 5)
MCMCtrace(samples,pdf = T,open_pdf = F,filename = "Big_m3", ind = TRUE,
          Rhat = FALSE, n.eff = FALSE)
write.csv(s, file = "Big_m3_sum.csv")

