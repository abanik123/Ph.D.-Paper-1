library(nimble)
library(MCMCvis)

# Defining lists
code_sim_bs <- list()
f <- list()

cap_bs <- list()
cap_d_bs <- list()

random_rows_bs <- list()
random_sort_bs <- list()

cap_df_bs <- list()

n_ind_bs <- list()

f_capture_mat <- list()
rows_to_na <- list()

sex_bs <- list()

c_sex <- list()

# Create a list to store simulated datasets
simulated_datasets <- list()

# Define the model code

num_datasets <- 10000  # Number of datasets to simulate

for (s in 1:num_datasets){
  
  code_sim_bs[[s]] <- nimbleCode({
    # Priors
    delta_m <- 0.5
    
    beta_init ~ dnorm(0,1)
    
    beta[1] <- 0.05
    beta[2] <- -3.5
    beta[3] <- 0.05
    beta[4] <- 0.05
    
    alpha_init ~ dnorm(0,1)
    
    alpha[1] <- -0.7
    alpha[2] <- -3.5
    alpha[3] <- 0.5
    alpha[4] <- 0.2
    
    pB <- 0.2
    pNB <- 0.05
    
    deltaNB ~ dbeta(1, 1)
    deltaB ~ dbeta(1, 1)
    
    # Probabilities of being in initial state
    
    omega_init[1,1] <- deltaB
    omega_init[1,2] <- 0
    omega_init[1,3] <- 1 - deltaB
    
    omega_init[2,1] <- 0
    omega_init[2,2] <- deltaNB
    omega_init[2,3] <- 1 - deltaNB
    
    omega_init[3,1] <- 0
    omega_init[3,2] <- 0
    omega_init[3,3] <- 1
    
    for (i in 1:N){
      
      sex[i] ~ dbern(delta_m)
      
      logit(piB[i]) <- (alpha_init*sex[i]) + (beta_init*(1-sex[i]))
      
      gamma_init[1,i] <- piB[i]       # prob. of being in initial state B
      gamma_init[2,i] <- 1 - piB[i]   # prob. of being in initial state NB
      gamma_init[3,i] <- 0            # prob. of being in initial state dead
      
      logit(phiB[i]) <- (alpha[1]*sex[i]) + (beta[1]*(1-sex[i]))
      logit(psiB_NB[i]) <- (alpha[2]*sex[i]) + (beta[2]*(1-sex[i]))
      logit(phiNB[i]) <- (alpha[3]*sex[i]) + (beta[3]*(1-sex[i]))
      logit(psiNB_B[i]) <- (alpha[4]*sex[i]) + (beta[4]*(1-sex[i]))
      
      # probabilities of state z(t+1) given z(t)
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
    omega[1,1] <- pB         # Pr(alive B t -> captured B t)
    omega[1,2] <- 0          # Pr(alive B t -> captured NB t)
    omega[1,3] <- 1 - pB     # Pr(alive B t -> not-captured t)
    
    omega[2,1] <- 0          # Pr(alive NB t -> captured B t)
    omega[2,2] <- pNB        # Pr(alive NB t -> captured NB t)
    omega[2,3] <- 1 - pNB    # Pr(alive NB t -> not-captured t)
    
    omega[3,1] <- 0          # Pr(dead t -> captured B t)
    omega[3,2] <- 0          # Pr(dead t -> captured NB t)
    omega[3,3] <- 1          # Pr(dead t -> not-captured t)
    
    # Simulating data
    for (i in 1:N) {
      # Initial state
      for (j in f[i]:f[i]) {
        
        z[i, j] ~ dcat(gamma_init[1:3,i])
        
        mydata[i,j] ~ dcat(omega_init[z[i,j],1:3])
      }
      for (j in (f[i]+1):nyears) {
        # State transition
        
        z[i,j] ~ dcat(gamma[z[i,j-1],1:3,i])     
        
        # Observation process
        mydata[i, j] ~ dcat(omega[z[i, j], 1:3])
      }
    }
  })
  
  N = 10000        # Number of individuals
  nyears =  5  # Number of sampling years
  
  f[[s]] <- rep(NA, N)
  for (j in 1:N) {
    f[[s]][j] <- sample(1:nyears,1)
  }
  
  
  sim_model_bs <- nimbleModel(code_sim_bs[[s]], 
                              constants = list(N = N, nyears = nyears, f = f[[s]]),
                              inits = list(alpha = c(-0.7,-3.5,0.5,0.2),
                                           beta = c(0.05, -3.5, 0.05, 0.05) ,
                                           pB=0.2, pNB=0.05,
                                           deltaB = 0.5, deltaNB = 0.5, delta_m=0.3,
                                           beta_init=0.5,alpha_init = 0.5), 
                              calculate = TRUE)
  
  # Get nodes for simulation
  sim_nodes_bs <- sim_model_bs$getDependencies(c("pB","pNB","alpha","beta",
                                                 "beta_init","alpha_init","delta_m",
                                                 "deltaB","deltaNB"),
                                               self = FALSE, downstream = TRUE)
  
  # Simulate the data
  sim_model_bs$simulate(sim_nodes_bs)
  
  cap_bs[[s]] <- sim_model_bs$mydata       # Saving the simulated capture matrix
  cap_bs[[s]][cap_bs[[s]]==3] <- 0
  
  c_sex[[s]] <- sim_model_bs$sex
  
  cap_bs[[s]][is.na(cap_bs[[s]])] <- 0  # NA's to zero's (not captured)
  
  cap_bs[[s]]=data.frame(cap_bs[[s]])   
  c_sex[[s]]=data.frame(c_sex[[s]])
  
  cap_d_bs[[s]] <- cbind( cap_bs[[s]], c_sex[[s]])
  
  # Removing all the rows containing only zeros
  cap_d_bs[[s]]<- cap_d_bs[[s]][!(apply(cap_bs[[s]], 1, function(y) all(y == 0))),]
  
  m_sample <- 5000
  
  random_rows_bs[[s]] <- sample(nrow(cap_d_bs[[s]]),m_sample)
  random_sort_bs[[s]] <- sort(random_rows_bs[[s]])
  cap_df_bs[[s]] <- cap_d_bs[[s]][random_sort_bs[[s]],]
  
  for (k in 1:nyears){
    names(cap_df_bs[[s]])[k]<- k
  }
  names(cap_df_bs[[s]])[names(cap_df_bs[[s]]) == 'c_sex..s..'] <- 'sex'
  
  f_capture_mat[[s]] <- cap_df_bs[[s]][,-nyears-1]      # Final capture matrix
  
  sex_bs[[s]] <- cap_df_bs[[s]]$sex                      # Sex
  n_ind_bs[[s]] = nrow(f_capture_mat[[s]])              # No. of individuals
  
}

## -----------------------------------------------------------------------##
## -----------------------------------------------------------------------##

code_m <- list()
f_1b <- list()

mcmc.multistate_b <- list()

s_a1 <- rep(NA, num_datasets)
s_a2 <- rep(NA, num_datasets)
s_a3 <- rep(NA, num_datasets)
s_a4 <- rep(NA, num_datasets)

s_b1 <- rep(NA, num_datasets)
s_b2 <- rep(NA, num_datasets)
s_b3 <- rep(NA, num_datasets)
s_b4 <- rep(NA, num_datasets)

s_pB <- rep(NA, num_datasets)
s_pNB <- rep(NA, num_datasets)

samples_b <- list()

get.first <- function(x) min(which(x != 0))
#first

for (s in 1:num_datasets){
  code_m[[s]] <- nimbleCode({
    
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

sex.st <- sex_bs[[s]]

y <- f_capture_mat[[s]]

my.data <- list(y = y + 1, sex = sex.st)
f_1b[[s]] <- apply(f_capture_mat[[s]], 1, get_first)


#' Constants in a list. 
## ---------------------------------------------------------------------------------------------------------
my.constants <- list(f_1b = f_1b[[s]], 
                     K = ncol(y), 
                     N = nrow(y))


#' 
#' Initial values without $p_B$. 
## ---------------------------------------------------------------------------------------------------------

s_inits <- sex.st #ifelse(!is.na(Bab_sex)==NA, 1)

s_inits[is.na(sex.st)] <- sample(c(0,1),sum(is.na(sex.st)), replace = TRUE)
s_inits[!is.na(sex.st)] <- NA 

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

mcmc.multistate_b[[s]] <- nimbleMCMC(code = code_b[[s]], 
                                     constants = my.constants,
                                     data = my.data,              
                                     inits = initial.values,
                                     monitors = parameters.to.save,
                                     niter = n.iter,
                                     nburnin = n.burnin, 
                                     nchains = n.chains,
                                     summary = TRUE)

samples_b[[s]]<- mcmc.multistate_b[[s]]$samples

s_a1[s] <- MCMCsummary(samples_b[[s]], params = "alpha[1]", round=5)$mean
s_a2[s] <- MCMCsummary(samples_b[[s]], params = "alpha[2]", round=5)$mean
s_a3[s] <- MCMCsummary(samples_b[[s]], params = "alpha[3]", round=5)$mean
s_a4[s] <- MCMCsummary(samples_b[[s]], params = "alpha[4]", round=5)$mean

s_b1[s] <- MCMCsummary(samples_b[[s]], params = "beta[1]", round=5)$mean
s_b2[s] <- MCMCsummary(samples_b[[s]], params = "beta[2]", round=5)$mean
s_b3[s] <- MCMCsummary(samples_b[[s]], params = "beta[3]", round=5)$mean
s_b4[s] <- MCMCsummary(samples_b[[s]], params = "beta[4]", round=5)$mean

s_pB <-  MCMCsummary(samples_b[[s]], params = "p_B", round=5)$mean
s_pNB <-  MCMCsummary(samples_b[[s]], params = "p_NB", round=5)$mean

}

d_b = data.frame(s_a1, s_a2, s_a3, s_a4,
                 s_b1, s_b2, s_b3, s_b4, s_pB, s_pNB)

write.csv(d_b, file = "model3_sim_dcat.csv")
