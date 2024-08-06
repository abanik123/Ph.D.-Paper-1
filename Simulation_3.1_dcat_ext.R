library(nimble)

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

num_datasets <- 2  # Number of datasets to simulate

for (s in 1:num_datasets){
  
  code_sim_bs[[s]] <- nimbleCode({
    # Priors
    delta_m ~ dbeta(1,1)
    
    beta_init ~ dnorm(0,1)
    
    beta[1] ~ dnorm(0,1)
    beta[2] ~ dnorm(0,1)
    beta[3] ~ dnorm(0,1)
    beta[4] ~ dnorm(0,1)
    
    alpha_init ~ dnorm(0,1)
    
    alpha[1] ~ dnorm(0,1)
    alpha[2] ~ dnorm(0,1)
    alpha[3] ~ dnorm(0,1)
    alpha[4] ~ dnorm(0,1)
    
    pB ~ dbeta(1, 1)
    pNB ~ dbeta(1, 1)
    
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
  
  N = 10        # Number of individuals
  nyears =  5  # Number of sampling years
  
  f[[s]] <- rep(NA, N)
  for (j in 1:N) {
    f[[s]][j] <- sample(1:nyears,1)
  }
  
  
  sim_model_bs <- nimbleModel(code_sim_bs[[s]], 
                              constants = list(N = N, nyears = nyears, f = f[[s]]),
                              inits = list(alpha = c(0.23,0.45,0.12,0.11),
                                           beta = c(0.15, 0.22, 0.32, 0.18) ,
                                           pB=0.7, pNB=0.4,
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
  
  m_sample <- 4
  
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

## ---------------------------------------------------------------------------------------------------------
get.first <- function(x) min(which(x != 0))
first <- apply(y, 1, get.first)
#first


#' Actually, the initial state of the dolly varden is known exactly. It is alive at site of initial capture. Therefore, we don't need to try and estimate the probability of initial states. 
## ---------------------------------------------------------------------------------------------------------
multistate <- nimbleCode({
  
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
  
  for (i in 1:N) {
    
    # priors
    phiB[i] ~ dbeta(1, 1)
    phiNB[i] ~ dbeta(1, 1)
    psiB_NB[i] ~ dbeta(1, 1)
    psiNB_B[i] ~ dbeta(1, 1)
    pB[i] ~ dbeta(1, 1)
    pNB[i] ~ dbeta(1, 1)
    
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
    
    # probabilities of y(t) given z(t)
    omega[1,1,i] <- 1 - pB[i]     # Pr(alive B t -> not-captured t)
    omega[1,2,i] <- pB[i]         # Pr(alive B t -> captured B t)
    omega[1,3,i] <- 0             # Pr(alive B t -> captured NB t)
    omega[2,1,i] <- 1 - pNB[i]    # Pr(alive NB t -> not-captured t)
    omega[2,2,i] <- 0             # Pr(alive NB t -> captured B t)
    omega[2,3,i] <- pNB[i]        # Pr(alive NB t -> captured NB t)
    omega[3,1,i] <- 1             # Pr(dead t -> not-captured t)
    omega[3,2,i] <- 0             # Pr(dead t -> captured B t)
    omega[3,3,i] <- 0             # Pr(dead t -> captured NB t)
    
    # latent state at first capture
    z[i,first[i]] <- y[i,first[i]] - 1 # if seen while B (y = 2), state is alive while B(y - 1 = z = 1) with prob = 1
    
    for (t in (first[i]+1):K){         # if seen while NB (y = 3), state is alive while B (y - 1 = z = 2) with prob = 1
      # draw z(t) given z(t-1)
      z[i,t] ~ dcat(gamma[z[i,t-1],1:3,i])
      # draw y(t) given z(t)
      y[i,t] ~ dcat(omega[z[i,t],1:3,i])
    }
  }
})

#' 
#' Data in a list. Remember to add 1. 
## ---------------------------------------------------------------------------------------------------------
my.data <- list(y = y + 1)

#' Constants in a list. 
## ---------------------------------------------------------------------------------------------------------

N = nrow(y)
nyears = ncol(y)

my.constants <- list(first = first, 
                     K = nyears, 
                     N = N)


phi_B <- rep(NA, N)
for (i in 1:N) {
  phi_B[i] <- rbeta(1, 1, 1)
}

phi_NB <- rep(NA, N)
for (i in 1:N) {
  phi_NB[i] <- rbeta(1, 1, 1)
}

psi_B_NB <- rep(NA, N)
for (i in 1:N) {
  psi_B_NB[i] <- rbeta(1, 1, 1)
}

psi_NB_B <- rep(NA, N)
for (i in 1:N) {
  psi_NB_B[i] <- rbeta(1, 1, 1)
}

p_B <- rep(NA, N)
for (i in 1:N) {
  p_B[i] <- rbeta(1, 1, 1)
}

p_NB <- rep(NA, N)
for (i in 1:N) {
  p_NB[i] <- rbeta(1, 1, 1)
}

#' 
#' Initial values without $p_B$. 
## ---------------------------------------------------------------------------------------------------------

zinits <- y
zinits[zinits==0] <- sample(c(1,2), sum(zinits==0), replace = TRUE)

initial.values <- function(){list(phiB = phi_B, 
                                  phiNB = phi_NB , 
                                  psiB_NB = psi_B_NB, 
                                  psiNB_B = psi_NB_B, 
                                  pB = p_B, 
                                  pNB = p_NB, 
                                  z = zinits)}

#' 
#' Parameters to monitor. 
## ---------------------------------------------------------------------------------------------------------
#parameters.to.save <- c("phiB", "phiNB","psiB_NB", "psiNB_B", "pB", "pNB")
#parameters.to.save

#' MCMC settings.
## ---------------------------------------------------------------------------------------------------------
n.iter <- 50000
n.burnin <- 10000
n.chains <- 2

#' 
#' Run nimble.
## ---------------------------------------------------------------------------------------------------------
mcmc.multistate <- nimbleMCMC(code = multistate, 
                              constants = my.constants,
                              data = my.data,              
                              inits = initial.values,
                              #monitors = parameters.to.save,
                              niter = n.iter,
                              nburnin = n.burnin, 
                              nchains = n.chains,
                              summary = TRUE,
                              WAIC = TRUE)

get_WAIC <- mcmc.multistate$WAIC
get_WAIC
get_samples <- mcmc.multistate$samples
get_summary <- mcmc.multistate$summary

#' 
#' Have a look to the results. Note that convergence is better. 
## ---------------------------------------------------------------------------------------------------------
#MCMCsummary(get_samples, round = 3)
MCMCtrace(get_samples,pdf = F, ind = TRUE,
          Rhat = TRUE, n.eff = TRUE)

post_pB <- MCMCsummary(get_samples,params = "pB", round = 3)$mean
#post_pB
post_pNB <- MCMCsummary(get_samples,params = "pNB", round = 3)$mean
#post_pNB
post_phiB <- MCMCsummary(get_samples,params = "phiB", round = 3)$mean
#post_phiB
post_phiNB <- MCMCsummary(get_samples,params = "phiNB", round = 3)$mean
#post_phiNB
post_psiB_NB <- MCMCsummary(get_samples,params = "psiB_NB", round = 3)$mean
#post_psiB_NB
post_psiNB_B <- MCMCsummary(get_samples,params = "psiNB_B", round = 3)$mean
#post_psiNB_B

d <- data.frame(post_pB,post_pNB,post_phiB,post_phiNB,post_psiB_NB,post_psiNB_B)
