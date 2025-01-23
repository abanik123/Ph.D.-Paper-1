library(nimble)
library(MCMCvis)

# Defining lists
code_sim_b <- list()
f <- list()

cap_b <- list()
cap_d_b <- list()
cap_df_b <- list()
capture_mat_b <- list()

random_rows_b <- list()
random_sort_b <- list()
n_ind_b <- list()

# Create a list to store simulated datasets
simulated_datasets <- list()


# Define the model code

num_datasets <- 10000  # Number of datasets to simulate

for (s in 1:num_datasets){
  
  code_sim_b[[s]] <- nimbleCode({
    # Priors
    piB = 0.5       #Assuming
    
    phiB = 0.1
    phiNB = 0.3
    
    psiB_NB = 0.1
    psiNB_B = 0.3
    
    pB = 0.2
    pNB = 0.05
    
    deltaNB ~ dbeta(1,1)
    deltaB ~ dbeta(1, 1)
    
    #deltaB = 0.5
    #deltaNB = 0.5
    
    # Probabilities of being in initial state
    
    gamma_init[1] <- piB # prob. of being in initial state B
    gamma_init[2] <- 1 - piB # prob. of being in initial state NB
    gamma_init[3] <- 0 # prob. of being in initial state dead
    
    omega_init[1,1] <- deltaB
    omega_init[1,2] <- 0
    omega_init[1,3] <- 1 - deltaB
    
    omega_init[2,1] <- 0
    omega_init[2,2] <- deltaNB
    omega_init[2,3] <- 1 - deltaNB
    
    omega_init[3,1] <- 0
    omega_init[3,2] <- 0
    omega_init[3,3] <- 1
    
    
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
        
        z[i, j] ~ dcat(gamma_init[1:3])
        
        mydata[i,j] ~ dcat(omega_init[z[i,j],1:3])
      }
      for (j in (f[i]+1):nyears) {
        # State transition
        
        z[i, j] ~ dcat(gamma[z[i, j - 1], 1:3])
        
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
  
  sim_model_b <- nimbleModel(code_sim_b[[s]], 
                             constants = list(N = N, nyears = nyears, f = f[[s]]),
                             inits = list(phiB = 0.1, phiNB = 0.3,
                                          pB=0.2, pNB=0.05,
                                          psiB_NB=0.1, psiNB_B=0.3,
                                          deltaB = 0.5, deltaNB = 0.5,
                                          piB = 0.5), calculate = TRUE)
  
  # Set initial parameter values
  #sim_model$phi <- runif(1, 0.4, 0.6)
  #sim_model$p <- runif(1, 0.3, 0.5)
  
  # Initialize the initial state for each individual
  #initial_states <- sample(1:2, N, replace = TRUE)
  #sim_model$alive[,1] <- initial_states
  
  # Get nodes for simulation
  sim_nodes_b <- sim_model_b$getDependencies(c("pB","pNB",
                                               "phiB","phiNB",
                                               "psiB_NB","psiNB_B",
                                               "piB","deltaB","deltaNB"), self = FALSE,
                                             downstream = TRUE)
  
  # Simulate the data
  sim_model_b$simulate(sim_nodes_b)
  
  cap_b[[s]] <- sim_model_b$mydata       # Saving the simulated capture matrix
  cap_b[[s]][cap_b[[s]]==3] <- 0
  
  cap_b[[s]][is.na(cap_b[[s]])] <- 0  # NA's to zero's (not captured)
  
  cap_b[[s]]=data.frame(cap_b[[s]])   
  
  # Removing all the rows containing only zeros
  cap_d_b[[s]]<- cap_b[[s]][!(apply(cap_b[[s]], 1, function(y) all(y == 0))),]
  
  m_sample <- 5000
  
  random_rows_b[[s]] <- sample(nrow(cap_d_b[[s]]),m_sample)
  random_sort_b[[s]] <- sort(random_rows_b[[s]])
  cap_df_b[[s]] <- cap_d_b[[s]][random_sort_b[[s]],]
  
  for (k in 1:nyears){
    names(cap_df_b[[s]])[k]<- k
  }
  
  capture_mat_b[[s]] <- cap_df_b[[s]]  # Final capture matrix
  n_ind_b[[s]] = nrow(capture_mat_b[[s]])
}

#################################################################

code_b <- list()
f_1b <- list()

x.init_b <- list()

zinits_b <- list()

mcmc.multistate_b <- list()

s_pB <- rep(NA, num_datasets)
s_pNB <- rep(NA, num_datasets)
s_phiB <- rep(NA, num_datasets)
s_phiNB <- rep(NA, num_datasets)
s_psiB_NB <- rep(NA, num_datasets)
s_psiNB_B <- rep(NA, num_datasets)

samples_b <- list()

summary_m1b <- list()

#y <- list()

# Function to get the index of the first non-zero value in a row
get_first_non_zero_index <- function(row) {
  non_zero_indices <- which(row != 0)
  if (length(non_zero_indices) > 0) {
    return(non_zero_indices[1])
  } else {
    return(NA)
  }
}

# Function to count zeros in a vector while handling NA values
count_zeros <- function(x) {
  sum(x == 0, na.rm = TRUE)
}

for (s in 1:num_datasets){
  
  code_b[[s]] <- nimbleCode({
    # Priors
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
    
    # Simulating data
    for (i in 1:N) {
      # Initial state
      for (j in f_1b[i]:f_1b[i]) {
        z[i,j] <- y[i,j] - 1
      }
      for (j in (f_1b[i]+1):nyears) {
        # State transition
        
        z[i, j] ~ dcat(gamma[z[i, j - 1], 1:3])
        
        # Observation process
        y[i, j] ~ dcat(omega[z[i, j], 1:3])
      }
    }
  })
  
  y <- capture_mat_b[[s]]
  #y <- y[[s]]
  
  my.data = list(y=y + 1)
  # Apply the function to each row
  f_1b[[s]] <- apply(capture_mat_b[[s]], 1, get_first_non_zero_index)
  
  N = 5000
  # Generate inits for the latent states
  x.init_b[[s]] <- capture_mat_b[[s]]
  for (i in 1:N){
    if (f_1b[[s]][i] == 1) next
    if (f_1b[[s]][i] > 1) x.init_b[[s]][i,1:(f_1b[[s]][i]-1)] <- NA
  }
  
  #x.init[[s]][x.init[[s]]==1] <- 2
  x.init_b[[s]][x.init_b[[s]]==0] <- 
    sample(c(1,2), sum(sapply(x.init_b[[s]], count_zeros)), replace = TRUE)
  #x.init[[s]][x.init[[s]]==2] <- 0
  
  
  zinits_b[[s]] <- as.matrix(x.init_b[[s]])
  
  #' Constants in a list. 
  ## ---------------------------------------------------------------------------------------------------------
  my.constants <- list(N = N, nyears = nyears, f_1b = f_1b[[s]])
  
  initial.values <- function(){list(phiB = runif(1, 0, 1),
                                    phiNB = runif(1, 0, 1),
                                    pB = runif(1, 0, 1),
                                    pNB = runif(1, 0, 1),
                                    psiB_NB = runif(1,0,1),
                                    psiNB_B = runif(1,0,1),
                                    z = zinits_b[[s]])}
  
  #' Parameters to monitor. 
  ## ---------------------------------------------------------------------------------------------------------
  parameters.to.save <- c("phiB","phiNB", "pB", "pNB","psiB_NB", "psiNB_B")
  
  #' MCMC settings.
  ## ---------------------------------------------------------------------------------------------------------
  n.iter <- 200000
  n.burnin <- 50000
  n.chains <- 2
  
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
  
  s_pB[s] <- MCMCsummary(samples_b[[s]], params = "pB", round=5)$mean
  s_pNB[s] <- MCMCsummary(samples_b[[s]], params = "pNB", round=5)$mean
  s_phiB[s] <- MCMCsummary(samples_b[[s]], params = "phiB", round=5)$mean
  s_phiNB[s] <- MCMCsummary(samples_b[[s]], params = "phiNB", round=5)$mean
  s_psiB_NB[s] <- MCMCsummary(samples_b[[s]], params = "psiB_NB", round=5)$mean
  s_psiNB_B[s] <- MCMCsummary(samples_b[[s]], params = "psiNB_B", round=5)$mean
  
}

d_b = data.frame(s_pB, s_pNB, s_phiB, s_phiNB, s_psiB_NB, s_psiNB_B)
write.csv(d_b, file = "model2_sim_dcat.csv")
