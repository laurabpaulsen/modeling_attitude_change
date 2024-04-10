## This script simulates data for three groups of agents with an 
## inverse temperature of 0.25, 0.5 and 0.75.
## The group level posterior is plotted and the true value


# loading function from helper functions
source("helper_functions.R")

pacman::p_load(cmdstanr)


for(bias_mu in c(0.25, 0.5, 0.75)){

  # parameters
  n_trials <- 153
  n_subjects <- 40
  
  # matrices used for saving the data
  first_rating_matrix <- matrix(NA, nrow = n_trials, ncol = n_subjects)
  group_rating_matrix <- matrix(NA, nrow = n_trials, ncol = n_subjects)
  second_rating_matrix <- matrix(NA, nrow = n_trials, ncol = n_subjects)
  
  for(i in 1:n_subjects){
    # subject level difference from the group mean bias
    subj_diff <- rnorm(1, 0, 0.01)

    # subejct level bias
    bias_subj <- bias_mu + subj_diff

    # check that bias is below 1 and above 0
    if(bias_subj > 1){
      bias_subj <- 0.99
    }
    if(bias_subj < 0){
      bias_subj <- 0.01
    }
    
    # generate responses for first rating and the group level
    first_rating <-  as.integer(runif(n_trials, min = 1, max = 8))
    group_rating <-  as.integer(runif(n_trials, min = 1, max = 8))


    # generate second rating
    second_rating <- simple_bayes_beta_binomial(
      first_rating, group_rating, 
      bias = bias_subj, 
      lower_bound=1, upper_bound = 8
    )
    
    # save data to matrices
    first_rating_matrix[, i] <- first_rating
    group_rating_matrix[, i] <- group_rating
    second_rating_matrix[, i] <- second_rating
  }
  
  # fit model
  simple_betabayes <- cmdstan_model("mdls/simple_bayes_betabinomial.stan")
  
  fit <- simple_betabayes$sample(
    data = list(N = n_trials,
                N_subj = n_subjects,
                lower_bound = 1,
                upper_bound = 8,
                first_rating = first_rating_matrix,
                group_rating = group_rating_matrix,
                second_rating = second_rating_matrix
    )
  )
  # save the posterior
  fit$save_object(file = paste0("fits/simple_bayes_sim_", bias_mu, ".RData"))
}