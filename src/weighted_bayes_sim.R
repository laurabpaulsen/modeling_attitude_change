## This script simulates data for three groups of agents with an 
## inverse temperature of 0.25, 0.5 and 0.75.
## The group level posterior is plotted and the true value


# loading function from helper functions
source("helper_functions.R")

pacman::p_load(cmdstanr)


for(weight1 in c(0.5, 1, 1.5)){
  # weight1 is between 0 and 2 and is the weight given to the first source
  # weight 2 is 2 - weight1 and is the weight given to the second source
  # weight1 + weight2 = 2

  # parameters
  n_trials <- 100
  n_subjects <- 40
    
  # matrices used for saving the data
  first_rating_matrix <- matrix(NA, nrow = n_trials, ncol = n_subjects)
  group_rating_matrix <- matrix(NA, nrow = n_trials, ncol = n_subjects)
  second_rating_matrix <- matrix(NA, nrow = n_trials, ncol = n_subjects)
    
  for(i in 1:n_subjects){
    # subject level difference from the group mean weight
    subj_diff <- rnorm(1, 0, 0.01)
    weight1_subj <- weight1 + subj_diff
      
    # generate responses for first rating and the group level
    first_rating <-  as.integer(runif(n_trials, min = 1, max = 8))
    group_rating <-  as.integer(runif(n_trials, min = 1, max = 8))


    # generate second rating
    second_rating <- weighted_bayes_beta_binomial(
      first_rating, group_rating, 
      weight1 = weight1_subj,
      lower_bound=1, upper_bound = 8
    )
      
    # save data to matrices
    first_rating_matrix[, i] <- first_rating
    group_rating_matrix[, i] <- group_rating
    second_rating_matrix[, i] <- second_rating
  }
    
  # fit model
  weighted_betabayes <- cmdstan_model("mdls/weighted_bayes_betabinomial.stan")
    
  fit <- weighted_betabayes$sample(
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
  fit$save_object(file = paste0("fits/weighted_bayes_sim_", weight1, ".RData"))

}