simple_bayes_beta_binomial <- function(source1, source2, bias, lower_bound=1, upper_bound=8){
  n_rows = length(source1)
  # calculate alpha for beta distribution (how many)
  alpha <- source1 + source2 - 2 * lower_bound
  
  # calculating beta for the beta distribution (out of how many)
  beta <- 2 * (upper_bound - lower_bound)
  
  # sample from a beta distribution
  belief <- rbeta(n_rows, 1 + alpha * bias, 1 + (beta - alpha) * bias)
  
  # predict the rating
  second_rating <- lower_bound + rbinom(n_rows, upper_bound - lower_bound, belief)
  
  return(second_rating)
}


weighted_bayes_beta_binomial <- function(source1, source2, weight1, lower_bound=1, upper_bound=8){
  # weight1 is between 0 and 2 and is the weight given to the first source
  # weight 2 is 2 - weight1 and is the weight given to the second source
  # weight1 + weight2 = 2
  
  n_rows = length(source1)
  alpha <- weight1 * source1 + (2 - weight1) * source2 - 2 * lower_bound
  
  # calculating beta for the beta distribution
  beta <- 2 * (upper_bound - lower_bound)
  
  # sample from a beta distribution
  belief <- rbeta(n_rows, alpha, beta)
  
  # predict the rating
  second_rating <- as.integer(lower_bound + rbinom(n_rows, upper_bound - lower_bound, belief))
  
  return(second_rating)
}