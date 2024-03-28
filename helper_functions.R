simple_bayes_beta_binomial <- function(source1, source2, inverse_temperature=1, lower_bound=1, upper_bound= 8){
  n_rows = length(source1)
  # calculate alpha for beta distribution
  alpha <- inverse_temperature * (source1 + source2 - 2 * lower_bound)
  
  # calculating beta for the beta distribution
  beta <- inverse_temperature * (2 * (upper_bound - lower_bound)) - alpha
  
  # sample from a beta distribution
  belief <- rbeta(n_rows, alpha, beta)
  
  # predict the rating
  second_rating <- lower_bound + rbinom(n_rows, upper_bound - lower_bound, belief)
  
  return(second_rating)
}


weighted_bayes_beta_binomial <- function(source1, source2, weight1, weight2, inverse_temperature=1, lower_bound=1, upper_bound= 8){
  n_rows = length(source1)
  # calculate alpha for beta distribution
  
  # NOTE: check course notes, riccardo does something where he scales the weights??
  ## From course notes  w1 <- (w1 - 0.5)*2 and w2 <- (w2 - 0.5)*2
  alpha <- inverse_temperature * (weight1 * source1 + weight2 * source2 - 2 * lower_bound)
  
  # calculating beta for the beta distribution
  beta <- inverse_temperature * (2 * (upper_bound - lower_bound)) - alpha
  
  # sample from a beta distribution
  belief <- rbeta(n_rows, alpha, beta)
  
  # predict the rating
  second_rating <- lower_bound + rbinom(n_rows, upper_bound - lower_bound, belief)
  
  return(second_rating)
}