data {
  int<lower=0> N; // number of trials
  int<lower = 0> lower_bound;
  int<lower = lower_bound + 1> upper_bound;
  vector<lower = lower_bound, upper = upper_bound>[N] first_rating;
  vector<lower = lower_bound, upper = upper_bound>[N] group_rating;
  array[N] int<lower = lower_bound, upper = upper_bound> second_rating;
}



transformed data {
  vector[N] alpha;
  vector[N] beta;
  
  array[N] int second_rating_tr;
  
  // preparing vectors with the shape for the beta distribution
  alpha = first_rating + group_rating - 2 * lower_bound; // the rating (how many...)
  beta = rep_vector(2*(upper_bound - lower_bound), N);             // out of how many (trials)
  
  // subtracting the lower bound from the second rating so the lowest rating is 0
  for(i in 1:N){
    second_rating_tr[i] = second_rating[i] - lower_bound;
  }
}

parameters {
  real loginvtemp;
  real logweight1;
  real logweight2;
}

transformed parameters {
  // constraining inverse temperature to be above 0
  real<lower=0> invtemp = exp(loginvtemp); 
  
  // constraining weights to be between 0 and 1
  real<lower=0, upper> weight1 = inv_logit(logweight1); 
  real<lower=0, upper> weight2 = inv_logit(logweight2); 
}


model {
  vector[N] belief;
  
  loginvtemp ~normal(0, 1);
  logweight1 ~normal(0, 1);
  logweight2 ~normal(0, 1);
  
  // insert here!!
}

generated quantities {
  // for model comparison
  array[N] real log_lik;


  
  // priors
  prior_invtmp real;
  prior_invtmp = exp(normal_rng(0, 1))
  
  prior_weight1 real;
  prior_weight1 = inv_logit(normal_rng(0, 1))
  
  prior_weight2 real;
  prior_weight2 = inv_logit(normal_rng(0, 1))
}

