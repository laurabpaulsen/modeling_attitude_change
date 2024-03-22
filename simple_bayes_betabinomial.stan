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
  
  // subtracting the lower bound from the second rating WHY???
  for(i in 1:N){
    second_rating_tr[i] = second_rating[i] - lower_bound;
  }
}

parameters {
  real loginvtemp;
}

transformed parameters {
  // constraining inverse temperature to be above 0
  real<lower=0> invtemp = exp(loginvtemp); 
}


model {
  vector[N] belief;
  
  loginvtemp ~normal(0, 1);
  
  
  // implementation here does not work atm
  //belief ~ beta(1 + alpha * invtemp, 1 + (beta - alpha) * invtemp);
  //second_rating_tr ~ binomial(rep_array(upper_bound - lower_bound, N), belief);
  
  // note there is also a beta_binomial function in STAN -> maybe implement using this instead?
  second_rating_tr ~ beta_binomial(rep_array(upper_bound - lower_bound, N), 1 + alpha * invtemp, 1 + (beta - alpha) * invtemp);
  
  
}

generated quantities {
  // for model comparison
  real log_lik;
  log_lik = beta_binomial_lpmf(second_rating_tr | rep_array(upper_bound - lower_bound, N), 1 + alpha * invtemp, 1 + (beta - alpha) * invtemp);

}

