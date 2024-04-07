data {
  int<lower = 0> N; // number of trials
  int<lower = 0> N_subj; //number of subjects
  int<lower = 0> lower_bound;
  int<lower = lower_bound + 1> upper_bound;
  array[N, N_subj] int<lower = lower_bound, upper = upper_bound> first_rating;
  array[N, N_subj] int<lower = lower_bound, upper = upper_bound> group_rating;
  array[N, N_subj] int<lower = lower_bound, upper = upper_bound> second_rating;
}



transformed data {
  array[N_subj] vector[N] alpha;
  array[N_subj] vector[N]  beta;
  
  array[N, N_subj] int<lower = lower_bound-1, upper = upper_bound-1> second_rating_tr;
  
  for(subj in 1:N_subj){
    // preparing vectors with the shape for the beta distribution for each participant
    alpha[subj] = to_vector(first_rating[:, subj]) + to_vector(group_rating[:, subj]) - 2 * lower_bound; // the rating (how many...)
    beta[subj] = rep_vector(2*(upper_bound - lower_bound), N);             // out of how many (trials)
    
    // subtracting the lower bound from the second rating
    // as we want binomial(...) = 0 to correspond to the lowest rating option
    
    for(i in 1:N){
      second_rating_tr[i, subj] = second_rating[i, subj] - lower_bound;
    }
  }

}

parameters {
  // group level hyperparameters 
  real mu_logbias;
  real sd_logbias;
  
  // subject level parameters
  array[N_subj] real logbias;
}

transformed parameters {
  // constraining inverse temperature to be above 0
  array[N_subj] real<lower=0> bias;
  
  for (subj in 1:N_subj){
    bias[subj] = inv_logit(mu_logbias + sd_logbias * logbias[subj]); 
  }
}


model {
  
  // group-level parameters
  mu_logbias ~ normal(0, 1);
  sd_logbias ~ normal(0, 0.2);
  
  // subject-level parameters
  logbias ~ normal(0, 1);
  
  
  // looping over subjects and using the beta_binomial to predict the rating
  for(subj in 1:N_subj){
    second_rating_tr[:, subj] ~ beta_binomial(rep_array(upper_bound - lower_bound, N), 1 + alpha[subj] * bias[subj], 1 + (beta[subj] - alpha[subj]) * bias[subj]);
  }
}

generated quantities {
  // priors
  real prior_bias;
  //                    grouplvl mu        grouplvl sd          subj lvl param
  prior_bias = inv_logit(normal_rng(0, 1) + normal_rng(0, 0.2) * normal_rng(0, 1));


  // posterior
  real posterior_bias;
  posterior_bias = inv_logit(mu_logbias);
  
  
  // for model comparison
  array[N_subj, N] real log_lik;
  
  for (subj in 1:N_subj){
    
    // loop over trials for each particpant
    for (n in 1:N){  
      log_lik[subj, n] =  beta_binomial_lpmf(second_rating_tr[n, subj] | (upper_bound - lower_bound), 1 + alpha[subj, n] * bias[subj], 1 + (beta[subj, n] - alpha[subj, n]) * bias[subj]);
    }
  }
}

