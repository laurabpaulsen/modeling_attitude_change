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
    
      // subtracting the lower bound from the second rating WHY???
    for(i in 1:N){
      second_rating_tr[i, subj] = second_rating[i, subj] - lower_bound;
    }
  }

}

parameters {
  // group level hyperparameters 
  real mu_loginvtemp;
  real sd_loginvtemp;
  
  // subject level parameters
  array[N_subj] real loginvtemp;
}

transformed parameters {
  // constraining inverse temperature to be above 0
  array[N_subj] real<lower=0> invtemp;
  
  for (subj in 1:N_subj){
  invtemp[subj] = exp(mu_loginvtemp + sd_loginvtemp * loginvtemp[subj]); 
  }
}


model {
  
  // group-level parameters
  mu_loginvtemp ~ normal(0, 1);
  sd_loginvtemp ~ normal(0, 0.2);
  
  // subject-level parameters
  loginvtemp ~ normal(0, 1.0);
  
  
  // looping over subjects and using the beta_binomial to predict the rating
  for(subj in 1:N_subj){
    second_rating_tr[:, subj] ~ beta_binomial(rep_array(upper_bound - lower_bound, N), 1 + alpha[subj] * invtemp[subj], 1 + (beta[subj] - alpha[subj]) * invtemp[subj]);
  }
  
}

generated quantities {
  // priors
  real prior_invtemp;
  prior_invtemp = exp(normal_rng(0, 1)); // FIX: DO WE ALSO NEED TO INCLUDE THE SUBJECT LEVEL PRIOR IN SOME WAY?
  
  // posterior
  real posterior_invtemp;
  posterior_invtemp = inv_logit(mu_loginvtemp);
  
  
  
  // for model comparison
  array[N, N_subj] real log_lik;
  
  
  // FIX: REMEBER TO INCLUDE THE GROUP LEVEL HERE AS WELL!!!!!
  for (subj in 1:N_subj){
    for (n in 1:N){  
      log_lik[n, subj] =  beta_binomial_lpmf(second_rating_tr[:, subj] | (upper_bound - lower_bound), 1 + alpha[subj, n] * invtemp[subj], 1 + (beta[subj, n] - alpha[subj, n]) * invtemp[subj]);
    }
  }
  
  

  
}

