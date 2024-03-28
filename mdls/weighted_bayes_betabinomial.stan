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
  real mu_logweight1;
  real sd_logweight1;
  
  real mu_logweight2;
  real sd_logweight2;
  
  // subject level parameters
  array[N_subj] real logweight1;
  array[N_subj] real logweight2;
}

transformed parameters {
  
  //subject level constrained parameters for each weight between 0 an 1
  array[N_subj] real<lower=0, upper> weight1;
  array[N_subj ]real<lower=0, upper> weight2;
  
  // constraining weights to be between 0 and 1
  for (subj in 1:N_subj){
    weight1[subj] = inv_logit(mu_logweight1 + sd_logweight1 * logweight1[subj]); 
    weight2[subj] = inv_logit(mu_logweight2 + sd_logweight2 * logweight2[subj]); 
  }
}


model {
  // group-level parameters
  mu_logweight1 ~ normal(0, 1);
  sd_logweight1 ~ normal(0, 0.2);
  
  mu_logweight2 ~ normal(0, 1);
  sd_logweight2 ~ normal(0, 0.2);
  
  // subject-level parameters
  logweight1 ~ normal(0, 1);
  logweight1 ~ normal(0, 1);
  
  // looping over subjects and using the beta_binomial to predict the rating
  for(subj in 1:N_subj){
    second_rating_tr[:, subj] ~ beta_binomial(rep_array(upper_bound - lower_bound, N), // INSERT SOMETHING HERE!!!!);
  }
  
}

generated quantities {
  // for model comparison
  array[N] real log_lik;
  
   // for model comparison
  array[N_subj] real log_lik;
  
  for (subj in 1:N_subj){
    log_lik[subj] = 0;
    
    // loop over trials for each particpant
    for (n in 1:N){  
      log_lik[subj] +=  beta_binomial_lpmf(second_rating_tr[:, subj] | // INSERT SOMETHING HERE!!);
    }
  }
}


  
  // priors
  prior_invtmp real;
  // generate here
  
  prior_weights real; // same for weight one and two so enough to generate one
  // generate here
}

