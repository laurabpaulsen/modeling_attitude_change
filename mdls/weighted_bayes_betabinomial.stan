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
  
  // subject level parameters
  array[N_subj] real logweight1;

}

transformed parameters {
  // constraining the weight to be between 0 and 1
  array[N_subj] real<lower=0> weight1;
  array[N_subj] real<lower=0> weight2;
  
  for (subj in 1:N_subj){
    weight1[subj] = inv_logit(mu_logweight1 + sd_logweight1 * logweight1[subj])*2; 
    weight2[subj] = 2 - weight1[subj];
  }
}


model {
  // group-level parameters
  mu_logweight1 ~ normal(0, 1);
  sd_logweight1 ~ normal(0, 0.2);
  
  // subject-level parameters
  logweight1 ~ normal(0, 1);

  vector[N] shape1;
  vector[N] shape2;
    
  // looping over subjects and using the beta_binomial to predict the rating
  for(subj in 1:N_subj){
    shape1 = weight1[subj] * to_vector(first_rating[:, subj]) + weight2[subj] * to_vector(group_rating[:, subj]) - 2 * lower_bound;
    shape2 = rep_vector(2 * (upper_bound - lower_bound), N);

    second_rating_tr[:, subj] ~ beta_binomial(rep_array(upper_bound - lower_bound, N), 1 + shape1, 1 + (shape2 - shape1));
  }
}

generated quantities {
  // priors
  real prior_weight;
  //                       grouplvl mu        grouplvl sd          subj lvl param
  prior_weight = inv_logit(normal_rng(0, 1) + normal_rng(0, 0.2) * normal_rng(0, 1))*2;


  // posterior
  real posterior_weight;
  posterior_weight = inv_logit(mu_logweight1)*2;
  
  
  // for model comparison
  array[N_subj, N] real log_lik;
  vector[N] shape1;
  vector[N] shape2;
  
  for (subj in 1:N_subj){

    shape1 = weight1[subj] * to_vector(first_rating[:, subj]) + weight2[subj] * to_vector(group_rating[:, subj]) - 2 * lower_bound;
    shape2 = rep_vector(2 * (upper_bound - lower_bound), N);

    
    // loop over trials for each particpant
    for (n in 1:N){  
      log_lik[subj, n] =  beta_binomial_lpmf(second_rating_tr[n, subj] | (upper_bound - lower_bound), 1 + shape1, 1 + (shape2 - shape1));
      }
  }
}

