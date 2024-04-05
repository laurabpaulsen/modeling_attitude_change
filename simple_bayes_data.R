# This script runs the simple bayes model on the data from 
# Simonsen et al and saves the output

pacman::p_load(cmdstanr, tidyverse)

data <- read_csv("data/Simonsen_clean.csv")

# REMEMBER TO DELETE TO RUN ON ALL PARTICPANTS # data from fewer participants for testing 
#data <- data |>
#  filter(ID == sample(unique(ID), 3))

# make a matrices with shape n_trials and n_subjects for first rating, group rating and second rating
unique_subjects <- unique(data$ID)

# initialize matrices to store ratings
n_subjects <- length(unique_subjects)
n_trials <- 153

first_rating_matrix <- matrix(NA, nrow = n_trials, ncol = n_subjects)
group_rating_matrix <- matrix(NA, nrow = n_trials, ncol = n_subjects)
second_rating_matrix <- matrix(NA, nrow = n_trials, ncol = n_subjects)

# Fill in the matrices with ratings
for (i in 1:length(unique_subjects)) {
  subject_data <- data[data$ID == unique_subjects[i], ]
  first_rating_matrix[, i] <- subject_data$FirstRating
  group_rating_matrix[, i] <- subject_data$GroupRating
  second_rating_matrix[, i] <- subject_data$SecondRating
}


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
fit$save_object(file = "fits/simple_bayes_data.RData")
