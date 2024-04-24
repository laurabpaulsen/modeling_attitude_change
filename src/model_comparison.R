pacman::p_load(cmdstanr, tidyverse, loo, bayesplot, brms)

fit_simple <- readRDS("fits/simple_bayes_data.RData")
fit_weighted <- readRDS("fits/weighted_bayes_data.RData")

# compare the models using loo

# get loo for the simple bayes model
loo_simple <- fit_simple$loo(save_psis = TRUE, cores = 4)

# get loo for the weighted bayes model
loo_weighted <- fit_weighted$loo(save_psis = TRUE, cores = 4)

# compare the models
model_comparison <- loo::loo_compare(loo_simple, loo_weighted)


print(model_comparison, simplify=FALSE)

# load the source data to compare predictions
data <- read_csv("data/Simonsen_clean.csv")

# plot predicted vs observed
fit_simple_pds <- fit_simple$draws(format="matrix")
head(fit_simple_pds)

# test a mixture model with brms

brms_model <- brms(
  data = data,
  family = betabinomial(),
  FirstRating | trials(8) ~ 1 + (1 | ID),
  iter = 2000,
  chains = 4,
  cores = 4
)
