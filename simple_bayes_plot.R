# this script is used for plotting the results from running the simple_bayes_sim.R and simple_bayes_data.R scripts

pacman::p_load(ggplot2, cmdstanr, bayesplot, posterior)

# plots for the simple bayes model on simulated data
invtemp_mu <- c(0.25, 0.5, 0.75)

for(i in invtemp_mu){
    file_name <- paste0("fits/simple_bayes_sim_", i, ".RData")
    fit <- readRDS(file_name)
  
    # extract the posterior
    posterior <- fit$draws()

    fit_df <- as_draws_df(posterior)
  
    ggplot(fit_df) +
    # plot the prior density
    geom_density(aes(x = prior_bias), fill = "skyblue", alpha = 0.5) +
    # plot the posterior density
    geom_density(aes(x = posterior_bias), fill = "red", alpha = 0.5) +
    
    labs(title = paste0("Prior and posterior density for bias = ", i),
           x = "Bias",
           y = "Density") +

    xlim(0, 1) +

    theme_bw()
  
    # save the plot
    ggsave(paste0("fig/simple_bayes_sim_", i, ".png"), create.dir = TRUE)
}



# Plots from the real data
file_name <- "fits/simple_bayes_data.RData"
fit <- readRDS(file_name)

# extract the posterior
posterior <- fit$draws()

draws_df <- as_draws_df(posterior)

ggplot(draws_df) +
# plot the prior density
geom_density(aes(x = prior_bias), fill = "skyblue", alpha = 0.5) +
# plot the posterior density
geom_density(aes(x = posterior_bias), fill = "red", alpha = 0.5) +

labs(title = "Prior and posterior density for bias on data from Simonsen et al.",
       x = "Bias",
       y = "Density") +

xlim(0, 1) +

theme_bw()

# save the plot
ggsave("fig/simple_bayes_data.png", create.dir = TRUE)

# make trace plots for data
ggplot(draws_df, aes(.iteration, posterior_bias, group = .chain, color = .chain)) +
  geom_line() +
  theme_bw()

# save the plot
ggsave("fig/simple_bayes_data_trace.png", create.dir = TRUE, width = 10, height = 5)
