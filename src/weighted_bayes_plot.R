# this script is used for plotting the results from running the simple_bayes_sim.R and simple_bayes_data.R scripts

pacman::p_load(ggplot2, cmdstanr, bayesplot, posterior)

# plots for the simple bayes model on simulated data
weight <- c(0.5, 1, 1.5)

for(i in weight){
    file_name <- paste0("fits/weighted_bayes_sim_", i, ".RData")
    fit <- readRDS(file_name)
  
    # extract the posterior
    posterior <- fit$draws()

    fit_df <- as_draws_df(posterior)
  
    ggplot(fit_df) +
    # plot the prior density
    geom_density(aes(x = prior_weight), fill = "skyblue", alpha = 0.5) +
    # plot the posterior density
    geom_density(aes(x = posterior_weight), fill = "red", alpha = 0.5) +
    
    labs(title = paste0("Prior and posterior density for weight1 = ", i),
           x = "Weight1",
           y = "Density") +

    xlim(0, 2) +

    theme_bw()
  
    # save the plot
    ggsave(paste0("fig/weighted_bayes_sim_", i, ".png"), create.dir = TRUE)
}



# Plots from the real data
#file_name <- "fits/weighted_bayes_data.RData"
#fit <- readRDS(file_name)

# extract the posterior
#posterior <- fit$draws()

#draws_df <- as_draws_df(posterior)

#ggplot(draws_df) +
# plot the prior density
#geom_density(aes(x = prior_bias), fill = "skyblue", alpha = 0.5) +
# plot the posterior density
#geom_density(aes(x = posterior_bias), fill = "red", alpha = 0.5) +

#labs(title = "Prior and posterior density for bias on data from Simonsen et al.",
#       x = "Bias",
#       y = "Density") +

#xlim(0, 1) +

#theme_bw()

# save the plot
#ggsave("fig/simple_bayes_data.png", create.dir = TRUE)

# make trace plots for data
#ggplot(draws_df, aes(.iteration, posterior_bias, group = .chain, color = .chain)) +
#  geom_line() +
#  theme_bw()

# save the plot
#ggsave("fig/simple_bayes_data_trace.png", create.dir = TRUE, width = 10, height = 5)

#png("fig/simple_bayes_data_PSIS.png", width = 800, height = 400)
# print log likelihood
#loo <- fit$loo(save_psis = TRUE, cores = 4)
#plot(loo)