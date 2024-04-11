# this script is used for plotting the results from running the simple_bayes_sim.R and simple_bayes_data.R scripts

pacman::p_load(ggplot2, cmdstanr, bayesplot, posterior, tidyverse)

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



# Extracting and preparing subject-level log-likelihoods
subject_log_lik <- fit$draws(variables = "log_lik") %>%
  as_draws_df() %>%
  gather(key = "subject", value = "log_lik", -.draw) %>%
  mutate(subject = as.factor(gsub("log_lik\\[", "", gsub("\\]", "", subject))))

# separate
subject_log_lik <- subject_log_lik %>%
  separate(col = subject, into = c("subject", "trial"), sep = ",", convert = TRUE)

# Get .chain and .iteration out
subject_log_lik <- subject_log_lik %>% filter(!subject %in% c(".chain", ".iteration"))

# Check summary stats
summary(subject_log_lik$log_lik)

# Inspecting for outliers
outliers <- subject_log_lik %>%
  filter(log_lik < -60000 | log_lik > 1000)

# Find outlier
subject_summary <- subject_log_lik %>%
  group_by(subject) %>%
  summarize(MaxLogLik = max(log_lik))

# Identify the subject with the maximum log-likelihood
outlier_subject <- subject_summary %>%
  filter(MaxLogLik == max(MaxLogLik))

# Extract the intercept for the vline from group-level log likelihood mean
group_log_lik <- fit$summary(variables = "group_log_lik_mean")$mean

# Plotting the densities for subject-level log-likelihoods
ggplot(subject_log_lik, aes(x = log_lik, fill = as.factor(subject))) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = group_log_lik, color = "blue", linetype = "dashed", linewidth = 1) +
  labs(title = "Subject-Level Log-Likelihoods with Group-Level Log-Likelihood",
       caption = "Dashed vertical line: Mean of Subjects' Log-Likelihood",
       x = "Log-Likelihood",
       y = "Density",
       fill = "Subject") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold"),
        plot.caption = element_text(color = "blue"))

# save the plot
ggsave("fig/simple_bayes_data_loglik.png", create.dir = TRUE)


png("fig/simple_bayes_data_PSIS.png", width = 800, height = 400)
# print log likelihood
loo <- fit$loo(save_psis = TRUE, cores = 4)
plot(loo)
