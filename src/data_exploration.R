# Data exploration script to visualise changes after first and second ratings

pacman::p_load(tidyverse)

# load the data
data <- read_csv("data/Simonsen_clean.csv")


data$ID <- as.factor(data$ID)
mean_group_rating <- mean(data$GroupRating)
# make data long format
data_long <- data %>%
  pivot_longer(cols = c(FirstRating, GroupRating, SecondRating), names_to = "RatingType", values_to = "Rating")

# plot the data for first and second rating, making FirstRating = 1 and SecondRating = 2
data_long %>% filter(RatingType %in% c("FirstRating", "SecondRating")) %>%
  mutate(RatingType = ifelse(RatingType == "FirstRating", 1, 2)) %>%
  ggplot(aes(x = RatingType, color=ID)) +
    scale_x_continuous(breaks = c(1, 2), labels = c("Mean First Rating", "Mean Second Rating")) +
    geom_smooth(aes(y = Rating), method = "lm", se = FALSE, xseq = c(1, 2)) +
    geom_hline(yintercept = mean_group_rating, linetype = "dashed", show.legend = TRUE) +
    labs(title = "First, group and second rating",
         x = "Rating type",
         y = "Rating") +
    theme_bw()

# subtract the group rating from the first and second rating by FaceID
data <- data %>%
  group_by(ID, FaceID) %>%
  mutate(FirstRating = FirstRating - GroupRating,
         SecondRating = SecondRating - GroupRating) %>%
  ungroup()
head(data)  

# make data long format
data_long <- data %>%
  pivot_longer(cols = c(FirstRating, SecondRating), names_to = "RatingType", values_to = "Rating")

# plot the data for first and second rating, making FirstRating = 1 and SecondRating = 2
data_long %>% filter(RatingType %in% c("FirstRating", "SecondRating")) %>%
  mutate(RatingType = ifelse(RatingType == "FirstRating", 1, 2)) %>%
  ggplot(aes(x = RatingType, color=ID)) +
    scale_x_continuous(breaks = c(1, 2), labels = c("First Rating Mean", "Second Rating Mean")) +
    geom_smooth(aes(y = Rating), method = "lm", se = FALSE, xseq = c(1, 2), alpha=0.6, show.legend = FALSE) +
    geom_hline(yintercept = 0, linetype = "dashed", show.legend = FALSE) +
    labs(title = "Mean [Rating - Group Rating] by Subject",
         x = "",
         y = "Rating") +
    theme_bw()
