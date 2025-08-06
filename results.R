library(dplyr)
library(ggplot2)
library(tidyr)

# Import the results
quiz_results <- read.csv("results.csv")

# Reshape the df
quiz_long <- quiz_results %>%
  pivot_longer(cols = starts_with("X"), 
               names_to = "question", 
               values_to = "answer") %>%
  mutate(question = as.numeric(gsub("X", 
                                    "", 
                                    question))) %>%
  arrange(Name, 
          question) %>%
  group_by(Name) %>%
  mutate(cum_score = cumsum(answer))

cumul_score_plot <- ggplot(quiz_long, 
                           aes(x = question, 
                               y = cum_score, 
                               group = Name, 
                               color = Name)) +
  geom_hline(yintercept = 24) +
  geom_line(size = 1) +
  labs(x = "Question Number",
    y = "Cumulative Score") +
  ylim(0,24) +
  facet_wrap(~ Name) +
  theme_classic() +
  theme(legend.position = "none")

# Get avg final score
avg_final <- mean((quiz_long %>%
                     ungroup() %>%
                     filter(question == 10))$cum_score)

sd_final <- sd((quiz_long %>%
                     ungroup() %>%
                     filter(question == 10))$cum_score)