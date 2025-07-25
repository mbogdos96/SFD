library(dplyr)
library(ggplot2)
library(ggbeeswarm)

#=================
# Question portion
#=================
# Set seed
set.seed(124153)

# Sample two groups one with n = 3 and one n = 5
sample_1 <- rnorm(3,4,0.8)

sample_2 <- rnorm(5,6,0.1)

# Combine into df
sample_df <- data.frame(value = c(sample_1,
                                  sample_2),
                        sample = c(1,1,1,2,2,2,2,2)) %>%
  group_by(sample) %>%
  mutate(mean = mean(value),
         stdd = sd(value),
         val_max = mean + stdd,
         val_min = mean - stdd,
         sample = as.factor(sample))

# Plot the two samples with error bar being sd
sample_plot <- ggplot(data = sample_df) +
  geom_errorbar(aes(x = sample,
                    ymax = val_max,
                    ymin = val_min),
                width = 0.2,
                color = "gray",
                linewidth = 1) +
  geom_beeswarm(aes(x = sample, y = value),
             size = 2) +
  theme_classic()

print(sample_plot)