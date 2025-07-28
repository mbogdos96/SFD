library(dplyr)
library(ggplot2)
library(ggbeeswarm)
library(patchwork)

# Generate some data
# Set seed
set.seed(45)

# Sample first
sample1 <- data.frame(ROS = rnorm(25,4,0.5)) %>%
  mutate(sample = "Formaldehyde 100 uM")

# Sample second
sample2 <- data.frame(ROS = rnorm(25,7,0.75)) %>%
  mutate(sample = "Acetaldehyde 100 uM")

# Combine dfs
samples <- rbind(sample1,
                 sample2) %>%
  group_by(sample) %>%
  mutate(mean_ROS = mean(ROS),
         sd_ROS = sd(ROS),
         se_ROS = sd_ROS/sqrt(25),
         ci_ROS = se_ROS*1.96,
         ROS_upr_sd = mean_ROS + sd_ROS,
         ROS_lwr_sd = mean_ROS - sd_ROS,
         ROS_upr_se = mean_ROS + se_ROS,
         ROS_lwr_se = mean_ROS - se_ROS,
         ROS_upr_ci = mean_ROS + ci_ROS,
         ROS_lwr_ci = mean_ROS - ci_ROS)

# Graph A, bar graphs and SE
bar_se <- ggplot(data = samples %>%
                   slice(1)) +
  geom_bar(aes(x = sample, 
               y = mean_ROS),
           stat = "identity",
           width = 0.4,
           alpha = 0.5,
           fill = "blue") +
  geom_errorbar(aes(ymax = ROS_upr_se,
                    ymin = ROS_lwr_se,
                    x = sample),
                width = 0.15,
                linewidth = 0.75) +
  labs(x = "Treatment",
       y = "Mean ROS (± SE)",
       title = "A") +
  ylim(0,8) +
  theme_classic()

# Graph B, bar graphs and sd
bar_sd <- ggplot(data = samples %>%
                   slice(1)) +
  geom_bar(aes(x = sample, 
               y = mean_ROS),
           stat = "identity",
           width = 0.4,
           alpha = 0.5,
           fill = "blue") +
  geom_errorbar(aes(ymax = ROS_upr_sd,
                    ymin = ROS_lwr_sd,
                    x = sample),
                width = 0.15,
                linewidth = 0.75) +
  labs(x = "Treatment",
       y = "Mean ROS (± SD)",
       title = "B") +
  ylim(0,8) +
  theme_classic()

# Graph C, box plot
box_plot <- ggplot(data = samples) +
  geom_boxplot(aes(x = sample, 
               y = ROS),
           stat = "boxplot",
           width = 0.4,
           alpha = 0.5,
           fill = "blue") +
  labs(x = "Treatment",
       y = "Mean ROS",
       title = "C") +
  ylim(0,8) +
  theme_classic()

# Graph D, beeswarm plot sd error bar and another for SE
beeswarm_plot <- ggplot(data = samples) +
  geom_errorbar(aes(ymax = ROS_upr_sd,
                    ymin = ROS_lwr_sd,
                    x = sample),
                width = 0.01,
                linewidth = 8,
                alpha = 0.3,
                color = "gray") +
  geom_errorbar(aes(ymax = ROS_upr_ci,
                    ymin = ROS_lwr_ci,
                    x = sample),
                width = 0.3,
                linewidth = 0.6) +
  geom_beeswarm(aes(x = sample,
                    y = ROS),
                width = 0.4,
                alpha = 0.7,
                fill = "blue",
                pch = 21,
                size = 3) +
  labs(x = "Treatment",
       y = "Mean ROS (95 % CI, SD)",
       title = "D") +
  ylim(0,8) +
  theme_classic()

# Combine plots for the slide
q4_plots <- bar_se +
  bar_sd +
  box_plot +
  beeswarm_plot