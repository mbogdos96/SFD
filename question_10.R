library(dplyr)
library(ggplot2)

# sample the two groups
tt_group_1 <- rnorm(300,5,1)

tt_group_2 <- rnorm(500,3,0.25)

# Bind them in a df
tt_g_df <- data.frame(ROS = tt_group_1,
                      group = 1) %>%
  rbind(data.frame(ROS = tt_group_2,
                   group = 2))

# Histogram of the two groups
tt_hist_plot <- ggplot(tt_g_df) +
  geom_histogram(aes(x = ROS,
                     fill = as.factor(group))) +
  labs(fill = "Treatment") +
  theme_classic()

print(tt_hist_plot)