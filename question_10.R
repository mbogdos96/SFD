# sample the two groups
tt_group_1 <- rnorm(30,5,1)

tt_group_2 <- rnorm(50,3,0.25)

# Bind them in a df
tt_g_df <- data.frame(ROS = tt_group_1,
                      group = 1) %>%
  rbind(data.frame(ROS = tt_group_2,
                   group = 2))

# Histogram of the two groups
tt_hist_plot <- ggplot(tt_g_df) +
  geom_histogram(aes(y = ROS,
                     fill = group),
                 stat = "identity") +
  theme_classic()