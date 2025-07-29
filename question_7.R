source("question_4.R")

# Sample no effect size and 
ten_no_effect <- sims_fn(5000,0,10)

# Filter for p < 0.001 ***
ten_psss <- ten_no_effect %>%
  filter(welch_p_val <= 0.001)

# Filter for p < 0.01 **
ten_pss <- ten_no_effect %>%
  filter(welch_p_val <= 0.01
         & welch_p_val > 0.001)

# Filter for p < 0.05 *
ten_ps <- ten_no_effect %>%
  filter(welch_p_val <= 0.05
         & welch_p_val > 0.01)

# Probability df
prob_stars <- data.frame(Sig_lvl = c("*",
                                     "**",
                                     "***"),
                         Prob = c(nrow(ten_ps)/100000,
                                  nrow(ten_pss)/100000,
                                  nrow(ten_psss)/100000))

# Plot these
boxplot_stars <- close_p_val(ten_ps %>%
                               filter(sim_no == 10)) +
  close_p_val(ten_pss %>%
                filter(sim_no == 182)) +
  close_p_val(ten_psss %>%
                filter(sim_no == 839))

# Find correlation between effect size and p value
# Function for simulating various
sim_range_fn <- function(effect_range,
                         no_simulations,
                         sample_ns){
  
  # Init df
  fn_df <- sims_fn(2,0,10) %>%
    mutate(effect = 1) %>%
    filter(ROS == 100)
  
  for (effect in effect_range) {
    
    for_df <- sims_fn(no_simulations,
                      effect,
                      sample_ns) %>%
      mutate(effect = effect)
    
    # Add to df
    fn_df <- rbind(fn_df,
                   for_df)
  }
  
  return(fn_df)
}

# Run the function
range_sims <- sim_range_fn(seq(0,2,by = 0.25),
                           100, 10)

# Manipulate the df to get the plot you want
rs_df <- range_sims %>%
  ungroup() %>%
  dplyr::select(welch_p_val,effect,sim_no) %>%
  group_by(effect,sim_no) %>%
  summarise(welch_p_val = mean(welch_p_val), 
            .groups = "drop")

# Plot the correlation
effect_p_corr <- ggplot(rs_df) +
  geom_point(aes(x = effect,
                 y = welch_p_val)) +
  labs(x = "Effect size",
       y = "Welch's t test p value") +
  theme_classic()

print(effect_p_corr)