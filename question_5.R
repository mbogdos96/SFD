library(dplyr)
library(ggplot2)

# Simulate data with no effect and t test and record p val
sims_fn <- function(no_sims,
                    effect,
                    n_sample){
  # Make a df
  fn_df <- data.frame(ROS = c(),
                      sample = c(),
                      sim_no = c(),
                      welch_p_val = c())
  
  # Simulations to iterate
  sim_list <- seq(1,no_sims, 
                  by = 1)
  
  # Loop through sims
  for (sim in sim_list){
    # Set the seed
    set.seed(sim)
    
    # Sample the first group
    HCHO <- data.frame(ROS = rnorm(n_sample,5,1)) %>%
      mutate(sample = "HCHO")
    
    # Sample the second group
    CCHO <- data.frame(ROS = rnorm(n_sample,
                                   5 - effect,
                                   1)) %>%
      mutate(sample = "CCHO")
    
    # Combine df and get p val
    comb_loop_df <- rbind(HCHO,
                          CCHO) %>%
      mutate(sim_no = sim,
             welch_p_val = t.test(HCHO$ROS,
                                  CCHO$ROS)$p.value)
    
    # Add into loop outside the df
    fn_df <- fn_df %>%
      rbind(comb_loop_df)
  }
  
  fn_df <- fn_df %>%
    group_by(sample,sim_no) %>%
    mutate(mean_ROS = mean(ROS)) %>%
    ungroup(sample) %>%
    mutate(ROS_change = unique(mean_ROS[sample == "HCHO"])
           - unique(mean_ROS[sample == "CCHO"]))
  
  return(fn_df)
}

# Simulate 100 times no effect sample size 10
ten_no <- sims_fn(100,0,10)

# Simulate 100 times effect of 1 sample size 10
ten_real <- sims_fn(100,1,10)

# Find some p vals that look really close
p_hack_close <- ten_no %>%
  filter(welch_p_val > 0.05 & welch_p_val < 0.06)

# Find a p val that is just significant
p_hack_madeit <- ten_no %>%
  filter(welch_p_val < 0.05 & ROS_change < 0)

# Boxplot for the two groups with non sig p
close_p_val <- function(df){
  fn_plot <- ggplot(data = df) +
  geom_boxplot(aes(x = sample, 
                   y = ROS),
               stat = "boxplot",
               width = 0.4,
               alpha = 0.5,
               fill = "blue") +
  labs(x = "Treatment",
       y = "Mean ROS") +
  theme_classic()
  
  return(fn_plot)
}

# Make the combined plot
p_val_plot <- close_p_val(p_hack_close) +
  close_p_val(p_hack_madeit)

# Hist of p values from the simulation
hist_p_val <- function(df){
  hist_fn_plot <- ggplot(df) +
    geom_histogram(aes(x = welch_p_val),
                   binwidth = 0.05) +
    geom_vline(xintercept = 0.05,
               color = "red",
               linetype = "dotted",
               linewidth = 1) +
    labs(x = "p value",
         y = "") +
    theme_classic()
  
  return(hist_fn_plot)} 

hist_p_val_plots <- hist_p_val(ten_no) +
  hist_p_val(ten_real)

print(hist_p_val_plots)

# Function for simulating the scenario described
p_hack_sim_fn <- function(no_sims,
                          effect,
                          n_sample,
                          m1,sd1){
  # Make a df
  fn_df <- data.frame(ROS = c(),
                      sample = c(),
                      sim_no = c(),
                      welch_p_val = c(),
                      cond = c())
  
  # Simulations to iterate
  sim_list <- seq(1,no_sims, 
                  by = 1)
  
  # Loop through sims
  for (sim in sim_list){
    # Set the seed
    set.seed(sim)
    
    # Sample the first group
    HCHO <- data.frame(ROS = rnorm(n_sample,m1,sd1)) %>%
      mutate(sample = "HCHO")
    
    # Sample the second group
    CCHO <- data.frame(ROS = rnorm(n_sample,
                                   m1 - effect,
                                   sd1)) %>%
      mutate(sample = "CCHO")
    
    if(t.test(HCHO$ROS,
              CCHO$ROS)$p.value > 0.05 &
       t.test(HCHO$ROS,
              CCHO$ROS)$p.value < 0.06){
      
      # Combine df and get p val, label as pre
      comb_loop_df <- rbind(HCHO,
                            CCHO) %>%
        mutate(sim_no = sim,
               welch_p_val = t.test(HCHO$ROS,
                                    CCHO$ROS)$p.value,
               cond = "pre")
      
      # Sample the first group again for 3 more points
      HCHO_post <- data.frame(ROS = rnorm(3,m1,sd1)) %>%
        mutate(sample = "HCHO") %>%
        rbind(HCHO) %>%
        mutate(cond = "post")
      
      # Sample the second group
      CCHO_post <- data.frame(ROS = rnorm(3,
                                     m1 - effect,
                                     sd1)) %>%
        mutate(sample = "CCHO") %>%
        rbind(CCHO) %>%
        mutate(cond = "post")
      
      # Combine df and get p val, label as pre
      comb_loop_df_post <- rbind(HCHO_post,
                            CCHO_post) %>%
        mutate(sim_no = sim,
               welch_p_val = t.test(HCHO_post$ROS,
                                    CCHO_post$ROS)$p.value)
      }
    
    else{
      comb_loop_df <- data.frame(ROS = c(),
                                 sample = c(),
                                 sim_no = c(),
                                 welch_p_val = c(),
                                 cond = c())
      
      comb_loop_df_post <- data.frame(ROS = c(),
                                 sample = c(),
                                 sim_no = c(),
                                 welch_p_val = c(),
                                 cond = c())
    }
    
    # Add into loop outside the df
    fn_df <- fn_df %>%
      rbind(comb_loop_df,
            comb_loop_df_post)
  }
  
  # Add into loop outside the df
  fn_df <- fn_df %>%
    group_by(sample,sim_no) %>%
    mutate(mean_ROS = mean(ROS)) %>%
    ungroup(sample) %>%
    mutate(ROS_change = unique(mean_ROS[sample == "HCHO"])
           - unique(mean_ROS[sample == "CCHO"]))
  
  return(fn_df)
}

# Simulate 10000 times no effect sample size 
# 10 mean 5 sd 3
ten_hack <- p_hack_sim_fn(10000,0,10,5,3)

# Get the probabilities
prob_p_hack <- data.frame(Second_p_val = c("Sig.",
                                          "Not sig."),
                          Probability = c(length(
                            unique(
 (ten_hack %>%
    filter(welch_p_val < 0.05
           & cond == "post"))$sim_no)),
 length(
   unique(
     (ten_hack %>%
        filter(welch_p_val > 0.05
               & cond == "post"))$sim_no)))) %>%
  mutate(Probability = Probability/length(unique(ten_hack$sim_no)))

# Plot of post p values
post_p_vals_plot <- hist_p_val(ten_no) +
  hist_p_val(ten_hack %>%
               filter(cond == "post"))