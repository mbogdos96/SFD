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

#===============
# Answer portion
#===============
# Simulate the answers
# Function for simulations
sd_n_sims_fn <- function(n_seq){
  # Generate a normal sample
  norm_sample <- rnorm(1000000, 
                        mean = 0, 
                        sd = 100)
  
  # Create an empty data frame
  df <- data.frame(n = numeric(), 
                   sd = numeric())
  
  # Loop through each sample size (n)
  for (n in n_seq){
    # Create 100 samples for each sample size
    for (i in 1:100){
      # Generate a sample of size n from 
      # the "normal" variable
      sample_data <- sample(norm_sample, 
                            n)
      
      # Calculate the standard 
      # deviation of the sample
      sample_sd <- sd(sample_data)
      
      # Append the sample size (n) 
      # and sample standard deviation (sd) 
      # to the data frame
      df <- rbind(df, data.frame(n = n, 
                                 sd = sample_sd))
    }
  }
  
  return(df)
}

# Simulate for n from 3 to 30
sd_n_three_thirty <- sd_n_sims_fn(seq(3,30,by = 1)) %>%
  group_by(n) %>% 
  summarize(average.sd = mean(sd),
            upr_sd = average.sd + sd(sd),
            lwr_sd = average.sd - sd(sd))

# Plot the relationship
sd_n_plot_sims <- ggplot(data = sd_n_three_thirty,
                         aes(x = n, y = average.sd)) +
  geom_hline(yintercept = 100) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr_sd, ymax = upr_sd),
                width = 0.01,
                alpha = 0.8) +
  labs(x = "Sample size (n)",
       y = "Average standard deviation (100 sims)") +
  theme_classic()

print(sd_n_plot_sims)