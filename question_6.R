library(dplyr)
library(ggplot2)
library(patchwork)

set.seed(123)

# Parameters
n1 <- 10
n2 <- 10
true_diff <- 0.4
sd1 <- sd2 <- 0.1

# Simulate data
group1 <- rnorm(n1, 
                mean = 0.5, 
                sd = sd1)

group2 <- rnorm(n2, 
                mean = true_diff, 
                sd = sd2)

# Welch's t-test
t_test_result <- t.test(group1, 
                        group2, 
                        var.equal = F)

# Equivalence bounds (smallest effect size of interest)
low_eqbound <- -0.3
high_eqbound <- 0.3
alpha <- 0.05

# Means and pooled SE
m1 <- mean(group1)
m2 <- mean(group2)
se <- sqrt(var(group1)/n1 + var(group2)/n2)
diff <- m1 - m2

# TOST t-stats
t1 <- (diff - low_eqbound) / se
t2 <- (diff - high_eqbound) / se

# Degrees of freedom (Welch-Satterthwaite)
df <- (var(group1)/n1 + var(group2)/n2)^2 / 
  ((var(group1)^2)/(n1^2 * (n1 - 1)) + (var(group2)^2)/(n2^2 * (n2 - 1)))

# One-sided p-values
p1 <- pt(t1, df, 
         lower.tail = F)
p2 <- pt(t2, df, 
         lower.tail = T)

# TOST result: reject equivalence if both p-values < alpha
cat("Welch's t-test:\n")
print(t_test_result)

cat("\nManual TOST results:\n")
cat(sprintf("p1 (>%g): %.4f\n", low_eqbound, p1))
cat(sprintf("p2 (<%g): %.4f\n", high_eqbound, p2))
cat(sprintf("Reject equivalence: %s\n", 
            ifelse(p1 < alpha & p2 < alpha, "YES", "NO")))

# Plot the differences
# Make a df with the two groups
group_df <- data.frame(cell_vi = group1) %>%
  mutate(cond = "HCHO 100 uM") %>%
  rbind(data.frame(cell_vi = group2) %>%
          mutate(cond = "CCHO 100 uM"))

# Boxplot of differences
eq_test_boxplot <- ggplot(data = group_df) +
  geom_boxplot(aes(x = cond, 
                   y = cell_vi),
               stat = "boxplot",
               width = 0.4,
               alpha = 0.5,
               fill = "blue") +
  labs(x = "RCS Normalized Treatment",
       y = "Cell Viability") +
  ylim(0,1) +
  theme_classic()

print(eq_test_boxplot)