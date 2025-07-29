library(pwr)
library(patchwork)

# Power analysis for alpha 0.05
trad_sig <- pwr.t.test(d = 0.5, 
                       power = 0.8, 
                       sig.level = 0.05, 
           type = "two.sample", alternative = "two.sided")

# Power analysis for alpha 0.1
alt_sig <- pwr.t.test(d = 0.5, 
           power = 0.8, 
           sig.level = 0.1, 
           type = "two.sample", alternative = "two.sided")

# Power plots
power_plots <- plot(trad_sig) +
  plot(alt_sig)