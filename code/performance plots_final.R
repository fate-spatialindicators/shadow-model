## Performance plots ##

library(ggplot2)
library(gridExtra)

performance_cleanedup = readRDS(paste0("C:/Users/cjcco/Box/Best Practices paper/sablefish_performance_50to1000knots.rds"))
performance_cleanedup[, "runtime_minutes"] = performance_cleanedup$runtime/60
performance_cleanedup[, "tweedie_dens_relative"] = abs(performance_cleanedup$tweedie_dens_m1/max(abs(performance_cleanedup$tweedie_dens_m1)))

###### Final figure

# Runtime vs. Relative (to max. density) Tweedie Predictive Density with Number of Knots
pf = ggplot(performance_cleanedup, aes(x=runtime_minutes, y=tweedie_dens_relative, label = knots)) + 
  geom_point(col="darkorchid4", size=3) +
  ylim(c(0.75, 1.05)) +
  ggtitle("Runtime vs. Relative Tweedie Predictive Density (with Number of Knots)") + xlab("Runtime (Minutes)") + 
  ylab("Relative Tweedie Predictive Density")
# locally weighted regression
pf + stat_smooth(method = "loess", formula = y ~ x, size = 1, se = FALSE, colour = "darkorchid4") + 
  geom_text(aes(label = knots), vjust = -2) + theme_classic()
# linear with quadratic
#pf + stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, se = FALSE, colour = "magenta3") + 
  #geom_text(aes(label = knots), vjust = -2)


# Alternative: Runtime (x) vs. Tweedie Predictive Density (y) with Number of Knots
#pf_a = ggplot(performance_cleanedup, aes(x=runtime_divby60, y=tweedie_dens_divby1000, label = knots)) + 
  #geom_point(col="magenta3", size=3) +
  #ylim(c(65, 85)) + 
  #ggtitle("Runtime vs. Tweedie Predictive Density (with Number of Knots)") + xlab("Runtime (Minutes)") + 
  #ylab("Tweedie Predictive Density")
# linear with quadratic
#pf_a + stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, se = FALSE, colour = "magenta3") + 
  #geom_text(aes(label = knots), vjust = -2)
