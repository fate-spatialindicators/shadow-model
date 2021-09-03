## Make plots of biomass and center of gravity (COG) ##

library(ggplot2)
library(dplyr)
library(viridis)

# plot COGs to compare prediction resolutions ----

# load estimated center of gravity output from 02_predict_biomass_cog.R
cog1 <- readRDS("results/COG_1x_bias_corrected.rds")
cog4 <- readRDS("results/COG_4x_bias_corrected.rds")

# set up error bars for plotting in two dimensions, where COG is not dynamic in spatial-only model
cog1 <- cog1 %>%
  distinct(coord, .keep_all = TRUE) %>%
  select(est, lwr, upr, coord) %>%
  mutate(res = "fine (1x)")
cog4 <- cog4 %>%
  distinct(coord, .keep_all = TRUE) %>%
  select(est, lwr, upr, coord) %>%
  mutate(res = "coarse (4x)")
cogs <- bind_rows(cog1, cog4)

cogs_wide_est <- reshape2::dcast(cogs, res ~ coord, value.var = "est")
cogs_wide_lwr <- reshape2::dcast(cogs, res ~ coord, value.var = "lwr") %>%
  mutate(X_lwr = X, Y_lwr = Y)
cogs_wide_upr <- reshape2::dcast(cogs, res ~ coord, value.var = "upr") %>%
  mutate(X_upr = X, Y_upr = Y)
cogs_wide <- cogs_wide_est %>%
  left_join(select(cogs_wide_lwr, res, X_lwr, Y_lwr)) %>%
  left_join(select(cogs_wide_upr, res, X_upr, Y_upr)) %>%
  mutate(diameter_x = X_upr - X_lwr, diameter_y = Y_upr - Y_lwr)

# scatter plot of COG at each resolution, with 2D error bars
ggplot(cogs_wide, aes(X, Y, color = factor(res))) +
  geom_point(size=4) +
  geom_segment(aes(x = X_lwr, xend = X_upr, y = Y, yend = Y), lwd = 1) +
  geom_segment(aes(x = X, xend = X, y = Y_lwr, yend = Y_upr), lwd = 1) +
  scale_color_viridis(discrete = TRUE, begin = 0, end = 0.8) +
  xlab("COG Eastings (km)") +
  ylab("COG Northings (km)") +
  labs(color = "resolution") +
  theme_classic()
ggsave("plots/cog_resolution_comparison.pdf", width = 4.5, height = 4, units = "in")


# plot biomass over time to compare prediction resolutions ----

# load estimated biomass output from 02_predict_biomass_cog.R
b1 <- readRDS("results/biomass_1x_bias_corrected.rds")
b4 <- readRDS("results/biomass_4x_bias_corrected.rds")

# make estimates relative to max est
b1$est_rel = b1$est/max(b1$est)
b1$lwr_rel = b1$lwr/max(b1$est)
b1$upr_rel = b1$upr/max(b1$est)
b4$est_rel = b4$est/max(b4$est)
b4$lwr_rel = b4$lwr/max(b4$est)
b4$upr_rel = b4$upr/max(b4$est)

# add column for grid resolution
b1$res <- "Fine (1x)"
b4$res <- "Coarse (4x)"
b = bind_rows(b1, b4)

# plot time series
ggplot(b, aes(x=year, y=est_rel, color=factor(res)), group=res) +
  geom_point(size=2, position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(x=year,ymin=lwr_rel, ymax=upr_rel), width=0, position = position_dodge(width = 0.6)) +
  #scale_x_continuous(breaks = seq(from = 2003, to = 2018, by = 1)) +
  scale_color_viridis(discrete = TRUE, begin = 0, end = 0.8, breaks=c("Fine (1x)","Coarse (4x)")) +
  xlab("Year") +
  ylab("Relative Biomass Estimate") +
  labs(color = "resolution") +
  theme_classic()
ggsave("plots/cog_resolution_comparison.pdf", width = 6.5, height = 4, units = "in")

# Coefficient of Variation (CV) across years
b1_mean = mean(b1$est)
b1_sd = sd(b1$est)
b1_cv = b1_sd/b1_mean

b4_mean = mean(b4$est)
b4_sd = sd(b4$est)
b4_cv = b4_sd/b4_mean

# CVs for each year (Annual CVs)
b1_cvs = data.frame("year" = 2003:2018)
b1_cvs[, "CV"] = b1$se/b1$log_est
mean(b1_cvs$CV)

b4_cvs = data.frame("year" = 2003:2018)
b4_cvs[, "CV"] = b4$se/b4$log_est
mean(b4_cvs$CV)

save(b1_cv, b4_cv, b1_cvs, b4_cvs, file = "results/biomass_cvs.RData")


