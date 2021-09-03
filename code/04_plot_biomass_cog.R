## Make plots of biomass and center of gravity (COG) ##

library(ggplot2)
library(dplyr)
library(viridis)

# get population density predictions, COGs, and biomass estimates
p1 = readRDS("results/predictions_1x.rds")
p4 = readRDS("results/predictions_4x.rds")
cog1 <- readRDS("~/best-practices/results/COG_1x_bias_corrected.rds")
cog4 <- readRDS("~/best-practices/results/COG_4x_bias_corrected.rds")
b1 <- readRDS("~/best-practices/results/biomass_1x_bias_corrected.rds")
b4 <- readRDS("~/best-practices/results/biomass_4x_bias_corrected.rds")

# set up error bars for plotting in two dimensions, where COG is not dynamic in spatial-only model
cog1 <- cog1 %>%
  distinct(coord, .keep_all = TRUE) %>%
  select(est, lwr, upr, coord) %>%
  mutate(res = "fine")
cog4 <- cog4 %>%
  distinct(coord, .keep_all = TRUE) %>%
  select(est, lwr, upr, coord) %>%
  mutate(res = "coarse")
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
