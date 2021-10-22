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
  mutate(res = "Fine (1x)")
cog4 <- cog4 %>%
  distinct(coord, .keep_all = TRUE) %>%
  select(est, lwr, upr, coord) %>%
  mutate(res = "Coarse (4x)")
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
ggplot(cogs_wide, aes(X, Y, color = res)) +
  geom_point(size = 4) +
  geom_segment(aes(x = X_lwr, xend = X_upr, y = Y, yend = Y), lwd = 1) +
  geom_segment(aes(x = X, xend = X, y = Y_lwr, yend = Y_upr), lwd = 1) +
  scale_color_viridis(discrete = TRUE, begin = 0, end = 0.8) +
  xlab("COG Eastings (km)") +
  ylab("COG Northings (km)") +
  labs(color = "Resolution") +
  theme_classic()
ggsave("plots/cog_resolution_comparison.pdf", width = 4.5, height = 4, units = "in")


# plot COGs to compare best and worst model ----

# load estimated center of gravity output from 02_predict_biomass_cog.R
cognull <- readRDS("results/COG_1x_nodepth.rds")

# set up error bars for plotting in two dimensions, where COG is not dynamic in spatial-only model
cog1$model <- "space + year + depth" # "Model 1"

cognull <- cognull %>%
  distinct(coord, .keep_all = TRUE) %>%
  select(est, lwr, upr, coord) %>%
  mutate(model = "space + year") # "Model 4"
cogs <- bind_rows(cog1, cognull)

cogs_wide_est <- reshape2::dcast(cogs, model ~ coord, value.var = "est")
cogs_wide_lwr <- reshape2::dcast(cogs, model ~ coord, value.var = "lwr") %>%
  mutate(X_lwr = X, Y_lwr = Y)
cogs_wide_upr <- reshape2::dcast(cogs, model ~ coord, value.var = "upr") %>%
  mutate(X_upr = X, Y_upr = Y)
cogs_wide <- cogs_wide_est %>%
  left_join(select(cogs_wide_lwr, model, X_lwr, Y_lwr)) %>%
  left_join(select(cogs_wide_upr, model, X_upr, Y_upr)) %>%
  mutate(diameter_x = X_upr - X_lwr, diameter_y = Y_upr - Y_lwr)

# scatter plot of COG between models, with 2D error bars
ggplot(cogs_wide, aes(X, Y, color = factor(model))) +
  geom_point(size = 4) +
  geom_segment(aes(x = X_lwr, xend = X_upr, y = Y, yend = Y), lwd = 1) +
  geom_segment(aes(x = X, xend = X, y = Y_lwr, yend = Y_upr), lwd = 1) +
  scale_color_viridis(discrete = TRUE, begin = 0, end = 0.8) +
  xlab("COG Eastings (km)") +
  ylab("COG Northings (km)") +
  labs(color = "Model") +
  theme_classic()
ggsave("plots/cog_model_structure_comparison.pdf", width = 5, height = 4, units = "in")


# plot COGs to compare data filtering/quantiles ----

# load estimated center of gravity output from 02_predict_biomass_cog.R
cog_80 <- readRDS("results/COG_1x_80quantile_bias_corrected.rds")
cog_90 <- readRDS("results/COG_1x_90quantile_bias_corrected.rds")
cog_95 <- readRDS("results/COG_1x_95quantile_bias_corrected.rds")
cog_100 <- readRDS("results/COG_1x_bias_corrected.rds")

# set up error bars for plotting in two dimensions, where COG is not dynamic in spatial-only model
cog_80 <- cog_80 %>%
  distinct(coord, .keep_all = TRUE) %>%
  select(est, lwr, upr, coord) %>%
  mutate(quant = "80% Quantile")
cog_90 <- cog_90 %>%
  distinct(coord, .keep_all = TRUE) %>%
  select(est, lwr, upr, coord) %>%
  mutate(quant = "90% Quantile")
cog_95 <- cog_95 %>%
  distinct(coord, .keep_all = TRUE) %>%
  select(est, lwr, upr, coord) %>%
  mutate(quant = "95% Quantile")
cog_100 <- cog_100 %>%
  distinct(coord, .keep_all = TRUE) %>%
  select(est, lwr, upr, coord) %>%
  mutate(quant = "100% Quantile")
cogs <- bind_rows(cog_80, cog_90, cog_95, cog_100)

cogs_wide_est <- reshape2::dcast(cogs, quant ~ coord, value.var = "est")
cogs_wide_lwr <- reshape2::dcast(cogs, quant ~ coord, value.var = "lwr") %>%
  mutate(X_lwr = X, Y_lwr = Y)
cogs_wide_upr <- reshape2::dcast(cogs, quant ~ coord, value.var = "upr") %>%
  mutate(X_upr = X, Y_upr = Y)
cogs_wide <- cogs_wide_est %>%
  left_join(select(cogs_wide_lwr, quant, X_lwr, Y_lwr)) %>%
  left_join(select(cogs_wide_upr, quant, X_upr, Y_upr)) %>%
  mutate(diameter_x = X_upr - X_lwr, diameter_y = Y_upr - Y_lwr,
  quant = factor(quant, levels=c("80% Quantile", "90% Quantile", "95% Quantile", "100% Quantile")))

# scatter plot of COG for each quantile, with 2D error bars
ggplot(cogs_wide, aes(X, Y, color = quant)) +
  geom_point(size = 4) +
  geom_segment(aes(x = X_lwr, xend = X_upr, y = Y, yend = Y), lwd = 1) +
  geom_segment(aes(x = X, xend = X, y = Y_lwr, yend = Y_upr), lwd = 1) +
  scale_color_viridis(discrete = TRUE, begin = 0, end = 0.8) +
  xlab("COG Eastings (km)") +
  ylab("COG Northings (km)") +
  labs(color = "Data Level") +
  theme_classic()
ggsave("plots/cog_quantiles_comparison.pdf", width = 4.5, height = 4, units = "in")


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
ggplot(b, aes(x=year, y=est_rel, color=res), group=res) +
  geom_point(size=2, position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(x=year,ymin=lwr_rel, ymax=upr_rel), width=0, position = position_dodge(width = 0.6)) +
  scale_x_continuous(breaks = seq(from = 2003, to = 2018, by = 3)) +
  scale_color_viridis(discrete = TRUE, begin = 0, end = 0.8, breaks=c("Coarse (4x)","Fine (1x)")) +
  xlab("Year") +
  ylab("Relative Biomass Estimate") +
  labs(color = "Resolution") +
  theme_classic()
ggsave("plots/biomass_resolution_comparison.pdf", width = 6.5, height = 4, units = "in")


# plot biomass over time to compare between best and worst model ----

# load estimated biomass output from 02_predict_biomass_cog.R
bnull <- readRDS("results/biomass_1x_nodepth.rds")

# make estimates relative to max est
bnull$est_rel = bnull$est/max(bnull$est)
bnull$lwr_rel = bnull$lwr/max(bnull$est)
bnull$upr_rel = bnull$upr/max(bnull$est)

# add column for model number
b1$model <- "space + year + depth" # "Model 1"
bnull$model <- "space + year" # "Model 4"
bnull$res <- "Fine (1x)"
b = bind_rows(b1, bnull)

# plot time series
ggplot(b, aes(x=year, y=est_rel, color=factor(model)), group=model) +
  geom_point(size=2, position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(x=year,ymin=lwr_rel, ymax=upr_rel), width=0, position = position_dodge(width = 0.6)) +
  scale_x_continuous(breaks = seq(from = 2003, to = 2018, by = 3)) +
  scale_color_viridis(discrete = TRUE, begin = 0, end = 0.8, breaks=c("space + year + depth","space + year")) +
  xlab("Year") +
  ylab("Relative Biomass Estimate") +
  labs(color = "Model") +
  theme_classic()
ggsave("plots/biomass_model_structure_comparison.pdf", width = 7, height = 4, units = "in")


# plot biomass over time to compare data filtering/quantiles ----

# load estimated biomass output from 02_predict_biomass_cog.R
b_80 <- readRDS("results/biomass_1x_80quantile_bias_corrected.rds")
b_90 <- readRDS("results/biomass_1x_90quantile_bias_corrected.rds")
b_95 <- readRDS("results/biomass_1x_95quantile_bias_corrected.rds")
b_100 <- readRDS("results/biomass_1x_bias_corrected.rds")

# make estimates relative to max est
b_80$est_rel = b_80$est/max(b_80$est)
b_80$lwr_rel = b_80$lwr/max(b_80$est)
b_80$upr_rel = b_80$upr/max(b_80$est)
b_90$est_rel = b_90$est/max(b_90$est)
b_90$lwr_rel = b_90$lwr/max(b_90$est)
b_90$upr_rel = b_90$upr/max(b_90$est)
b_95$est_rel = b_95$est/max(b_95$est)
b_95$lwr_rel = b_95$lwr/max(b_95$est)
b_95$upr_rel = b_95$upr/max(b_95$est)
b_100$est_rel = b_100$est/max(b_100$est)
b_100$lwr_rel = b_100$lwr/max(b_100$est)
b_100$upr_rel = b_100$upr/max(b_100$est)

# add column for quantile
b_80$quant <- "80% Quantile"
b_90$quant <- "90% Quantile"
b_95$quant <- "95% Quantile"
b_100$quant <- "100% Quantile"
b <- bind_rows(b_80, b_90, b_95, b_100) %>%
  mutate(quant_order = factor(quant, levels=c("80% Quantile", "90% Quantile", "95% Quantile", "100% Quantile")))

# plot time series
ggplot(b, aes(x=year, y=est_rel, color=quant_order), group=quant_order) +
  geom_point(size=2, position = position_dodge(width = 0.65)) +
  geom_errorbar(aes(x=year,ymin=lwr_rel, ymax=upr_rel), width=0, position = position_dodge(width = 0.65)) +
  scale_x_continuous(breaks = seq(from = 2003, to = 2018, by = 3)) +
  scale_y_continuous(breaks = seq(from = 0.4, to = 1.0, by = 0.2)) +
  scale_color_viridis(discrete = TRUE, begin = 0, end = 0.8, breaks=c("80% Quantile","90% Quantile","95% Quantile","100% Quantile")) +
  xlab("Year") +
  ylab("Relative Biomass Estimate") +
  labs(color = "Data Level") +
  theme_classic()
ggsave("plots/biomass_quantile_comparison.pdf", width = 7.5, height = 4, units = "in")
