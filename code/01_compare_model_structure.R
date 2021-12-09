# Fitting spatiotemporal models to West Coast Groundfish Bottom Trawl data with sdmTMB

#remotes::install_github("pbs-assess/sdmTMB")
library(sdmTMB)

haul_new = readRDS("data/sablefish_cleaned.rds")

# Fit models to the sablefish data.
# 1. spatial only
# 2. IID spatiotemporal model
# 3. AR1 spatiotemporal model
# 4. spatial only - depth

# Make mesh, similar to same resolution as west coast stock
# assessments
spde = make_mesh(
  haul_new,
  xy_cols = c("X","Y"),
  type = "cutoff",
  cutoff = 15,
  seed = 2021
)

# view number of knots
spde$mesh$n

# set up function to refit models when needed to aid convergence
# refit_model_if_needed <- function(m) {
#     if (max(abs(m$gradients) > 0.01)) {
#       m <- sdmTMB::run_extra_optimization(m,
#                                           nlminb_loops = 1L,
#                                           newton_loops = 1L
#         )
#       }
#   m
# }

# spatial only + depth
model_1 <- sdmTMB_cv(cpue_kg_km2 ~ 0 + as.factor(year) + log_depth_scaled + log_depth_scaled2,
                     spde = spde,
                     k_folds = max(haul_new$fold),
                     fold_ids = haul_new$fold,
                     data = haul_new,
                     control = sdmTMBcontrol(nlminb_loops = 2, newton_loops = 1),
                     priors = sdmTMBpriors(matern_s = pc_matern(range_gt = 75, sigma_lt = 5)),
                     spatial_only = TRUE,
                     family = tweedie(link = "log"))
saveRDS(model_1, "results/fit_spatial_depth.rds")
model_1$sum_loglik

# IID
model_2 <- sdmTMB_cv(cpue_kg_km2 ~ 0 + as.factor(year) + log_depth_scaled + log_depth_scaled2,
                     spde = spde,
                     k_folds = max(haul_new$fold),
                     fold_ids = haul_new$fold,
                     data = haul_new,
                     time = "year",
                     control = sdmTMBcontrol(nlminb_loops = 2, newton_loops = 1),
                     priors = sdmTMBpriors(matern_s = pc_matern(range_gt = 75, sigma_lt = 5)),
                     fields = "IID",
                     spatial_only = FALSE,
                     family = tweedie(link = "log"))
saveRDS(model_2, "results/fit_IID_depth.rds")
model_2$sum_loglik

# AR1
model_3 <- sdmTMB_cv(cpue_kg_km2 ~ 0 + as.factor(year) + log_depth_scaled + log_depth_scaled2,
                     spde = spde,
                     k_folds = max(haul_new$fold),
                     fold_ids = haul_new$fold,
                     data = haul_new,
                     time = "year",
                     control = sdmTMBcontrol(nlminb_loops = 2, newton_loops = 1),
                     priors = sdmTMBpriors(matern_s = pc_matern(range_gt = 75, sigma_lt = 5)),
                     fields = "AR1",
                     spatial_only = FALSE,
                     family = tweedie(link = "log"))
saveRDS(model_3, "results/fit_AR1_depth.rds")
model_3$sum_loglik

# spatial only - depth
model_4 <- sdmTMB_cv(cpue_kg_km2 ~ 0 + as.factor(year),
                     spde = spde,
                     k_folds = max(haul_new$fold),
                     fold_ids = haul_new$fold,
                     data = haul_new,
                     control = sdmTMBcontrol(nlminb_loops = 2, newton_loops = 1),
                     priors = sdmTMBpriors(matern_s = pc_matern(range_gt = 75, sigma_lt = 5)),
                     spatial_only = TRUE,
                     family = tweedie(link = "log"))
saveRDS(model_4, "results/fit_spatial_nodepth.rds")
model_4$sum_loglik

# fit best fit model structure (spatial only + depth) to full dataset
model_1_full <- sdmTMB(cpue_kg_km2 ~ 0 + as.factor(year) + log_depth_scaled + log_depth_scaled2,
                       spde = spde,
                       data = haul_new,
                       time = "year",
                       control = sdmTMBcontrol(nlminb_loops = 2, newton_loops = 1),
                       priors = sdmTMBpriors(matern_s = pc_matern(range_gt = 75, sigma_lt = 5)),
                       spatial_only = TRUE,
                       family = tweedie(link = "log"))
saveRDS(model_1_full, "results/fit_spatial_depth_full.rds")

# fit worst fit model to full dataset for comparison of biomass trends
model_4_full <- sdmTMB(cpue_kg_km2 ~ 0 + as.factor(year),
                     spde = spde,
                     data = haul_new,
                     time = "year",
                     control = sdmTMBcontrol(nlminb_loops = 2, newton_loops = 1),
                     priors = sdmTMBpriors(matern_s = pc_matern(range_gt = 75, sigma_lt = 5)),
                     spatial_only = TRUE,
                     family = tweedie(link = "log"))
saveRDS(model_4_full, "results/fit_spatial_nodepth_full.rds")


################################################################################

# Fit models after filtering spatial extent of data:

# data filtering based on latitude quantiles
haul_95 = dplyr::filter(haul_new, latitude <= quantile(latitude, 0.975),
                         latitude >= quantile(latitude, 0.025)) # 95% quantile

haul_90 = dplyr::filter(haul_new, latitude <= quantile(latitude, 0.95),
                         latitude >= quantile(latitude, 0.05)) # 90% quantile

haul_80 = dplyr::filter(haul_new, latitude <= quantile(latitude, 0.90),
                         latitude >= quantile(latitude, 0.1)) # 80% quantile

# make new meshes
spde_95 = make_mesh(
  haul_95,
  xy_cols = c("X","Y"),
  type = "cutoff",
  cutoff = 15,
  seed = 2021
)
spde_90 = make_mesh(
  haul_90,
  xy_cols = c("X","Y"),
  type = "cutoff",
  cutoff = 15,
  seed = 2021
)
spde_80 = make_mesh(
  haul_80,
  xy_cols = c("X","Y"),
  type = "cutoff",
  cutoff = 15,
  seed = 2021
)

model_1_full_95quantile <- sdmTMB(cpue_kg_km2 ~ 0 + as.factor(year) + log_depth_scaled + log_depth_scaled2,
                       spde = spde_95,
                       data = haul_95,
                       time = "year",
                       control = sdmTMBcontrol(nlminb_loops = 2, newton_loops = 1),
                       priors = sdmTMBpriors(matern_s = pc_matern(range_gt = 75, sigma_lt = 5)),
                       spatial_only = TRUE,
                       family = tweedie(link = "log"))
saveRDS(model_1_full_95quantile, "results/fit_spatial_depth_full_95quantile.rds")

model_1_full_90quantile <- sdmTMB(cpue_kg_km2 ~ 0 + as.factor(year) + log_depth_scaled + log_depth_scaled2,
                                  spde = spde_90,
                                  data = haul_90,
                                  time = "year",
                                  control = sdmTMBcontrol(nlminb_loops = 2, newton_loops = 1),
                                  priors = sdmTMBpriors(matern_s = pc_matern(range_gt = 75, sigma_lt = 5)),
                                  spatial_only = TRUE,
                                  family = tweedie(link = "log"))
saveRDS(model_1_full_90quantile, "results/fit_spatial_depth_full_90quantile.rds")

model_1_full_80quantile <- sdmTMB(cpue_kg_km2 ~ 0 + as.factor(year) + log_depth_scaled + log_depth_scaled2,
                                  spde = spde_80,
                                  data = haul_80,
                                  time = "year",
                                  control = sdmTMBcontrol(nlminb_loops = 2, newton_loops = 1),
                                  priors = sdmTMBpriors(matern_s = pc_matern(range_gt = 75, sigma_lt = 5)),
                                  spatial_only = TRUE,
                                  family = tweedie(link = "log"))
saveRDS(model_1_full_80quantile, "results/fit_spatial_depth_full_80quantile.rds")

## residuals
#
# # randomized-quantile/PIT:
# rq_res <- residuals(model_1_full)
# rq_res <- rq_res[is.finite(rq_res)] # some Inf
# qqnorm(rq_res);qqline(rq_res)
#
# # simulation-based; DHARMa:
# sim <- simulate(model_1_full, nsim = 500L)
# pred_fixed <- model_1_full$family$linkinv(predict(model_1_full, newdata = NULL)$est_non_rf)
# res_sim <- DHARMa::createDHARMa(
#   simulatedResponse = sim,
#   observedResponse = haul_new$cpue_kg_km2,
#   fittedPredictedResponse = pred_fixed
# )
# plot(res_sim)
#
# # MCMC-based
# # warning: slow!
# stan_fit <- tmbstan::tmbstan(model_1_full$tmb_obj, iter = 100L, warmup = 99L, chains = 1L)
# saveRDS(stan_fit, "results/stan_fit.rds")
# stan_fit <- readRDS("results/stan_fit.rds")
# stan_eta <- predict(model_1_full, tmbstan_model = stan_fit)
# stan_mu <- as.numeric(model_1_full$family$linkinv(stan_eta))
# mcmc_res <- sdmTMB:::qres_tweedie(model_1_full, y = haul_new$cpue_kg_km2, mu = stan_mu)
#
# png("plots/residuals.png", res = 300, width = 8, height = 3, units = "in")
# par(mar = c(3, 4, 2, 1), cex = 0.8, mfrow = c(1, 3), cex.main = 0.8)
# qqnorm(rq_res, cex = 0.5, main = "", xlab = "", ylab = "")
# qqline(rq_res, col = "red")
# mtext(side = 3, text = "Randomized quantile residuals", line = 0.5, cex = 0.8)
# mtext(side = 1, line = 2, text = "Expected", cex = 0.8)
# mtext(side = 2, line = 2, text = "Observed", cex = 0.8)
#
# gap::qqunif(
#   res_sim$scaledResiduals,
#   pch = 19,
#   bty = "n",
#   logscale = FALSE,
#   col = "#00000010",
#   cex = 0.5,
#   ann = FALSE,
#   # asp = 1,
# )
# mtext(side = 3, text = "Simulated-based uniform residuals (DHARMa)", line = 0.5, cex = 0.8)
# mtext(side = 1, line = 2, text = "Expected", cex = 0.8)
# mtext(side = 2, line = 2, text = "Observed", cex = 0.8)
# box()
#
# qqnorm(mcmc_res, cex = 0.5, main = "", xlab = "", ylab = "")
# qqline(mcmc_res, col = "red")
# mtext(side = 1, line = 2, text = "Expected", cex = 0.8)
# mtext(side = 2, line = 2, text = "Observed", cex = 0.8)
# mtext(side = 3, text = "MCMC-based residuals", line = 0.5, cex = 0.8)
# dev.off()

# png("plots/residuals-dharma.png", res = 300, width = 4, height = 4, units = "in")
# par(mar = c(3, 4, 2, 1), cex = 0.8, mfrow = c(1, 1), cex.main = 0.8)
# gap::qqunif(
#   res_sim$scaledResiduals,
#   pch = 19,
#   bty = "n",
#   logscale = FALSE,
#   col = "#00000010",
#   cex = 0.5,
#   ann = FALSE,
#   # asp = 1,
# )
# mtext(side = 3, text = "Simulated-based uniform residuals (DHARMa)", line = 0.5, cex = 0.8)
# mtext(side = 1, line = 2, text = "Expected", cex = 0.8)
# mtext(side = 2, line = 2, text = "Observed", cex = 0.8)
# box()
# dev.off()

