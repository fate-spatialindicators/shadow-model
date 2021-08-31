# Fitting spatiotemporal models to West Coast Groundfish Bottom Trawl data with sdmTMB

#devtools::install_github("pbs-assess/sdmTMB")
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

# spatial only + depth
model_1 <- sdmTMB_cv(cpue_kg_km2 ~ 0 + as.factor(year) + log_depth_scaled + log_depth_scaled2,
                     spde = spde,
                     k_folds = max(haul_new$fold),
                     fold_ids = haul_new$fold,
                     data = haul_new,
                     priors = sdmTMBpriors(matern_s = pc_matern(range_gt = 75, sigma_lt = 5)),
                     spatial_only = TRUE,
                     family = tweedie(link = "log"))
saveRDS(model_1, "results/fit_spatial_depth.rds")

# IID
model_2 <- sdmTMB_cv(cpue_kg_km2 ~ 0 + as.factor(year) + log_depth_scaled + log_depth_scaled2,
                     spde = spde,
                     k_folds = max(haul_new$fold),
                     fold_ids = haul_new$fold,
                     data = haul_new,
                     time = "year",
                     priors = sdmTMBpriors(matern_s = pc_matern(range_gt = 75, sigma_lt = 5)),
                     fields = "IID",
                     spatial_only = FALSE,
                     family = tweedie(link = "log"))
saveRDS(model_2, "results/fit_IID_depth.rds")

# AR1
model_3 <- sdmTMB_cv(cpue_kg_km2 ~ 0 + as.factor(year) + log_depth_scaled + log_depth_scaled2,
                     spde = spde,
                     k_folds = max(haul_new$fold),
                     fold_ids = haul_new$fold,
                     data = haul_new,
                     time = "year",
                     priors = sdmTMBpriors(matern_s = pc_matern(range_gt = 75, sigma_lt = 5)),
                     fields = "AR1",
                     spatial_only = FALSE,
                     family = tweedie(link = "log"))
saveRDS(model_3, "results/fit_AR1_depth.rds")

# spatial only - depth
model_4 <- sdmTMB_cv(cpue_kg_km2 ~ 0 + as.factor(year),
                     spde = spde,
                     k_folds = max(haul_new$fold),
                     fold_ids = haul_new$fold,
                     data = haul_new,
                     priors = sdmTMBpriors(matern_s = pc_matern(range_gt = 75, sigma_lt = 5)),
                     spatial_only = TRUE,
                     family = tweedie(link = "log"))
saveRDS(model_4, "results/fit_spatial_nodepth.rds")

# fit best fit model structure (spatial only + depth) to full dataset
model_1_full <- sdmTMB(cpue_kg_km2 ~ 0 + as.factor(year) + log_depth_scaled + log_depth_scaled2,
                       spde = spde,
                       data = haul_new,
                       time = "year",
                       priors = sdmTMBpriors(matern_s = pc_matern(range_gt = 75, sigma_lt = 5)),
                       spatial_only = TRUE,
                       family = tweedie(link = "log"))
saveRDS(model_1_full, "results/fit_spatial_depth_full.rds")


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
                       priors = sdmTMBpriors(matern_s = pc_matern(range_gt = 75, sigma_lt = 5)),
                       spatial_only = TRUE,
                       family = tweedie(link = "log"))
saveRDS(model_1_full_95quantile, "results/fit_spatial_depth_full_95quantile.rds")

model_1_full_90quantile <- sdmTMB(cpue_kg_km2 ~ 0 + as.factor(year) + log_depth_scaled + log_depth_scaled2,
                                  spde = spde_90,
                                  data = haul_90,
                                  time = "year",
                                  priors = sdmTMBpriors(matern_s = pc_matern(range_gt = 75, sigma_lt = 5)),
                                  spatial_only = TRUE,
                                  family = tweedie(link = "log"))
saveRDS(model_1_full_90quantile, "results/fit_spatial_depth_full_90quantile.rds")

model_1_full_80quantile <- sdmTMB(cpue_kg_km2 ~ 0 + as.factor(year) + log_depth_scaled + log_depth_scaled2,
                                  spde = spde_80,
                                  data = haul_80,
                                  time = "year",
                                  priors = sdmTMBpriors(matern_s = pc_matern(range_gt = 75, sigma_lt = 5)),
                                  spatial_only = TRUE,
                                  family = tweedie(link = "log"))
saveRDS(model_1_full_80quantile, "results/fit_spatial_depth_full_80quantile.rds")
