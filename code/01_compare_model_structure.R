# Fitting spatiotemporal models to West Coast Groundfish Bottom Trawl data with sdmTMB

#devtools::install_github("pbs-assess/sdmTMB")
library(sdmTMB)

haul_new = readRDS("data/sablefish_cleaned.rds")

# Fit models to the sablefish data.
# 1. spatial only
# 2. IID spatiotemporal model
# 3. AR1 spatiotemporal model

# Make mesh, similar to same resolution as west coast stock
# assessments
spde = make_mesh(
  haul_new,
  xy_cols = c("X","Y"),
  type = "cutoff",
  cutoff=15,
  seed = 2021
)

# spatial only + depth
model_1 <- sdmTMB_cv(cpue_kg_km2 ~ 0 + as.factor(year) + log_depth_scaled + log_depth_scaled2,
                     spde = spde,
                     k_folds = max(haul_new$fold),
                     fold_ids = haul_new$fold,
                     data = haul_new,
                     priors = sdmTMBpriors(matern_s = pc_matern(range_gt = 75, sigma_lt = 5)),
                     spatial_only = TRUE,
                     family = tweedie(link = "log"))
saveRDS(model_1, "fit_spatial_depth.rds")

# IID
model_2 <- sdmTMB_cv(cpue_kg_km2 ~ 0 + as.factor(year)+ log_depth_scaled + log_depth_scaled2,
                     spde = spde,
                     k_folds = max(haul_new$fold),
                     fold_ids = haul_new$fold,
                     data = haul_new,
                     time = "year",
                     priors = sdmTMBpriors(matern_s = pc_matern(range_gt = 75, sigma_lt = 5)),
                     fields = "IID",
                     spatial_only = FALSE,
                     family = tweedie(link = "log"))
saveRDS(model_2, "fit_IID_depth.rds")

# AR1
model_3 <- sdmTMB_cv(cpue_kg_km2 ~ 0 + as.factor(year)+ log_depth_scaled + log_depth_scaled2,
                     spde = spde,
                     k_folds = max(haul_new$fold),
                     fold_ids = haul_new$fold,
                     data = haul_new,
                     time = "year",
                     priors = sdmTMBpriors(matern_s = pc_matern(range_gt = 75, sigma_lt = 5)),
                     fields = "AR1",
                     spatial_only = FALSE,
                     family = tweedie(link = "log"))
saveRDS(model_3, "fit_AR1_depth.rds")

# spatial only - depth
model_4 <- sdmTMB_cv(cpue_kg_km2 ~ 0 + as.factor(year),
                     spde = spde,
                     k_folds = max(haul_new$fold),
                     fold_ids = haul_new$fold,
                     data = haul_new,
                     priors = sdmTMBpriors(matern_s = pc_matern(range_gt = 75, sigma_lt = 5)),
                     spatial_only = TRUE,
                     family = tweedie(link = "log"))
saveRDS(model_4, "fit_spatial_nodepth.rds")

# fit best fit model structure (spatial only + depth) to full dataset
model_1_full <- sdmTMB(cpue_kg_km2 ~ 0 + as.factor(year) + log_depth_scaled + log_depth_scaled2,
                       spde = spde,
                       data = haul_new,
                       priors = sdmTMBpriors(matern_s = pc_matern(range_gt = 75, sigma_lt = 5)),
                       spatial_only = TRUE,
                       family = tweedie(link = "log"))
saveRDS(model_1_full, "fit_spatial_depth_full.rds")
