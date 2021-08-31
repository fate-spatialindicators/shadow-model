#### Generate Biomass and COG Estimates ####

library(sdmTMB)

# select years to predict and gather replicate prediction data for each of these years
years = 2003:2018
wc_grid = readRDS("data/wc_grid_1x.rds")
#wc_grid = readRDS("data/wc_grid_4x.rds")
Predict_data_years = wc_grid
Predict_data_years$year = 2003

for(i in 2:length(unique(years))) {
  wc_grid$year = unique(years)[i]
  Predict_data_years = rbind(Predict_data_years, wc_grid)
}
rm(wc_grid)

# load fitted model
m = readRDS("results/fit_spatial_depth_full.rds")

# predict to grid to estimate COG, biomass index with 95% CI
p = predict(m, newdata = Predict_data_years, return_tmb_object = TRUE)
saveRDS(p, "results/predictions_1x.rds")

# compute indices (takes a lot of memory for 1x resolution predictions)
biomass_estimates = get_index(p, bias_correct = TRUE) # bias correction on
saveRDS(biomass_estimates, "results/biomass_1x_bias_corrected.rds")
rm(biomass_estimates)
gc()

COG = get_cog(p, bias_correct = TRUE)
saveRDS(COG, "results/COG_1x_bias_corrected.rds")
rm(COG, m, p)
gc()

################################################################################

# compute predictions and indices for alternative models with latitude filtering

m_95 = readRDS("results/fit_spatial_depth_full_95quantile.rds")

p_95 = predict(m_95, newdata = Predict_data_years, return_tmb_object = TRUE)
saveRDS(p_95, "results/predictions_1x_95quantile.rds")

biomass_estimates_95 = get_index(p_95, bias_correct = TRUE) # bias correction on
saveRDS(biomass_estimates_95, "results/biomass_1x_95quantile_bias_corrected.rds")
rm(biomass_estimates_95)
gc()

COG_95 = get_cog(p_95, bias_correct = TRUE)
saveRDS(COG_95, "results/COG_1x_95quantile_bias_corrected.rds")
rm(COG_95, m_95, p_95)
gc()


m_90 = readRDS("results/fit_spatial_depth_full_90quantile.rds")

p_90 = predict(m_90, newdata = Predict_data_years, return_tmb_object = TRUE)
saveRDS(p_90, "results/predictions_1x_90quantile.rds")

biomass_estimates_90 = get_index(p_90, bias_correct = TRUE) # bias correction on
saveRDS(biomass_estimates_90, "results/biomass_1x_90quantile_bias_corrected.rds")
rm(biomass_estimates_90)
gc()

COG_90 = get_cog(p_90, bias_correct = TRUE)
saveRDS(COG_90, "results/COG_1x_90quantile_bias_corrected.rds")
rm(COG_90, m_90, p_90)
gc()


m_80 = readRDS("results/fit_spatial_depth_full_80quantile.rds")

p_80 = predict(m_80, newdata = Predict_data_years, return_tmb_object = TRUE)
saveRDS(p_80, "results/predictions_1x_80quantile.rds")

biomass_estimates_80 = get_index(p_80, bias_correct = TRUE) # bias correction on
saveRDS(biomass_estimates_80, "results/biomass_1x_80quantile_bias_corrected.rds")
rm(biomass_estimates_80)
gc()

COG_80 = get_cog(p_80, bias_correct = TRUE)
saveRDS(COG_80, "results/COG_1x_80quantile_bias_corrected.rds")
rm(COG_80, m_80, p_80)
gc()
