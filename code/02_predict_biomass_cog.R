#### Generate Biomass and COG Estimates ####

library(sdmTMB)

# select years to predict and gather replicate prediction data for each of these years
years = 2003:2018
wc_grid = readRDS("data/wc_grid_1x.rds")
wc_grid_predict = wc_grid
wc_grid_predict$year = 2003

for(i in 2:length(unique(years))) {
  wc_grid$year = unique(years)[i]
  wc_grid_predict = rbind(wc_grid_predict, wc_grid)
}
rm(wc_grid)

# load fitted model
m = readRDS("results/fit_spatial_depth_full.rds")

# predict to grid to estimate COG, biomass index with CI (default 95%)
p = predict(m, newdata = wc_grid_predict, return_tmb_object = TRUE)
saveRDS(p, "results/predictions_1x.rds")

# compute indices
biomass_estimates = get_index(p, bias_correct = TRUE)
saveRDS(biomass_estimates, "results/biomass_1x_bias_corrected.rds")
rm(biomass_estimates)
gc()

COG = get_cog(p, bias_correct = TRUE)
saveRDS(COG, "results/COG_1x_bias_corrected.rds")
rm(COG, p)
gc()

#---------------------------------------------------------------------------------

# repeat above computations at 4x the spatial resolution of prediction grid

wc_grid_4x = readRDS("data/wc_grid_4x.rds")
wc_grid_4x_predict = wc_grid_4x
wc_grid_4x_predict$year = 2003

for(i in 2:length(unique(years))) {
  wc_grid_4x$year = unique(years)[i]
  wc_grid_4x_predict = rbind(wc_grid_4x_predict, wc_grid_4x)
}
rm(wc_grid_4x)

# predict to grid to estimate COG, biomass index with 95% CI
p_4x = predict(m, newdata = wc_grid_4x_predict, return_tmb_object = TRUE)
saveRDS(p_4x, "results/predictions_4x.rds")

# compute indices
biomass_estimates_4x = get_index(p_4x, bias_correct = TRUE)
saveRDS(biomass_estimates_4x, "results/biomass_4x_bias_corrected.rds")
rm(biomass_estimates_4x)
gc()

COG_4x = get_cog(p_4x, bias_correct = TRUE)
saveRDS(COG_4x, "results/COG_4x_bias_corrected.rds")
rm(COG_4x, p_4x)
gc()

#---------------------------------------------------------------------------------

# compute predictions and indices for alternative model with spatiotemporal fields
# here without bias correction to avoid memory allocation limits

m_IID = readRDS("results/fit_IID_depth_full.rds")

p_IID = predict(m_IID, newdata = wc_grid_predict, return_tmb_object = TRUE)
saveRDS(p_IID, "results/predictions_1x_IID.rds")

biomass_estimates_IID = get_index(p_IID)
saveRDS(biomass_estimates_IID, "results/biomass_1x_IID.rds")
rm(biomass_estimates_IID)
gc()

COG_IID = get_cog(p_IID)
saveRDS(COG_IID, "results/COG_1x_IID.rds")
rm(COG_IID, p_IID)
gc()


# alternative model predicted to coarse (4x) resolution prediction grid
p_IID_4x = predict(m_IID, newdata = wc_grid_4x_predict, return_tmb_object = TRUE)
saveRDS(p_IID_4x, "results/predictions_4x_IID.rds")

biomass_estimates_IID_4x = get_index(p_IID_4x)
saveRDS(biomass_estimates_IID_4x, "results/biomass_4x_IID.rds")
rm(biomass_estimates_IID_4x)
gc()

COG_IID_4x = get_cog(p_IID_4x)
saveRDS(COG_IID_4x, "results/COG_4x_IID.rds")
rm(COG_IID_4x, m_IID, p_IID_4x)
gc()

#---------------------------------------------------------------------------------

# compute predictions and indices for alternative models with latitude filtering

m_95 = readRDS("results/fit_spatial_depth_full_95quantile.rds")

p_95 = predict(m_95, newdata = wc_grid_predict, return_tmb_object = TRUE)
saveRDS(p_95, "results/predictions_1x_95quantile.rds")

biomass_estimates_95 = get_index(p_95, bias_correct = TRUE)
saveRDS(biomass_estimates_95, "results/biomass_1x_95quantile_bias_corrected.rds")
rm(biomass_estimates_95)
gc()

COG_95 = get_cog(p_95, bias_correct = TRUE)
saveRDS(COG_95, "results/COG_1x_95quantile_bias_corrected.rds")
rm(COG_95, m_95, p_95)
gc()


m_90 = readRDS("results/fit_spatial_depth_full_90quantile.rds")

p_90 = predict(m_90, newdata = wc_grid_predict, return_tmb_object = TRUE)
saveRDS(p_90, "results/predictions_1x_90quantile.rds")

biomass_estimates_90 = get_index(p_90, bias_correct = TRUE)
saveRDS(biomass_estimates_90, "results/biomass_1x_90quantile_bias_corrected.rds")
rm(biomass_estimates_90)
gc()

COG_90 = get_cog(p_90, bias_correct = TRUE)
saveRDS(COG_90, "results/COG_1x_90quantile_bias_corrected.rds")
rm(COG_90, m_90, p_90)
gc()


m_80 = readRDS("results/fit_spatial_depth_full_80quantile.rds")

p_80 = predict(m_80, newdata = wc_grid_predict, return_tmb_object = TRUE)
saveRDS(p_80, "results/predictions_1x_80quantile.rds")

biomass_estimates_80 = get_index(p_80, bias_correct = TRUE)
saveRDS(biomass_estimates_80, "results/biomass_1x_80quantile_bias_corrected.rds")
rm(biomass_estimates_80)
gc()

COG_80 = get_cog(p_80, bias_correct = TRUE)
saveRDS(COG_80, "results/COG_1x_80quantile_bias_corrected.rds")
rm(COG_80, m_80, p_80)
gc()
