#### Generate Biomass and COG Estimates ####

library(sdmTMB)

# select years to predict and gather replicate prediction data for each of these years
years = 2003:2018
#wc_grid = readRDS("data/wc_grid_1x.rds")
wc_grid = readRDS("data/wc_grid_4x.rds")
Predict_data_years = wc_grid
Predict_data_years$year = 2003

for(i in 2:length(unique(years))) {
  wc_grid$year = unique(years)[i]
  Predict_data_years = rbind(Predict_data_years, wc_grid)
}
rm(wc_grid)

# load fitted model
m = readRDS("results/fit_spatial_depth_full.rds")

# make timeseries plots of COG, biomass index with 95% CI from model estimates
p = predict(m, newdata = Predict_data_years, return_tmb_object = TRUE)
rm(Predict_data_years)
saveRDS(p, "results/predictions_4x.rds")

# compute indices (takes a lot of memory for 1x resolution predictions)
gc()
biomass_estimates = get_index(p, bias_correct = TRUE) # bias correction on
saveRDS(biomass_estimates, "results/biomass_4x_bias_corrected.rds")
rm(biomass_estimates)
gc()
COG = get_cog(p, bias_correct = TRUE)
saveRDS(COG, "results/COG_4x_bias_corrected.rds")
