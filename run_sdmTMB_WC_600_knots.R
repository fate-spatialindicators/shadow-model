#devtools::install_github("pbs-assess/sdmTMB")
library(sdmTMB)

# Load data directly from previous model fit
m = readRDS("sablefish_600_density_depth_varying600knots.rds")

performance <- data.frame(
  #knots = n_knots
  knots = 600
)

for (k in 1:length(performance$knots)) {
  
  c_spde <- make_mesh(data = m$data, xy_cols = c("X", "Y"), type = "kmeans", n_knots = performance$knots[k])
  density_model <- sdmTMB(formula = cpue_kg_km2 ~ 0 + as.factor(year),
                          time_varying = ~ 0 + log_depth_scaled + log_depth_scaled2,
                          data = m$data,
                          time = "year", 
                          spde = c_spde, 
                          reml = TRUE, 
                          anisotropy = TRUE,
                          family = tweedie(link = "log"),
                          control = sdmTMBcontrol(step.min = 0.01, step.max = 1)
  )

  saveRDS(density_model, file=paste0("sablefish_",performance$knots[k],"_density_depth_varying_new.rds"))
  #saveRDS(density_model2, file=paste0("C:/Users/cjcco/OneDrive/Desktop/1st Paper/output/WC/",species[spp],"/",species[spp],"_",performance$knots[k],"_density_no_covar.rds"))
}