## Residuals Plot ##

#install.packages("remotes")
#remotes::install_github("pbs-assess/sdmTMB")

library(sdmTMB)

# Load data directly from previous model fit
m = readRDS("sablefish_600_density_depth_varying600knots.rds")

  c_spde = make_mesh(data = m$data, xy_cols = c("X", "Y"), type = "kmeans", n_knots = 600)
  
  m1 = sdmTMB(formula = cpue_kg_km2 ~ 0 + as.factor(year),
                          time_varying = ~ 0 + log_depth_scaled + log_depth_scaled2,
                          data = m$data,
                          time = "year", 
                          spde = c_spde, 
                          reml = TRUE, 
                          anisotropy = TRUE,
                          family = tweedie(link = "log"),
                          control = sdmTMBcontrol(step.min = 0.01, step.max = 1)
  )
  
# plot SPDE mesh
plot(c_spde)

# residuals
residuals_m1 = residuals(m1)

# Q-Q plot
qqnorm(residuals_m1);abline(a = 0, b = 1)


# Mesh plot
sp = make_mesh(m$data, c("X", "Y"), n_knots = 600, type = "kmeans")

plot(sp$mesh, main = NA, edge.color = "grey60", asp = 1)
points(sp$loc_xy, pch = ".", col = rgb(red=1,blue=0.4,green=0,alpha=0.2))
#points(sp$loc_centers, pch = 20, col = rgb(red=1,blue=0,green=0,alpha=0.1), cex = 2, alpha=0.01)
