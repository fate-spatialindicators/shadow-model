## Make plots of difference in spatial predictions ##

library(ggplot2)
library(viridis)
library(raster)
options("rgdal_show_exportToProj4_warnings"="none")
library(rgdal)
library(dplyr)
library(scales)

# get coastlines and transform to projection and scale of data
shore_gcs <- rnaturalearth::ne_countries(continent = "north america", scale = "medium", returnclass = "sp")
shore <- sp::spTransform(shore_gcs, CRS = CRS(SRS_string='EPSG:32610'))
shore <- fortify(shore)
unit_scale <- 1000 # to change units from m to km
shore$long <- shore$long/unit_scale
shore$lat <- shore$lat/unit_scale

# get 200m contour
contour200 <- rgdal::readOGR("data/shelf_break_contour_shapefile/ne_10m_bathymetry_K_200.shp")
contour200_crop <- raster::crop(contour200, extent(-150, -100, 20, 70))
contour200_crop <- sp::spTransform(contour200_crop, CRS = CRS(SRS_string='EPSG:32610'))
contour200_df <- fortify(contour200_crop)
contour200_df$long <- contour200_df$long/unit_scale
contour200_df$lat <- contour200_df$lat/unit_scale

# get population density predictions
p_1 = readRDS("results/predictions_1x.rds")
p_4 = readRDS("results/predictions_4x.rds")

# filter to specific year for example
year_ex = 2018
p_1_ex = p_1$data %>%
  mutate(exp_est = exp(est)) %>%
  filter(year == year_ex) %>%
  select(X, Y, est, exp_est, depth) # "est" is pop density on log scale, "exp_est" on linear scale
p_4_ex = p_4$data %>%
  mutate(exp_est = exp(est)) %>%
  filter(year == year_ex) %>%
  select(X, Y, est, exp_est, depth)

# rasters of predicted densities and depth
r_1 = rasterFromXYZ(p_1_ex, crs = CRS(SRS_string='EPSG:32610'))
r_4 = rasterFromXYZ(p_4_ex, crs = CRS(SRS_string='EPSG:32610'))

# disaggregate coarse resolution to fine resolution to map the difference at the finer resolution
r_4 = disaggregate(r_4, fact = 4)
# crop to equalize extents
r_1 <- crop(r_1, r_4)
all.equal(extent(r_1), extent(r_4))
## Take the difference in predictions and depth, doing so in log space to compute the log ratio
r_diff = overlay(r_1[["est"]], r_4[["est"]], fun = function(x,y){(x-y)})
r_diff2 = overlay(r_1[["depth"]], r_4[["depth"]], fun = function(x,y){(x-y)})
names(r_diff) = "diff_est"
names(r_diff2) = "diff_depth"
r_all = stack(r_diff, r_diff2, r_1)

r_diff_df = as.data.frame(rasterToPoints(r_all))

# make a map of the log ratio of depth --------------
  ggplot() +
    geom_tile(data = r_diff_df, aes(x, y, fill = diff_est)) +
    scale_fill_gradient2("Log Ratio of Density", low = "blue", high = "red", mid = "grey92", na.value = "white") +
    annotation_map(shore, color = "black", fill = "cornsilk", size = 0.1) +
    ggthemes::theme_map() +
    theme(
      axis.text = element_text(),
      axis.ticks = element_line(),
      axis.title = element_text(size = 15),
      panel.border = element_rect(colour = "black", fill = NA, size = 2),
      legend.position = c(0.01, 0.01)) +
    geom_path(data = contour200_df, aes(x = long, y = lat, group = group), color = "darkcyan", size = 0.4) +
    coord_cartesian(xlim = c(290, 715), ylim= c(3890, 5305)) +
    xlab("Eastings (km)") +
    ylab("Northings (km)")

ggsave("plots/difference_density_estimate_map.pdf", width = 4.5, height = 8.35, units = "in")


# make scatter plot of log ratio in predicted density and depth with smoother --------------
ggplot(r_diff_df, aes(x=abs(diff_depth), y=abs(diff_est))) +
  geom_point(col = alpha("darkblue", 0.1), size=2) +
  stat_smooth(method = "loess", formula = y ~ x, size = 1.5, se = FALSE, colour = "black") +
  xlab("Difference in Depth (m)") +
  ylab("Log-Ratio of Population Density Predictions") +
  scale_y_continuous(breaks = seq(0, 10, by = 2))

ggsave("plots/difference_density_estimate_depth_biplot.pdf", width = 4.5, height = 4, units = "in")
