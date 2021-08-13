## Make plots of difference in spatial predictions for sdmTMB West Coast ##

library(ggplot2)
library(viridis)
library(raster)
library(rgdal)
library(dplyr)
library(scales)
library(ggnewscale) # required to plot bathymetry and predicted density in same panel with diff scales

# get coastlines and transform to projection and scale of data
shore <- rnaturalearth::ne_countries(continent = "north america", scale = "medium", returnclass = "sp")
shore <- sp::spTransform(shore, CRS = "+proj=utm +zone=10 ellps=WGS84")
shore <- fortify(shore)
shore$long <- shore$long/1000
shore$lat <- shore$lat/1000

# get 200m contour
contour200 <- rgdal::readOGR("data/shelf_break_contour_shapefile/ne_10m_bathymetry_K_200.shp")
contour200_crop <- raster::crop(contour200, extent(-150, -100, 20, 70))
contour200_crop <- sp::spTransform(contour200_crop, CRS = "+proj=utm +zone=10 ellps=WGS84")
contour200_df <- fortify(contour200_crop)
contour200_df$long <- contour200_df$long/1000
contour200_df$lat <- contour200_df$lat/1000

# get population density predictions
load("data/plotdata_all_1x.Rdata")
p_1 = p
load("data/plotdata_all_4x.Rdata")
p_4 = p

# filter to specific year for example
year_ex = 2018
p_1_ex = p_1 %>%
  mutate(exp_est = exp(est)) %>%
  filter(year == year_ex) %>%
  select(X, Y, est, exp_est, depth) # density estimates ("est") already in log space. select "exp_est" for exponentiated density estimates
p_4_ex = p_4 %>%
  mutate(exp_est = exp(est)) %>%
  filter(year == year_ex) %>%
  select(X, Y, est, exp_est, depth)

# rasters of predicted densities and depth
r_1 = rasterFromXYZ(p_1_ex, crs = "+proj=utm +zone=10 ellps=WGS84")
r_4 = rasterFromXYZ(p_4_ex, crs = "+proj=utm +zone=10 ellps=WGS84")

# disaggregate coarse resolution to fine resolution, with no method specified cells it just takes the value of the larger original cells
r_4 = disaggregate(r_4, fact = 4)
# crop to equalize extents
r_1 <- crop(r_1, r_4)
all.equal(extent(r_1), extent(r_4))
## Combine rasters with difference as function  (note that you may want to use log ("est") or exponentiated density ("exp_est") based on what looks better)
# (also note that if you take the difference of logs you are computing the log ratio: log(x/y))
r_diff = overlay(r_1[["est"]], r_4[["est"]], fun = function(x,y){(x-y)})
r_diff2 = overlay(r_1[["depth"]], r_4[["depth"]], fun = function(x,y){(x-y)}) # calculate difference in depth
names(r_diff) = "diff_est"
names(r_diff2) = "diff_depth" # rename column
# stack differenced density with other layers
#r_all = stack(r_diff, r_1)
#r_all2 = stack(r_diff2, r_1) # stack
r_all = stack(r_diff, r_diff2, r_1)

# quick view
#image(r_diff)

# other plotting options include levelplot or....
r_diff_df = as.data.frame(rasterToPoints(r_all))
#r_diff_df2 = as.data.frame(rasterToPoints(r_all2)) # make into df
#r_diff_df[, "diff_depth"] = r_diff_df2[,3] # add diff_depth column to main df

  ggplot() +
    geom_raster(data = r_diff_df, aes(x, y, fill = diff_est)) +
    scale_fill_gradient2("Log-Ratio of Density", low = "blue", high = "red") +
    annotation_map(shore, color = "black", fill = "white", size = 0.1) +
    #ggthemes::theme_map() +
    geom_path(data = contour200_df, aes(x = long, y = lat, group = group), color = "darkcyan", size = 0.5) +
    coord_cartesian(xlim = c(285, 1005), ylim= c(3550, 5290)) +
    xlab("Eastings (km)") +
    ylab("Northings (km)") +
    ggtitle("Log-Ratio of Density Estimates Between 1x and 4x Resolution for 2018")

# Largest differences are just in 2 small locations where there are deep canyons, which explains why the diffs are so great there due to depth averaging
# Lets visualize this by plotting bathymetry next to the raster of differences in density

  ## load custom bathymetry raster
  raster_hiRes <- raster("data/bathy_clipped")
  # Coarse-grain to speed plotting
  raster_lowRes = aggregate(raster_hiRes, fact = 15, fun = mean)
  raster_lowRes = raster_lowRes / 10 # units were originally decimeters, so convert to meters
  # set cells outside survey bounds to NA
  raster_lowRes = reclassify(raster_lowRes, c(-Inf, -1280, NA))
  raster_lowRes = reclassify(raster_lowRes, c(-50, Inf, NA), right = FALSE)
  # load Cowcod Conservation Areas, not included in trawl survey
  CCA = rgdal::readOGR('data/cowcod_conservation_area_shapefile/kv299cy7357.shp')
  identical(crs(CCA),crs(raster_lowRes)) # check that have same CRS
  # mask CCA from bathymetry raster
  raster_masked = raster::mask(raster_lowRes, CCA, inverse = TRUE)
  # reproject to match density predictions
  bathy = projectRaster(raster_masked, crs = "+proj=utm +zone=10 ellps=WGS84")
  bathy_df = as.data.frame(rasterToPoints(bathy))
  # truncate southern california for ease of viewing side-by side with predictions
  bathy_df = filter(bathy_df, y > 3825000)
  r_diff_df = filter(r_diff_df, y > 3825)
  # rescale and maybe shift for plotting alongside densities?
  bathy_df_shift = dplyr::mutate(bathy_df, x = (x/1000) - 100, y = y/1000)

  ggplot() +
    geom_raster(data = r_diff_df, aes(x, y, fill = diff_est)) +
    scale_fill_gradient2("Log Ratio of Density", low = "darkblue", high = "darkred", na.value = "white") +
    annotation_map(shore, color = "black", fill = "cornsilk", size = 0.1) +
    ggthemes::theme_map() +
    theme(
      axis.text = element_text(),
      axis.ticks = element_line(),
      axis.title = element_text()) +
    geom_path(data = contour200_df, aes(x = long, y = lat, group = group), color = "darkcyan", size = 0.5) +
    new_scale_fill() +
    geom_tile(data = bathy_df_shift, aes(x, y, fill = bathy_clipped)) +
    scale_fill_viridis_c(name = "Depth (m)") +
    coord_cartesian(xlim = c(200, 715), ylim= c(3870, 5305)) +
    xlab("Eastings (km)") +
    ylab("Northings (km)")


  # #Too difficult to see if overlayed with transparency
  # ggplot() +
  #   geom_tile(data = bathy_df_shift, aes(x, y, fill = bathy_clipped)) +
  #   scale_fill_viridis_c(name = "Depth (m)") +
  #   new_scale_fill() +
  #   geom_raster(data = r_diff_df, aes(x, y, fill = diff_est, alpha = 0.95)) +
  #    scale_fill_gradient2("Log Ratio of Density", low = "black", high = "black", na.value = "white") +
  #   annotation_map(shore, color = "black", fill = "cornsilk", size = 0.1) +
  #   ggthemes::theme_map() +
  #   theme(
  #     axis.text = element_text(),
  #     axis.ticks = element_line(),
  #     axis.title = element_text()) +
  #   geom_path(data = contour200_df, aes(x = long, y = lat, group = group), color = "darkcyan", size = 0.5) +
  #   coord_cartesian(xlim = c(285, 1005), ylim= c(3550, 5290)) +
  #   xlab("Eastings (km)") +
  #   ylab("Northings (km)")




# Scatterplots of differences in predicted density and depth --------------

  # Plot of difference in depth (meters; x) and difference in density estimates (log-ratio; y)
  plot(abs(r_diff_df$diff_depth), abs(r_diff_df$diff_est), main="Difference in Depth vs. Log-Ratio of Predicted Density Estimates Between 1x and 4x for 2018",
       xlab="Difference in Depth (m)", ylab="Difference in Density Estimates (Log-Ratio)", col = alpha("darkblue", 0.3), pch=16)
  # absolute values?


# histogram (probability density) of density estimates (log) between fine (1x) and coarse (4x) resolutions
ggplot(NULL, aes(x=est)) +
  geom_density(data = p_1_ex, aes(x=est, color = "darkblue", fill = "lightblue")) +
  geom_density(data = p_4_ex, aes(x=est, color = "darkred", fill = "red")) +
  ggtitle("Probability Density of Predicted (Log) Density Estimates Between Fine and Coarse Resolutions") +
  xlab("Predicted (Log) Density Estimates") +
  ylab("Probability Density") +
  scale_color_identity(guide = "none") +
  scale_fill_identity(guide = "legend", labels = c("Fine (1x)", "Coarse (4x)"), name = "Grid Resolution")

# Plot of depth vs. log-ratio of density (absolute values)
library(scales)
plot(abs(r_diff_df$depth), abs(r_diff_df$diff_est),main="Depth vs. Log-Ratio of Predicted Density Estimates (absolute value) Between 1x and 4x for 2018",
     xlab="Depth (m)", ylab="Difference in Density Estimates (Log-Ratio)", col = alpha("darkblue", 0.1), pch=16)
# absolute log-ratio values to see at what depths are the greatest overall differences

# Plot of depth vs. log-ratio of density [absolute values] w/ loess line
pf = ggplot(r_diff_df, aes(x=abs(depth), y=abs(diff_est))) +
  geom_point(col = alpha("darkblue", 0.1), size=2) +
  ggtitle("Depth vs. Log-Ratio of Predicted Density Estimates (absolute value) Between 1x and 4x for 2018") +
  xlab("Depth (m)") +
  ylab("Difference in Density Estimates (Log-Ratio)") +
  scale_y_continuous(breaks = seq(0, 10, by = 2))
pf + stat_smooth(method = "loess", formula = y ~ x, size = 1.5, se = FALSE, colour = "black")


# Plot of depth vs. density estimate (log) for Fine (1x) Resolution for 2018
plot(abs(p_1_ex$depth), p_1_ex$est, main="Depth (m) vs. Density Estimate (log) for Fine (1x) Resolution for 2018",
     xlab="Depth (m)", ylab="Density Estimate (log)", col = alpha("darkblue", 0.3), pch=16)
# visualize at what depths is predicted density highest
