## Make plots of difference in spatial predictions for sdmTMB West Coast ##

library(ggplot2)
library(viridis)
library(raster)
library(dplyr)
library(sf)

#setwd("C:/Users/lewis.barnett/Downloads")

# get coastlines and transform to projection and scale of data
shore <- rnaturalearth::ne_countries(continent = "north america", scale = "medium", returnclass = "sp")
shore <- sp::spTransform(shore, CRS = "+proj=utm +zone=10 ellps=WGS84")
shore <- fortify(shore)
shore$long <- shore$long/1000
shore$lat <- shore$lat/1000

# get 200m contour
contour200 <- rgdal::readOGR("C:/Users/cjcco/Box/Best Practices paper/Shelf_Break_Contour_Shapefile/ne_10m_bathymetry_K_200.shp")
contour200_crop <- raster::crop(contour200, extent(-150, -100, 20, 70))
contour200_crop <- sp::spTransform(contour200_crop, CRS = "+proj=utm +zone=10 ellps=WGS84")
contour200_df <- fortify(contour200_crop)
contour200_df$long <- contour200_df$long/1000
contour200_df$lat <- contour200_df$lat/1000

# going to take a shortcut here to get to your actual predictions for illustrative purposes as I can't find all the individual pieces I need more directly
load("C:/Users/cjcco/Box/Best Practices paper/plotdata_all_1x.Rdata")
#load("plotdata_all_1x.Rdata")
p_1 = p
load("C:/Users/cjcco/Box/Best Practices paper/plotdata_all_4x.Rdata")
#load("plotdata_all_4x.Rdata")
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
# Feel free to play around but this looks more interesting than the difference of densities on a linear scale...
# but, For example, you could take the difference in linear space and then show the log of that?

  # bin depths for color grouping in ggplot
  df = r_diff_df %>%
    mutate(depth_bins = cut(abs(r_diff_df$depth), 
                            breaks = c(0, 150, 250, c(350, 1700))))
  
  # Plot of difference in depth (meters; x) and difference in density estimates (log-ratio; y) [absolute values]
  pf = ggplot(df, aes(x=abs(diff_depth), y=abs(diff_est))) + 
    geom_point(aes(color = (depth_bins)),alpha=0.5,pch=16) +
    ggtitle("Difference in Depth vs. Log-Ratio of Predicted Density Estimates Between 1x and 4x for 2018") + 
    xlab("Difference in Depth (m)") + 
    ylab("Difference in Density Estimates (Log-Ratio)") +
    scale_y_continuous(breaks = seq(0, 10, by = 2)) + 
    scale_x_continuous(breaks = seq(0, 1000, by = 100)) + 
    scale_color_viridis(discrete = TRUE, direction = -1)
  # loess line (span = 0.50 for more wiggliness)
  pf + stat_smooth(method = "loess", formula = y ~ x, size = 1.5, span = 0.50, se = FALSE, colour = "black") + 
    theme_classic() # remove background

##################################

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
