#### Sablefish Case Study data preparation ####

library(sp)
library(raster)
library(dplyr)

# Prepare data
# haul data includes environmental covariates with location information
haul = nwfscSurvey::PullHaul.fn(YearRange = c(2003,2018),
                                SurveyName = "NWFSC.Combo")

# project lat/lon to UTM, after removing missing values and unsatisfactory hauls
haul = haul %>% filter(!is.na(longitude_dd), !is.na(latitude_dd), performance == "Satisfactory")
haul_trans = haul
coordinates(haul_trans) <- c("longitude_dd", "latitude_dd")
proj4string(haul_trans) <- CRS("+proj=longlat +datum=WGS84")
newproj = paste("+proj=utm +zone=10 ellps=WGS84")
unit_scale <- 1000
haul_trans <- spTransform(haul_trans, CRS(newproj))
haul_trans = as.data.frame(haul_trans)
haul_trans$longitude_dd = haul_trans$longitude_dd/unit_scale
haul_trans$latitude_dd = haul_trans$latitude_dd/unit_scale
haul_trans$year = as.numeric(substr(haul_trans$date_yyyymmdd,1,4))
haul$X = haul_trans$longitude_dd
haul$Y = haul_trans$latitude_dd
haul$year = haul_trans$year

# center and scale depth, removing NAs
haul = dplyr::filter(haul, !is.na(depth_hi_prec_m))
haul$log_depth_scaled = (log(haul$depth_hi_prec_m) - mean(log(haul$depth_hi_prec_m))) / sd(log(haul$depth_hi_prec_m))
haul$log_depth_scaled2 = haul$log_depth_scaled ^ 2

# get catch data
catch = nwfscSurvey::PullCatch.fn(Name = "sablefish",
                                  YearRange = c(2003,2018),
                                  SurveyName="NWFSC.Combo")
# format to join catch and haul
names(catch) = tolower(names(catch))
catch$trawl_id = as.numeric(catch$trawl_id)

# Join catch and haul data
haul_new = haul %>%
  left_join(catch, by = "trawl_id") %>%
  select(trawl_id, X, Y,
         latitude = latitude_dd.x,
         longitude = longitude_dd.x,
         year = year.x,
         log_depth_scaled,
         log_depth_scaled2,
         cpue_kg_km2)
# Set NA CPUEs to 0
haul_new$cpue_kg_km2[which(is.na(haul_new$cpue_kg_km2))] = 0

set.seed(2021)
n_folds = 5
n_blocks = 5
# first assign blocks
haul_new$block = 1
for(jj in 1:n_blocks) {
  haul_new$block[which(haul_new$latitude < quantile(haul_new$latitude,1 - jj*(1/n_blocks)))] = jj+1
}
# now assign folds
block_fold = data.frame("block"=1:n_blocks, "fold"=rep(1:n_folds,n_blocks/n_folds))
haul_new = dplyr::left_join(haul_new, block_fold)

saveRDS(haul_new,"data/sablefish_cleaned.rds")

################################################################################

#### Make Prediction Grids ####

# read in the grid cell data from the survey design
grid_cells = readxl::read_excel("data/Selection Set 2018 with Cell Corners.xlsx")
coordinates(grid_cells) <- c("Cent.Long", "Cent.Lat")
proj4string(grid_cells) <- CRS("+proj=longlat +datum=WGS84")
grid_cells <- spTransform(grid_cells, CRS(newproj))


### Make Fine Resolution (1x) Prediction Grid ###

# make prediction raster from grid_cell centroids, given standard dimensions of survey design grid (here in meters, converted from nm)
predict_raster = raster::raster(grid_cells, resolution = c(2778,3704), vals = NULL)
## load custom bathymetry raster
bathy_hiRes <- raster::raster("data/bathy_clipped")
bathy_hiRes <- bathy_hiRes / 10 # units were originally decimeters, so convert to meters
# aggregate and project bathymetry to survey grid cells, the absolute minimum resolution of the prediction grid
bathy_raster <- raster::projectRaster(bathy_hiRes, predict_raster, crs = newproj, method="bilinear")
# load Cowcod Conservation Areas, not included in trawl survey, and re-project
CCA = rgdal::readOGR('data/spatial_closure_boundaries/kv299cy7357.shp')
CCA = sp::spTransform(CCA, sp::CRS(newproj))
# mask CCA from bathymetry raster used for prediction
bathy_raster = suppressWarnings(raster::mask(bathy_raster, CCA, inverse = TRUE))
# create matrix of point data with coordinates and depth from raster
wc_grid <- as.data.frame(raster::rasterToPoints(bathy_raster))
colnames(wc_grid) = c("X", "Y", "depth")

# scale covariates
wc_grid$log_depth_scaled <- (log(wc_grid$depth * -1) - mean(log(haul$depth_hi_prec_m))) / sd(log(haul$depth_hi_prec_m))
wc_grid$log_depth_scaled2 <- wc_grid$log_depth_scaled ^ 2
wc_grid$X <- wc_grid$X/unit_scale
wc_grid$Y <- wc_grid$Y/unit_scale

saveRDS(wc_grid, file = "data/wc_grid_1x.rds")


### Make Coarse Resolution (4x) Prediction Grid ###

bathy_raster_4x = aggregate(bathy_raster, fact = 4, expand = FALSE)
wc_grid_4x <- as.data.frame(raster::rasterToPoints(bathy_raster_4x))
colnames(wc_grid_4x) = c("X", "Y", "depth")

# scale covariates
wc_grid_4x$log_depth_scaled <- (log(wc_grid_4x$depth * -1) - mean(log(haul$depth_hi_prec_m))) / sd(log(haul$depth_hi_prec_m))
wc_grid_4x$log_depth_scaled2 <- wc_grid_4x$log_depth_scaled ^ 2
wc_grid_4x$X <- wc_grid_4x$X/unit_scale
wc_grid_4x$Y <- wc_grid_4x$Y/unit_scale

saveRDS(wc_grid_4x, file = "data/wc_grid_4x.rds")
