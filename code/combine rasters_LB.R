## Combining prediction rasters into a differenced raster (e.g., 4x-1x)

library(dplyr)
library(sp)
library(rgdal)
library(raster)

# haul data includes environmental covariates with location information
haul = nwfscSurvey::PullHaul.fn(YearRange = c(2003,2018), SurveyName = "NWFSC.Combo")

# project lat/lon to UTM, after removing missing values and unsatisfactory hauls
haul = haul %>% filter(!is.na(longitude_dd), !is.na(latitude_dd), performance == "Satisfactory")
haul_trans = haul
coordinates(haul_trans) <- c("longitude_dd", "latitude_dd")
proj4string(haul_trans) <- CRS("+proj=longlat +datum=WGS84")
newproj = paste("+proj=utm +zone=10 ellps=WGS84")
#newproj = paste("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
unit_scale <- 1000
haul_trans <- spTransform(haul_trans, CRS(newproj))
haul_trans = as.data.frame(haul_trans)
haul_trans$longitude_dd = haul_trans$longitude_dd/unit_scale
haul_trans$latitude_dd = haul_trans$latitude_dd/unit_scale
haul_trans$year = as.numeric(substr(haul_trans$date_yyyymmdd,1,4))
haul$X = haul_trans$longitude_dd
haul$Y = haul_trans$latitude_dd
haul$year = haul_trans$year
#haul$year_centered = haul$year - mean(unique(haul$year))

# center and scale depth, removing NAs
haul = dplyr::filter(haul, !is.na(depth_hi_prec_m))
haul$log_depth_scaled = scale(log(haul$depth_hi_prec_m))
haul$log_depth_scaled2 = haul$log_depth_scaled ^ 2

# make prediction raster roughly from grid_cell centroids, given standard cell dimensions (here in meters, converted from nm)
newproj = paste("+proj=utm +zone=10 ellps=WGS84")
# read in the grid cell data from the survey design
grid_cells = readxl::read_excel("data/WC/WC_BTS/shapefiles/survey_grid/Selection Set 2018 with Cell Corners.xlsx")
coordinates(grid_cells) <- c("Cent.Long", "Cent.Lat")
proj4string(grid_cells) <- CRS("+proj=longlat +datum=WGS84")
grid_cells <- spTransform(grid_cells, CRS(newproj))

# make prediction raster roughly from grid_cell centroids, given standard cell dimensions (here in meters, converted from nm)
predict_raster_1x = raster::raster(grid_cells, resolution = c(2778,3704), vals = NULL)
## load custom bathymetry raster
bathy_hiRes <- raster::raster("data/WC/WC_BTS/shapefiles/bathy_clipped")
bathy_hiRes <- bathy_hiRes / 10 # units were originally decimeters, so convert to meters
# aggregate and project bathymetry to survey grid cells, the absolute minimum resolution of the prediction grid
bathy_raster_1x <- raster::projectRaster(bathy_hiRes, predict_raster_1x, crs = newproj, method="bilinear")
# load Cowcod Conservation Areas, not included in trawl survey, and reproject
CCA = rgdal::readOGR('data/WC/WC_BTS/shapefiles/spatial_closure_boundaries/kv299cy7357.shp')
CCA = sp::spTransform(CCA, sp::CRS(newproj))
# mask CCA from bathymetry raster used for prediction
bathy_raster_1x = suppressWarnings(raster::mask(bathy_raster_1x, CCA, inverse = TRUE))

# aggregate
bathy_raster_4x = aggregate(bathy_raster_1x, fact = 4, expand = FALSE)
# disaggregate to original resolution, with no method specified cells just take the value of the larger original cells
bathy_raster_4x_1 = disaggregate(bathy_raster_4x, fact = 4)
# crop to equalize extents
bathy_raster_1x <- crop(bathy_raster_1x, bathy_raster_4x_1)
all.equal(extent(bathy_raster_1x), extent(bathy_raster_4x_1))

## Combine rasters with difference as function:
bathy_raster_diff_1x4x = overlay(bathy_raster_1x, bathy_raster_4x_1, fun = function(x,y){(x-y)})

#bathy_raster_diff_1x4x = sp::over(bathy_raster_1x, bathy_raster_4x, fn = function(x,y){(x-y)}) # alternative to overlay, doesn't work

# create matrix of point data with coordinates and depth from raster
wc_grid_diff_1x4x <- as.data.frame(raster::rasterToPoints(bathy_raster_diff_1x4x))
colnames(wc_grid_diff_1x4x) = c("X", "Y", "depth")

# scale covariates
wc_grid_diff_1x4x$log_depth_scaled <- (log(wc_grid_diff_1x4x$depth * -1) - mean(log(haul$depth_hi_prec_m))) / sd(log(haul$depth_hi_prec_m))
wc_grid_diff_1x4x$log_depth_scaled2 <- wc_grid_diff_1x4x$log_depth_scaled ^ 2
wc_grid_diff_1x4x$X <- wc_grid_diff_1x4x$X/unit_scale
wc_grid_diff_1x4x$Y <- wc_grid_diff_1x4x$Y/unit_scale

saveRDS(wc_grid_diff_1x4x, file=paste0("C:/Users/cjcco/Box/1st Paper/wc_grid_diff_1x4x.rds")) # save differenced (4x-1x) prediction grid
