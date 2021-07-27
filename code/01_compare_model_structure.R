# Fitting spatiotemporal models to West Coast Groundfish Bottom Trawl data with sdmTMB

#devtools::install_github("pbs-assess/sdmTMB")
#library(INLA) # if we want tools to make other meshes
#library(ggplot2) # only needed for cross-validation plots
library(dplyr)
library(sdmTMB)
library(sp)
library(tictoc) # for calculating runtime
library(tweedie)

# Prepare data
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

# catch data includes catch, effort, etc. This takes a few minutes to grab all ~ 900 spp
catch = nwfscSurvey::PullCatch.fn(YearRange = c(2003,2018), SurveyName="NWFSC.Combo")
# format to later join catch and haul
names(catch) = tolower(names(catch))
catch$trawl_id = as.numeric(catch$trawl_id)

catch = dplyr::filter(catch, common_name=="sablefish")

# Join catch and haul data
haul_new = haul %>% 
  left_join(catch, by = "trawl_id") %>% 
  select(trawl_id, X, Y, latitude = latitude_dd.x, longitude = longitude_dd.x, year = year.x, log_depth_scaled, log_depth_scaled2, cpue_kg_km2)
# Set NA CPUEs to 0
haul_new$cpue_kg_km2[which(is.na(haul_new$cpue_kg_km2))] = 0

set.seed(2021)
n_folds = 5
n_blocks = 20
# first assign blocks
haul_new$block = 1
for(jj in 1:n_blocks) {
  haul_new$block[which(haul_new$latitude < quantile(haul_new$latitude,1 - jj*(1/n_blocks)))] = jj+1
}
# now assign folds
block_fold = data.frame("block"=1:n_blocks, "fold"=rep(1:n_folds,n_blocks/n_folds))
haul_new = dplyr::left_join(haul_new, block_fold)

# Fit models to the sablefish data. 
# 1. spatial only, 2-4 = models with spatiotemporal effects

library(future)
plan(multisession, workers = floor(availableCores() / 2))

spde = make_mesh(
  haul_new,
  xy_cols = c("X","Y"),
  type = "cutoff",
  cutoff = 15,
  #cutoff=70,
  seed = 2021
)

model_1 <- sdmTMB_cv(cpue_kg_km2 ~ 0 + as.factor(year),
                time_varying = ~ 0 + log_depth_scaled + log_depth_scaled2,
                spde = spde,
                k_folds = max(haul_new$fold),
                fold_ids = haul_new$fold,
                data = haul_new,
                time = "year", 
                priors = sdmTMBpriors(matern_s = pc_matern(range_gt = 5, sigma_lt = 5),
                                      matern_st = pc_matern(range_gt = 5, sigma_lt = 5)),
                anisotropy = TRUE,
                spatial_only = TRUE,
                family = tweedie(link = "log"))
# IID
model_2 <- sdmTMB_cv(cpue_kg_km2 ~ 0 + as.factor(year),
                     time_varying = ~ 0 + log_depth_scaled + log_depth_scaled2,
                     spde = spde,
                     k_folds = max(haul_new$fold),
                     fold_ids = haul_new$fold,
                     data = haul_new,
                     time = "year",
                     priors = sdmTMBpriors(matern_s = pc_matern(range_gt = 5, sigma_lt = 5),
                                           matern_st = pc_matern(range_gt = 5, sigma_lt = 5)),
                     fields = "IID",
                     anisotropy = TRUE,
                     spatial_only = FALSE,
                     family = tweedie(link = "log"))
# RW
model_3 <- sdmTMB_cv(cpue_kg_km2 ~ 0 + as.factor(year),
                     time_varying = ~ 0 + log_depth_scaled + log_depth_scaled2,
                     spde = spde,
                     k_folds = max(haul_new$fold),
                     fold_ids = haul_new$fold,
                     data = haul_new,
                     time = "year",
                     priors = sdmTMBpriors(matern_s = pc_matern(range_gt = 5, sigma_lt = 5),
                                           matern_st = pc_matern(range_gt = 5, sigma_lt = 5)),
                     fields = "RW",
                     anisotropy = TRUE,
                     spatial_only = FALSE,
                     family = tweedie(link = "log"))
# AR(1)
model_4 <- sdmTMB_cv(cpue_kg_km2 ~ 0 + as.factor(year),
                     time_varying = ~ 0 + log_depth_scaled + log_depth_scaled2,
                     spde = spde,
                     k_folds = max(haul_new$fold),
                     fold_ids = haul_new$fold,
                     data = haul_new,
                     time = "year", 
                     priors = sdmTMBpriors(matern_s = pc_matern(range_gt = 5, sigma_lt = 5),
                                           matern_st = pc_matern(range_gt = 5, sigma_lt = 5)),
                     fields = "AR1",
                     anisotropy = TRUE,
                     spatial_only = FALSE,
                     family = tweedie(link = "log"))


