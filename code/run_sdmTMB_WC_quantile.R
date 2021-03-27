# Prior to estimation, truncate data by some quantile of density
# Motivation: what happens when we get rid of the tails of the distribution?

# Fitting spatiotemporal models to West Coast Groundfish Bottom Trawl data with sdmTMB

#devtools::install_github("pbs-assess/sdmTMB")
#library(INLA) # if we want tools to make other meshes
#library(ggplot2) # only needed for cross-validation plots
library(dplyr)
library(sdmTMB)
library(sp)
library(tictoc) # for calculating runtime

###########################################################################################################
# options 

# specify # of knots for mesh
n_knots = 600

# do cross validation?
use_cv = FALSE

###########################################################################################################
# Prepare data and fit models

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

catch$common_name = NA
#catch$common_name[which(catch$scientific_name=="Ophiodon elongatus")] = "lingcod"
#catch$common_name[which(catch$scientific_name=="Microstomus pacificus")] = "Dover sole"
#catch$common_name[which(catch$scientific_name=="Sebastolobus alascanus")] = "shortspine thornyhead"
#catch$common_name[which(catch$scientific_name=="Atheresthes stomias")] = "arrowtooth flounder"
#catch$common_name[which(catch$scientific_name=="Hippoglossus stenolepis")] = "Pacific halibut" # few obs, model fit issues
#catch$common_name[which(catch$scientific_name=="Glyptocephalus zachirus")] = "rex sole"
#catch$common_name[which(catch$scientific_name=="Parophrys vetulus")] = "English sole"
catch$common_name[which(catch$scientific_name=="Anoplopoma fimbria")] = "sablefish"
#catch$common_name[which(catch$scientific_name=="Sebastes alutus")] = "Pacific ocean perch"
#catch$common_name[which(catch$scientific_name=="Gadus macrocephalus")] = "Pacific cod" # few obs
#catch$common_name[which(catch$scientific_name=="Raja rhina")] = "longnose skate"
#catch$common_name[which(catch$scientific_name=="Raja binoculata")] = "big skate"
#catch$common_name[which(catch$scientific_name=="Squalus suckleyi")] = "spiny dogfish"
catch = dplyr::filter(catch, !is.na(common_name))

# Loop over species
species = unique(catch$common_name)
  
  subset = dplyr::filter(catch, common_name == species)
  haul_new = haul %>% 
    left_join(subset, by = "trawl_id") %>% 
    select(trawl_id, X, Y, latitude = latitude_dd.x, longitude = longitude_dd.x, year = year.x, log_depth_scaled, log_depth_scaled2, cpue_kg_km2)
  # Set NA CPUEs to 0
  haul_new$cpue_kg_km2[which(is.na(haul_new$cpue_kg_km2))] = 0


  # truncate data by some quantile of density [80%, 90%, and 95% should be good]
  #haul_new = dplyr::filter(haul_new, latitude <= quantile(latitude, 0.975),
                           #latitude >= quantile(latitude,0.025)) # 95% quantile
  
  #haul_new = dplyr::filter(haul_new, latitude <= quantile(latitude, 0.95),
                           #latitude >= quantile(latitude,0.05)) # 90% quantile
  
  haul_new = dplyr::filter(haul_new, latitude <= quantile(latitude, 0.90),
                           latitude >= quantile(latitude,0.1)) # 80% quantile


  # using AUC and Tweedie predictive density to evaluate performance
  # you can iterate fits over a range of number of knots by giving set of values rather than n_knots
  performance <- data.frame(
    knots = n_knots
    #knots = seq(650,1000,50)
  )
  
  for (k in 1:length(performance$knots)) {
    
    # Create species-specific directories to save output
    if(!dir.exists(paste0("C:/Users/cjcco/OneDrive/Desktop/1st Paper/output/WC3/",species))) dir.create(paste0("C:/Users/cjcco/OneDrive/Desktop/1st Paper/output/WC3/",species))
    
    if(use_cv==TRUE) {
      # create ids based on latitude quantiles
      haul_new$fold = 5
      haul_new$fold[which(haul_new$latitude < quantile(haul_new$latitude,0.8))] = 4
      haul_new$fold[which(haul_new$latitude < quantile(haul_new$latitude,0.6))] = 3
      haul_new$fold[which(haul_new$latitude < quantile(haul_new$latitude,0.4))] = 2
      haul_new$fold[which(haul_new$latitude < quantile(haul_new$latitude,0.2))] = 1
      
      tic() # start calculation of runtime
      
      density_model <- sdmTMB_cv(formula = cpue_kg_km2 ~ 0 + as.factor(year),
                                 time_varying = ~ 0 + log_depth_scaled + log_depth_scaled2,
                                 data = haul_new, 
                                 x = "X", y = "Y", 
                                 k_folds=max(haul_new$fold), 
                                 fold_ids = "fold",
                                 n_knots = performance$knots[k],
                                 reml = TRUE, 
                                 time = "year", 
                                 anisotropy = TRUE,
                                 family = tweedie(link = "log"),
                                 control = sdmTMBcontrol(step.min = 0.01, step.max = 1)
      )
      
      #density_model2 <- sdmTMB_cv(formula = cpue_kg_km2 ~ 0 + as.factor(year),
      #data = haul_new, 
      #x = "X", y = "Y", 
      #k_folds=max(haul_new$fold), 
      #fold_ids = "fold",
      #n_knots = performance$knots[k],
      #reml = TRUE, 
      #time = "year", 
      #anisotropy = TRUE,
      #family = tweedie(link = "log"),
      #control = sdmTMBcontrol(step.min = 0.01, step.max = 1)
      #)
      
      saveRDS(density_model, file=paste0("C:/Users/cjcco/OneDrive/Desktop/1st Paper/output/WC3/",species,"/",species,"_",performance$knots[k],"cv_density_depth_varying_80quantile_take2.rds"))
      #saveRDS(density_model2, file=paste0("C:/Users/cjcco/OneDrive/Desktop/1st Paper/output/WC/",species[spp],"/",species[spp],"_",performance$knots[k],"cv_density_no_covar.rds"))
      
      toc(log = TRUE, quiet = TRUE) # end calculation of runtime
      log.txt = tic.log(format = TRUE) # log seconds elapsed
      runtime = gsub(" sec elapsed", "", unlist(log.txt)) # extract value for data frame and plot
      tic.clearlog() # clear the log (to prevent duplicates)
      
      # validate against the test set (note that currently you need to manually specify which models you want the performance metrics from)
      performance[k, "tweedie_dens_m1"] = sum(density_model$sum_loglik)
      performance[k, "runtime"] = runtime # add runtime to performance data frame
      #performance[k, "tweedie_dens_m2"] = sum(density_model2$sum_loglik)
      saveRDS(performance, file = paste0("C:/Users/cjcco/OneDrive/Desktop/1st Paper/output/WC3/",species,"_performance_600_80quantile_take2.rds"))
      
    } else {
      c_spde <- make_spde(haul_new$X, haul_new$Y, n_knots = performance$knots[k])
      density_model <- sdmTMB(formula = cpue_kg_km2 ~ 0 + as.factor(year),
                              time_varying = ~ 0 + log_depth_scaled + log_depth_scaled2,
                              data = haul_new,
                              time = "year", 
                              spde = c_spde, 
                              reml = TRUE, 
                              anisotropy = TRUE,
                              family = tweedie(link = "log"),
                              control = sdmTMBcontrol(step.min = 0.01, step.max = 1)
      )

      saveRDS(density_model, file=paste0("C:/Users/cjcco/OneDrive/Desktop/1st Paper/output/WC3/",species,"/",species,"_",performance$knots[k],"_density_depth_varying600knots_80quantile_take2.rds"))

    }
  }
  
  # plot Tweedie predictive density as a function of knots. Higher is better.
  #b <- ggplot(performance, aes(knots, tweedie_dens)) + geom_point() +
  #  geom_line() + ylab("Tweedie predictive density") + xlab("Knots")
  #performance_plot <- ggpubr::ggarrange(a, b, ncol = 1, nrow = 2)
  #ggpubr::ggexport(performance_plot, filename = paste0("output/WC/",species[spp],"_knots.pdf"))

##########################################################################################################
# make prediction raster roughly from grid_cell centroids, given standard cell dimensions (here in meters, converted from nm)
# TO DO: standardize prediction grid resolution with other regions?  Also move this chunk to plotting script or own script?

# read in the grid cell data from the survey design
grid_cells = readxl::read_excel("C:/Users/cjcco/OneDrive/Desktop/1st Paper/Selection Set 2018 with Cell Corners.xlsx")
coordinates(grid_cells) <- c("Cent.Long", "Cent.Lat")
proj4string(grid_cells) <- CRS("+proj=longlat +datum=WGS84")
grid_cells <- spTransform(grid_cells, CRS(newproj))

# make prediction raster roughly from grid_cell centroids, given standard cell dimensions (here in meters, converted from nm)
predict_raster = raster::raster(grid_cells, resolution = c(2778,3704), vals = NULL)
## load custom bathymetry raster
bathy_hiRes <- raster::raster("C:/Users/cjcco/OneDrive/Desktop/1st Paper/bathy_clipped")
bathy_hiRes <- bathy_hiRes / 10 # units were originally decimeters, so convert to meters
# aggregate and project bathymetry to survey grid cells, the absolute minimum resolution of the prediction grid
bathy_raster <- raster::projectRaster(bathy_hiRes, predict_raster, crs = newproj, method="bilinear")
# load Cowcod Conservation Areas, not included in trawl survey, and reproject
CCA = rgdal::readOGR('C:/Users/cjcco/OneDrive/Desktop/1st Paper/spatial_closure_boundaries/kv299cy7357.shp')
CCA = sp::spTransform(CCA, sp::CRS(newproj))
# mask CCA from bathymetry raster used for prediction
bathy_raster = suppressWarnings(raster::mask(bathy_raster, CCA, inverse = TRUE))
# create matrix of point data with coordinates and depth from raster
wc_grid <- as.data.frame(raster::rasterToPoints(bathy_raster)) # rough area of survey extent is 123497km^2, from 2.778*3.704 (cell res) * nrow(wc_grid) = 12002 
colnames(wc_grid) = c("X", "Y", "depth")

# scale covariates
wc_grid$log_depth_scaled <- (log(wc_grid$depth * -1) - mean(log(haul$depth_hi_prec_m))) / sd(log(haul$depth_hi_prec_m))
wc_grid$log_depth_scaled2 <- wc_grid$log_depth_scaled ^ 2
wc_grid$X <- wc_grid$X/unit_scale
wc_grid$Y <- wc_grid$Y/unit_scale

saveRDS(wc_grid, file=paste0("C:/Users/cjcco/OneDrive/Desktop/1st Paper/wc_grid.rds")) # save prediction grid
