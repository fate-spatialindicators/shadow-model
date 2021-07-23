## generalized paths; for running up to 1000 knots

#devtools::install_github("pbs-assess/sdmTMB")
library(dplyr)
library(sdmTMB)
library(sp)
library(tictoc) # for calculating runtime

###########################################################################################################
# options 

# specify # of knots for mesh
#n_knots = 600

# do cross validation?
use_cv = TRUE

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
catch = dplyr::filter(catch, 
        common_name == "sablefish", !is.na(common_name))

# Loop over species
species = unique(catch$common_name)

for(spp in 1:length(species)) {
  
  subset = dplyr::filter(catch, common_name == species[spp])
  haul_new = haul %>% 
    left_join(subset, by = "trawl_id") %>% 
    select(trawl_id, X, Y, latitude = latitude_dd.x, longitude = longitude_dd.x, year = year.x, log_depth_scaled, log_depth_scaled2, cpue_kg_km2)
  # Set NA CPUEs to 0
  haul_new$cpue_kg_km2[which(is.na(haul_new$cpue_kg_km2))] = 0
  
  # filter by species and only include range of coordinates with positive observations over the timeseries
  #haul_new = dplyr::filter(haul_new, latitude_dd >= min(latitude_dd[which(cpue_kg_km2>0)]),
  #                         latitude_dd <= max(latitude_dd[which(cpue_kg_km2>0)]),
  #                         longitude_dd >= min(longitude_dd[which(cpue_kg_km2>0)]),
  #                         longitude_dd <= max(longitude_dd[which(cpue_kg_km2>0)]))
  
  # using AUC and Tweedie predictive density to evaluate performance
  # you can iterate fits over a range of number of knots by giving set of values rather than n_knots
  performance <- data.frame(
    #knots = n_knots
    knots = seq(50,1000,50)
  )
  
  for (k in 1:length(performance$knots)) {
    
    # Create species-specific directories to save output
    #if(!dir.exists(paste0("./output/WC/",species[spp]))) dir.create(paste0("./output/WC/",species[spp]))
    
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
      
      saveRDS(density_model, file=paste0(species[spp],"_",performance$knots[k],"cv_density_depth_varying.rds"))
      #saveRDS(density_model2, file=paste0("C:/Users/cjcco/OneDrive/Desktop/1st Paper/output/WC/",species[spp],"/",species[spp],"_",performance$knots[k],"cv_density_no_covar.rds"))
      
      toc(log = TRUE, quiet = TRUE) # end calculation of runtime
      log.txt = tic.log(format = TRUE) # log seconds elapsed
      runtime = gsub(" sec elapsed", "", unlist(log.txt)) # extract value for data frame and plot
      tic.clearlog() # clear the log (to prevent duplicates)
      
      # validate against the test set (note that currently you need to manually specify which models you want the performance metrics from)
      performance[k, "tweedie_dens_m1"] = sum(density_model$sum_loglik)
      performance[k, "runtime"] = runtime # add runtime to performance data frame
      #performance[k, "tweedie_dens_m2"] = sum(density_model2$sum_loglik)
      saveRDS(performance, file = paste0(species[spp],"_performance_650to1000knots.rds"))
      
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

      saveRDS(density_model, file=paste0(species[spp],"_",performance$knots[k],"_density_depth_varying.rds"))
      #saveRDS(density_model2, file=paste0("C:/Users/cjcco/OneDrive/Desktop/1st Paper/output/WC/",species[spp],"/",species[spp],"_",performance$knots[k],"_density_no_covar.rds"))
      
    }
  }
}
