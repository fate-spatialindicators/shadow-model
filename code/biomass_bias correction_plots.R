## Relative Biomass Estimates - w/ bias correction ##

# comparing biomass estimates between fine and coarse prediction grids, 
# where both use bias correction and another panel where both do not use bias correction [similar to figure 7]

# data description:
# Biomass estimates, sablefish, 100 knots, 1x/fine, w/ bias correction
# Biomass estimates, sablefish, 100 knots, 4x/coarse, w/ bias correction

# Panel 1:
biomass_1x = readRDS("Biomass_Estimates_Sablefish_100knots_1x_Bias_Correction")
biomass_4x = readRDS("Biomass_Estimates_Sablefish_100knots_4x_Bias_Correction")

# standardized (biomass estimate time series divided by max./2018 biomass estimate)
biomass_est_1x_relative = biomass_1x$est/max(biomass_1x$est)
biomass_est_4x_relative = biomass_4x$est/max(biomass_4x$est)
year = 2003:2018
biomass_1x_rel = data.frame("est" = biomass_est_1x_relative, "year" = year)
biomass_4x_rel = data.frame("est" = biomass_est_4x_relative, "year" = year)

# standardized CIs
biomass_1x_rel[, "lwr"] = biomass_1x$lwr/max(biomass_1x$est)
biomass_1x_rel[, "upr"] = biomass_1x$upr/max(biomass_1x$est)
biomass_4x_rel[, "lwr"] = biomass_4x$lwr/max(biomass_4x$est)
biomass_4x_rel[, "upr"] = biomass_4x$upr/max(biomass_4x$est)

# add column for grid resolution
biomass_1x_rel[, "Grid_Resolution"] = "Fine (1x)"
biomass_4x_rel[, "Grid_Resolution"] = "Coarse (4x)"

# combine dataframes
biomass_est_all = rbind(biomass_1x_rel,biomass_4x_rel)

ggplot(biomass_est_all, aes(x=year, y=est, color=factor(Grid_Resolution))) + 
  geom_point(data = biomass_1x_rel, aes(x=year,y=est), size=3) +
  geom_line(data=biomass_1x_rel,aes(x=year,y=est)) +
  geom_errorbar(data=biomass_1x_rel, aes(x=year,ymin=lwr, ymax=upr), width=.1) +
  geom_point(data = biomass_4x_rel, aes(x=year+0.1,y=est), size=3) +
  geom_line(data=biomass_4x_rel,aes(x=year+0.1,y=est)) +
  geom_errorbar(data=biomass_4x_rel, aes(x=year+0.1,ymin=lwr, ymax=upr), width=.1) + 
  scale_x_continuous(breaks = seq(from = 2003, to = 2018, by = 1)) + 
  scale_color_viridis(discrete = TRUE, begin = 0, end = 0.8, breaks=c("Fine (1x)","Coarse (4x)")) +
  ggtitle("Relative (to 2018 est.) Biomass Estimates (w/ bias correction) by Year w/ 95% CIs") +
  xlab("Year") +
  ylab("Relative Biomass Estimate") + theme_classic()


# Panel 2:

# data description:
# Biomass estimates, sablefish, 100 knots, 1x/fine, NO bias correction
# Biomass estimates, sablefish, 100 knots, 4x/coarse, NO bias correction

biomass_1x = readRDS("Biomass_Estimates_Sablefish_100knots_1x_NObias_Correction")
biomass_4x = readRDS("Biomass_Estimates_Sablefish_100knots_4x_NObias_Correction")

# standardized (biomass estimate time series divided by max./2018 biomass estimate)
biomass_est_1x_relative = biomass_1x$est/max(biomass_1x$est)
biomass_est_4x_relative = biomass_4x$est/max(biomass_4x$est)
year = 2003:2018
biomass_1x_rel = data.frame("est" = biomass_est_1x_relative, "year" = year)
biomass_4x_rel = data.frame("est" = biomass_est_4x_relative, "year" = year)

# standardized CIs
biomass_1x_rel[, "lwr"] = biomass_1x$lwr/max(biomass_1x$est)
biomass_1x_rel[, "upr"] = biomass_1x$upr/max(biomass_1x$est)
biomass_4x_rel[, "lwr"] = biomass_4x$lwr/max(biomass_4x$est)
biomass_4x_rel[, "upr"] = biomass_4x$upr/max(biomass_4x$est)

# add column for grid resolution
biomass_1x_rel[, "Grid_Resolution"] = "Fine (1x)"
biomass_4x_rel[, "Grid_Resolution"] = "Coarse (4x)"

# combine dataframes
biomass_est_all = rbind(biomass_1x_rel,biomass_4x_rel)

ggplot(biomass_est_all, aes(x=year, y=est, color=factor(Grid_Resolution))) + 
  geom_point(data = biomass_1x_rel, aes(x=year,y=est), size=3) +
  geom_line(data=biomass_1x_rel,aes(x=year,y=est)) +
  geom_errorbar(data=biomass_1x_rel, aes(x=year,ymin=lwr, ymax=upr), width=.1) +
  geom_point(data = biomass_4x_rel, aes(x=year+0.1,y=est), size=3) +
  geom_line(data=biomass_4x_rel,aes(x=year+0.1,y=est)) +
  geom_errorbar(data=biomass_4x_rel, aes(x=year+0.1,ymin=lwr, ymax=upr), width=.1) + 
  scale_x_continuous(breaks = seq(from = 2003, to = 2018, by = 1)) + 
  scale_color_viridis(discrete = TRUE, begin = 0, end = 0.8, breaks=c("Fine (1x)","Coarse (4x)")) +
  ggtitle("Relative (to 2018 est.) Biomass Estimates (NO bias correction) by Year w/ 95% CIs") +
  xlab("Year") +
  ylab("Relative Biomass Estimate") + theme_classic()
