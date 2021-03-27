## Comparative plots of biomass estimates and COG estimates between data levels 
# (i.e., 80% quantile, 90% quantile, 95% quantile, 100%/full/status quo) ##

# Biomass:

library(ggplot2)
library(viridis)

# data
biomass_estimate_1x_600knots_80quantile = readRDS(paste0("C:/Users/cjcco/Box/Best Practices paper/biomass_estimate_1x_600knots_80quantile.rds"))
biomass_estimate_1x_600knots_90quantile = readRDS(paste0("C:/Users/cjcco/Box/Best Practices paper/biomass_estimate_1x_600knots_90quantile.rds"))
biomass_estimate_1x_600knots_95quantile = readRDS(paste0("C:/Users/cjcco/Box/Best Practices paper/biomass_estimate_1x_600knots_95quantile.rds"))
biomass_estimate_1x_600knots_full = readRDS(paste0("C:/Users/cjcco/Box/Best Practices paper/biomass_estimate_1x.rds"))

# standardized (biomass estimate time series divided by it's own 2018 biomass estimate)
biomass_est_1x_80quantile_relative = biomass_estimate_1x_600knots_80quantile$est/biomass_estimate_1x_600knots_80quantile$est[16]
biomass_est_1x_90quantile_relative = biomass_estimate_1x_600knots_90quantile$est/biomass_estimate_1x_600knots_90quantile$est[16]
biomass_est_1x_95quantile_relative = biomass_estimate_1x_600knots_95quantile$est/biomass_estimate_1x_600knots_95quantile$est[16]
biomass_est_1x_full_relative = biomass_estimate_1x_600knots_full$est/biomass_estimate_1x_600knots_full$est[16]

year = 2003:2018
biomass_est_1x_80quantile_rel = data.frame("est" = biomass_est_1x_80quantile_relative, "year" = year)
biomass_est_1x_90quantile_rel = data.frame("est" = biomass_est_1x_90quantile_relative, "year" = year)
biomass_est_1x_95quantile_rel = data.frame("est" = biomass_est_1x_95quantile_relative, "year" = year)
biomass_est_1x_full_rel = data.frame("est" = biomass_est_1x_full_relative, "year" = year)

# standardized CIs
biomass_est_1x_80quantile_rel[, "lwr"] = biomass_estimate_1x_600knots_80quantile$lwr/biomass_estimate_1x_600knots_80quantile$est[16]
biomass_est_1x_80quantile_rel[, "upr"] = biomass_estimate_1x_600knots_80quantile$upr/biomass_estimate_1x_600knots_80quantile$est[16]
biomass_est_1x_90quantile_rel[, "lwr"] = biomass_estimate_1x_600knots_90quantile$lwr/biomass_estimate_1x_600knots_90quantile$est[16]
biomass_est_1x_90quantile_rel[, "upr"] = biomass_estimate_1x_600knots_90quantile$upr/biomass_estimate_1x_600knots_90quantile$est[16]
biomass_est_1x_95quantile_rel[, "lwr"] = biomass_estimate_1x_600knots_95quantile$lwr/biomass_estimate_1x_600knots_95quantile$est[16]
biomass_est_1x_95quantile_rel[, "upr"] = biomass_estimate_1x_600knots_95quantile$upr/biomass_estimate_1x_600knots_95quantile$est[16]
biomass_est_1x_full_rel[, "lwr"] = biomass_estimate_1x_600knots_full$lwr/biomass_estimate_1x_600knots_full$est[16]
biomass_est_1x_full_rel[, "upr"] = biomass_estimate_1x_600knots_full$upr/biomass_estimate_1x_600knots_full$est[16]

# add column for data level
biomass_est_1x_80quantile_rel[, "data_level"] = "80% Quantile"
biomass_est_1x_90quantile_rel[, "data_level"] = "90% Quantile"
biomass_est_1x_95quantile_rel[, "data_level"] = "95% Quantile"
biomass_est_1x_full_rel[, "data_level"] = "100%/Full"

# combine dataframes
biomass_est_1x_all = rbind(biomass_est_1x_80quantile_rel,biomass_est_1x_90quantile_rel,biomass_est_1x_95quantile_rel,biomass_est_1x_full_rel)

# relative biomass estimates time series by data level (note: added +0.1 and +0.2 to year to offset points/line/error bars so they don't overlap)
ggplot(biomass_est_1x_all, aes(x=year, y=est, color=factor(data_level))) + 
  geom_point(data = biomass_est_1x_80quantile_rel, aes(x=year,y=est), size=3) +
  geom_line(data=biomass_est_1x_80quantile_rel,aes(x=year,y=est)) +
  geom_errorbar(data=biomass_est_1x_80quantile_rel, aes(x=year,ymin=lwr, ymax=upr), width=.1) +
  geom_point(data = biomass_est_1x_90quantile_rel, aes(x=year+0.1,y=est), size=3) +
  geom_line(data=biomass_est_1x_90quantile_rel,aes(x=year+0.1,y=est)) +
  geom_errorbar(data=biomass_est_1x_90quantile_rel, aes(x=year+0.1,ymin=lwr, ymax=upr), width=.1) +
  geom_point(data = biomass_est_1x_95quantile_rel, aes(x=year+0.2,y=est), size=3) +
  geom_line(data=biomass_est_1x_95quantile_rel,aes(x=year+0.2,y=est)) +
  geom_errorbar(data=biomass_est_1x_95quantile_rel, aes(x=year+0.2,ymin=lwr, ymax=upr), width=.1) + 
  geom_point(data = biomass_est_1x_full_rel, aes(x=year+0.2,y=est), size=3) +
  geom_line(data=biomass_est_1x_full_rel,aes(x=year+0.2,y=est)) +
  geom_errorbar(data=biomass_est_1x_full_rel, aes(x=year+0.2,ymin=lwr, ymax=upr), width=.1) + 
  scale_x_continuous(breaks = seq(from = 2003, to = 2018, by = 1)) + 
  scale_color_viridis(discrete = TRUE, begin = 0, end = 0.8, breaks=c("80% Quantile","90% Quantile","95% Quantile","100%/Full")) +
  ggtitle("Relative (to each time series max. estimate) Biomass Estimates by Year w/ 95% CIs") +
  xlab("Year") +
  ylab("Relative Biomass Estimate") + theme_classic()

# biomass estimates time series by data level
ggplot(NULL, aes(x=year, y=est)) + 
  geom_point(data = biomass_estimate_1x_600knots_80quantile, aes(x=year,y=est, color = "cornflowerblue"), size=3) +
  geom_line(data=biomass_estimate_1x_600knots_80quantile,aes(x=year,y=est, color = "cornflowerblue")) +
  geom_errorbar(data=biomass_estimate_1x_600knots_80quantile, aes(x=year,ymin=lwr, ymax=upr), width=.1, colour="cornflowerblue") +
  geom_point(data = biomass_estimate_1x_600knots_90quantile, aes(x=year+0.1,y=est, color = "darkorchid"), size=3) +
  geom_line(data=biomass_estimate_1x_600knots_90quantile,aes(x=year+0.1,y=est, color = "darkorchid")) +
  geom_errorbar(data=biomass_estimate_1x_600knots_90quantile, aes(x=year+0.1,ymin=lwr, ymax=upr), width=.1, colour="darkorchid") +
  geom_point(data = biomass_estimate_1x_600knots_95quantile, aes(x=year+0.2,y=est, color = "green4"), size=3) +
  geom_line(data=biomass_estimate_1x_600knots_95quantile,aes(x=year+0.2,y=est, color = "green4")) +
  geom_errorbar(data=biomass_estimate_1x_600knots_95quantile, aes(x=year+0.2,ymin=lwr, ymax=upr), width=.1, colour="green4") +
  geom_point(data = biomass_estimate_1x_600knots_full, aes(x=year+0.3,y=est, color = "red"), size=3) +
  geom_line(data=biomass_estimate_1x_600knots_full,aes(x=year+0.3,y=est, color = "red")) +
  geom_errorbar(data=biomass_estimate_1x_600knots_full, aes(x=year+0.3,ymin=lwr, ymax=upr), width=.1, colour="red") +
  ggtitle("Biomass Estimates by Year w/ 95% CIs") + xlab("Year") + 
  ylab("Biomass Estimate") +
  scale_x_continuous(breaks = seq(from = 2003, to = 2018, by = 1)) +
  scale_color_identity(guide = "legend",  labels = c("80% Quantile", "90% Quantile", "95% Quantile", "Full/100%"), name = "Data Level")

# COG:

# data
COG_1x_600knots_80quantile = readRDS(paste0("C:/Users/cjcco/Box/Best Practices paper/COG_1x_600knots_80quantile_formatted.rds"))
COG_1x_600knots_90quantile = readRDS(paste0("C:/Users/cjcco/Box/Best Practices paper/COG_1x_600knots_90quantile_formatted.rds"))
COG_1x_600knots_95quantile = readRDS(paste0("C:/Users/cjcco/Box/Best Practices paper/COG_1x_600knots_95quantile_formatted.rds"))
COG_1x_600knots_full = readRDS(paste0("C:/Users/cjcco/Box/Best Practices paper/COG_1x.rds"))

# add column for data level
COG_1x_600knots_80quantile[, "data_level"] = "80% Quantile"
COG_1x_600knots_90quantile[, "data_level"] = "90% Quantile"
COG_1x_600knots_95quantile[, "data_level"] = "95% Quantile"
COG_1x_600knots_full[, "data_level"] = "100%/Full"

# combine dataframes
COG_1x_600knots_all = rbind(COG_1x_600knots_80quantile,COG_1x_600knots_90quantile,COG_1x_600knots_95quantile,COG_1x_600knots_full)

# plot COG Eastings by Year with lower/upper 95% CIs, comparative across data level
p1 = ggplot(COG_1x_600knots_all, aes(x=year, y=X, color=factor(data_level))) + 
  geom_point(data = COG_1x_600knots_80quantile, aes(x=year,y=X), size=3) +
  geom_line(data=COG_1x_600knots_80quantile, aes(x=year,y=X)) +
  geom_errorbar(data=COG_1x_600knots_80quantile, aes(x=year,ymin=X_lwr, ymax=X_upr), width=.1) +
  geom_point(data = COG_1x_600knots_90quantile, aes(x=year+0.1,y=X), size=3) +
  geom_line(data=COG_1x_600knots_90quantile, aes(x=year+0.1,y=X)) +
  geom_errorbar(data=COG_1x_600knots_90quantile, aes(x=year+0.1,ymin=X_lwr, ymax=X_upr), width=.1) +
  geom_point(data = COG_1x_600knots_95quantile, aes(x=year+0.2,y=X), size=3) +
  geom_line(data=COG_1x_600knots_95quantile, aes(x=year+0.2,y=X)) +
  geom_errorbar(data=COG_1x_600knots_95quantile, aes(x=year+0.2,ymin=X_lwr, ymax=X_upr), width=.1) +
  geom_point(data = COG_1x_600knots_full, aes(x=year+0.3,y=X), size=3) +
  geom_line(data=COG_1x_600knots_full, aes(x=year+0.3,y=X)) +
  geom_errorbar(data=COG_1x_600knots_full, aes(x=year+0.3,ymin=X_lwr, ymax=X_upr), width=.1) +
  ggtitle("COG Eastings Estimates by Year w/ 95% CIs") + xlab("Year") + 
  ylab("COG Eastings (km)") +
  scale_x_continuous(breaks = seq(2003, 2018, by = 1)) +
  scale_y_continuous(breaks = seq(395, 600, by = 25)) + 
  scale_color_viridis(discrete = TRUE, begin = 0, end = 0.8, breaks=c("80% Quantile","90% Quantile","95% Quantile","100%/Full")) + 
  theme_classic()

# plot COG Northings by Year with lower/upper 95% CIs, comparative across data level
p2 = ggplot(COG_1x_600knots_all, aes(x=year, y=Y, color=factor(data_level))) + 
  geom_point(data = COG_1x_600knots_80quantile, aes(x=year,y=Y), size=3) +
  geom_line(data=COG_1x_600knots_80quantile, aes(x=year,y=Y)) +
  geom_errorbar(data=COG_1x_600knots_80quantile, aes(x=year,ymin=Y_lwr, ymax=Y_upr), width=.1) +
  geom_point(data = COG_1x_600knots_90quantile, aes(x=year+0.1,y=Y), size=3) +
  geom_line(data=COG_1x_600knots_90quantile, aes(x=year+0.1,y=Y)) +
  geom_errorbar(data=COG_1x_600knots_90quantile, aes(x=year+0.1,ymin=Y_lwr, ymax=Y_upr), width=.1) +
  geom_point(data = COG_1x_600knots_95quantile, aes(x=year+0.2,y=Y), size=3) +
  geom_line(data=COG_1x_600knots_95quantile, aes(x=year+0.2,y=Y)) +
  geom_errorbar(data=COG_1x_600knots_95quantile, aes(x=year+0.2,ymin=Y_lwr, ymax=Y_upr), width=.1) +
  geom_point(data = COG_1x_600knots_full, aes(x=year+0.3,y=Y), size=3) +
  geom_line(data=COG_1x_600knots_full, aes(x=year+0.3,y=Y)) +
  geom_errorbar(data=COG_1x_600knots_full, aes(x=year+0.3,ymin=Y_lwr, ymax=Y_upr), width=.1) +
  ggtitle("COG Northings Estimates by Year w/ 95% CIs") + xlab("Year") + 
  ylab("COG Northings (km)") +
  scale_x_continuous(breaks = seq(2003, 2018, by = 1)) +
  scale_y_continuous(breaks = seq(4215, 4900, by = 50)) + 
  scale_color_viridis(discrete = TRUE, begin = 0, end = 0.8, breaks=c("80% Quantile","90% Quantile","95% Quantile","100%/Full")) + 
  theme_classic()

grid.arrange(p2, p1, nrow = 2)
