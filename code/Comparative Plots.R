## Comparative plots of biomass estimates and COG estimates between knots (50, 300, and 600) ##

library(ggplot2)
#install.packages("viridis")
library(viridis)

# Biomass:

# data
biomass_1x_50knots = readRDS(paste0("C:/Users/cjcco/Box/Best Practices paper/biomass_estimates_1x_50knots.rds"))
biomass_1x_300knots = readRDS(paste0("C:/Users/cjcco/Box/Best Practices paper/biomass_estimates_1x_300knots.rds"))
biomass_1x_600knots = readRDS(paste0("C:/Users/cjcco/Box/Best Practices paper/biomass_estimate_1x.rds"))

# standardized (biomass estimate time series divided by its own max. (2018) biomass estimate)
biomass_est_1x_50knots_relative = biomass_1x_50knots$est/max(biomass_1x_50knots$est)
biomass_est_1x_300knots_relative = biomass_1x_300knots$est/max(biomass_1x_300knots$est)
biomass_est_1x_600knots_relative = biomass_1x_600knots$est/max(biomass_1x_600knots$est)
year = 2003:2018
biomass_1x_50knots_rel = data.frame("est" = biomass_est_1x_50knots_relative, "year" = year)
biomass_1x_300knots_rel = data.frame("est" = biomass_est_1x_300knots_relative, "year" = year)
biomass_1x_600knots_rel = data.frame("est" = biomass_est_1x_600knots_relative, "year" = year)

# standardized CIs
biomass_1x_50knots_rel[, "lwr"] = biomass_1x_50knots$lwr/max(biomass_1x_50knots$est)
biomass_1x_50knots_rel[, "upr"] = biomass_1x_50knots$upr/max(biomass_1x_50knots$est)
biomass_1x_300knots_rel[, "lwr"] = biomass_1x_300knots$lwr/max(biomass_1x_300knots$est)
biomass_1x_300knots_rel[, "upr"] = biomass_1x_300knots$upr/max(biomass_1x_300knots$est)
biomass_1x_600knots_rel[, "lwr"] = biomass_1x_600knots$lwr/max(biomass_1x_600knots$est)
biomass_1x_600knots_rel[, "upr"] = biomass_1x_600knots$upr/max(biomass_1x_600knots$est)

# add column for number of knots
biomass_1x_50knots_rel[, "knots"] = 50
biomass_1x_300knots_rel[, "knots"] = 300
biomass_1x_600knots_rel[, "knots"] = 600

# combine dataframes
biomass_allknots = rbind(biomass_1x_50knots_rel, biomass_1x_300knots_rel, biomass_1x_600knots_rel)

# relative biomass estimates time series by number of knots (note: added +0.1 and +0.2 to year to offset points/line/error bars so they don't overlap)
ggplot(biomass_allknots, aes(x=year, y=est, color=factor(knots))) + 
  geom_point(data = biomass_1x_50knots_rel, aes(x=year,y=est), size=3) +
  geom_line(data=biomass_1x_50knots_rel,aes(x=year,y=est)) +
  geom_errorbar(data=biomass_1x_50knots_rel, aes(x=year,ymin=lwr, ymax=upr), width=.1) +
  geom_point(data = biomass_1x_300knots_rel, aes(x=year+0.1,y=est), size=3) +
  geom_line(data=biomass_1x_300knots_rel,aes(x=year+0.1,y=est)) +
  geom_errorbar(data=biomass_1x_300knots_rel, aes(x=year+0.1,ymin=lwr, ymax=upr), width=.1) +
  geom_point(data = biomass_1x_600knots_rel, aes(x=year+0.2,y=est), size=3) +
  geom_line(data=biomass_1x_600knots_rel,aes(x=year+0.2,y=est)) +
  geom_errorbar(data=biomass_1x_600knots_rel, aes(x=year+0.2,ymin=lwr, ymax=upr), width=.1) + 
  scale_x_continuous(breaks = seq(from = 2003, to = 2018, by = 1)) + 
  scale_color_viridis(discrete = TRUE, begin = 0, end = 0.8, breaks=c("50","300","600")) +
  ggtitle("Relative (to each time series max. estimate) Biomass Estimates by Year w/ 95% CIs") +
  xlab("Year") +
  ylab("Relative Biomass Estimate") + theme_classic()

## OLD ##

# relative biomass estimates time series by number of knots (note: added +0.1 and +0.2 to year to offset points/line/error bars so they don't overlap)
#ggplot(NULL, aes(x=year, y=est)) + 
  #geom_point(data = biomass_1x_50knots_rel, aes(x=year,y=est, color = "darkorchid1"), size=3) +
  #geom_line(data=biomass_1x_50knots_rel,aes(x=year,y=est, color = "darkorchid1")) +
  #geom_errorbar(data=biomass_1x_50knots_rel, aes(x=year,ymin=lwr, ymax=upr), width=.1, colour="darkorchid1") +
  #geom_point(data = biomass_1x_300knots_rel, aes(x=year+0.1,y=est, color = "mediumblue"), size=3) +
  #geom_line(data=biomass_1x_300knots_rel,aes(x=year+0.1,y=est, color = "mediumblue")) +
  #geom_errorbar(data=biomass_1x_300knots_rel, aes(x=year+0.1,ymin=lwr, ymax=upr), width=.1, colour="mediumblue") +
  #geom_point(data = biomass_1x_600knots_rel, aes(x=year+0.2,y=est, color = "salmon"), size=3) +
  #geom_line(data=biomass_1x_600knots_rel,aes(x=year+0.2,y=est, color = "salmon")) +
  #geom_errorbar(data=biomass_1x_600knots_rel, aes(x=year+0.2,ymin=lwr, ymax=upr), width=.1, colour="salmon") +
  #ggtitle("Relative (to max. est.) Biomass Estimates by Year w/ 95% CIs") + xlab("Year") + 
  #ylab("Relative Biomass Estimate") +
  #scale_x_continuous(breaks = seq(from = 2003, to = 2018, by = 1)) +
  #scale_color_identity(guide = "legend",  labels = c("50 knots", "300 knots", "600 knots"), name = "Number of Knots")

# biomass estimates time series by number of knots
#ggplot(NULL, aes(x=year, y=est)) + 
  #geom_point(data = biomass_1x_50knots, aes(x=year,y=est, color = "darkorchid1"), size=3) +
  #geom_line(data=biomass_1x_50knots,aes(x=year,y=est, color = "darkorchid1")) +
  #geom_errorbar(data=biomass_1x_50knots, aes(x=year,ymin=lwr, ymax=upr), width=.1, colour="darkorchid1") +
  #geom_point(data = biomass_1x_300knots, aes(x=year+0.1,y=est, color = "mediumblue"), size=3) +
  #geom_line(data=biomass_1x_300knots,aes(x=year+0.1,y=est, color = "mediumblue")) +
  #geom_errorbar(data=biomass_1x_300knots, aes(x=year+0.1,ymin=lwr, ymax=upr), width=.1, colour="mediumblue") +
  #geom_point(data = biomass_1x_600knots, aes(x=year+0.2,y=est, color = "salmon"), size=3) +
  #geom_line(data=biomass_1x_600knots,aes(x=year+0.2,y=est, color = "salmon")) +
  #geom_errorbar(data=biomass_1x_600knots, aes(x=year+0.2,ymin=lwr, ymax=upr), width=.1, colour="salmon") +
  #ggtitle("Biomass Estimates by Year w/ 95% CIs") + xlab("Year") + 
  #ylab("Biomass Estimate") +
  #scale_x_continuous(breaks = seq(from = 2003, to = 2018, by = 1)) +
  #scale_color_identity(guide = "legend",  labels = c("50 knots", "300 knots", "600 knots"), name = "Number of Knots")

##

# COG:

# data
COG_1x_50knots = readRDS(paste0("C:/Users/cjcco/Box/Best Practices paper/COG_1x_50knots.rds"))
COG_1x_300knots = readRDS(paste0("C:/Users/cjcco/Box/Best Practices paper/COG_1x_300knots.rds"))
COG_1x_600knots = readRDS(paste0("C:/Users/cjcco/Box/Best Practices paper/COG_1x.rds"))

# add column for knots
COG_1x_50knots[, "knots"] = "50"
COG_1x_300knots[, "knots"] = "300"
COG_1x_600knots[, "knots"] = "600"

# combine dataframes
COG_1x_knots_all = rbind(COG_1x_50knots,COG_1x_300knots,COG_1x_600knots)

# plot COG Eastings by Year with lower/upper 95% CIs, comparative across number of knots
p1 = ggplot(COG_1x_knots_all, aes(x=year, y=X, color=factor(knots))) + 
  geom_point(data = COG_1x_50knots, aes(x=year,y=X), size=3) +
  geom_line(data=COG_1x_50knots, aes(x=year,y=X)) +
  geom_errorbar(data=COG_1x_50knots, aes(x=year,ymin=X_lwr, ymax=X_upr), width=.1) +
  geom_point(data = COG_1x_300knots, aes(x=year+0.1,y=X), size=3) +
  geom_line(data=COG_1x_300knots, aes(x=year+0.1,y=X)) +
  geom_errorbar(data=COG_1x_300knots, aes(x=year+0.1,ymin=X_lwr, ymax=X_upr), width=.1) +
  geom_point(data = COG_1x_600knots, aes(x=year+0.2,y=X), size=3) +
  geom_line(data=COG_1x_600knots, aes(x=year+0.2,y=X)) +
  geom_errorbar(data=COG_1x_600knots, aes(x=year+0.2,ymin=X_lwr, ymax=X_upr), width=.1) + 
  ggtitle("COG Eastings Estimates by Year w/ 95% CIs") + xlab("Year") + 
  ylab("COG Eastings (km)") +
  scale_x_continuous(breaks = seq(2003, 2018, by = 1)) +
  scale_y_continuous(breaks = seq(395, 600, by = 25)) + 
  scale_color_viridis(discrete = TRUE, begin = 0, end = 0.8, breaks=c("50","300","600")) + 
  theme_classic()

# plot COG Northings by Year with lower/upper 95% CIs, comparative across number of knots
p2 = ggplot(COG_1x_knots_all, aes(x=year, y=Y, color=factor(knots))) + 
  geom_point(data = COG_1x_50knots, aes(x=year,y=Y), size=3) +
  geom_line(data=COG_1x_50knots, aes(x=year,y=Y)) +
  geom_errorbar(data=COG_1x_50knots, aes(x=year,ymin=Y_lwr, ymax=Y_upr), width=.1) +
  geom_point(data = COG_1x_300knots, aes(x=year+0.1,y=Y), size=3) +
  geom_line(data=COG_1x_300knots, aes(x=year+0.1,y=Y)) +
  geom_errorbar(data=COG_1x_300knots, aes(x=year+0.1,ymin=Y_lwr, ymax=Y_upr), width=.1) +
  geom_point(data = COG_1x_600knots, aes(x=year+0.2,y=Y), size=3) +
  geom_line(data=COG_1x_600knots, aes(x=year+0.2,y=Y)) +
  geom_errorbar(data=COG_1x_600knots, aes(x=year+0.2,ymin=Y_lwr, ymax=Y_upr), width=.1) + 
  ggtitle("COG Northings Estimates by Year w/ 95% CIs") + xlab("Year") + 
  ylab("COG Northings (km)") +
  scale_x_continuous(breaks = seq(2003, 2018, by = 1)) +
  scale_y_continuous(breaks = seq(4300, 5000, by = 100)) + 
  scale_color_viridis(discrete = TRUE, begin = 0, end = 0.8, breaks=c("50","300","600")) + 
  theme_classic()

grid.arrange(p2, p1, nrow = 2)
