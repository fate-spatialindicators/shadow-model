## COG Time Series Plots ##

library(ggplot2)
library(viridis)

# data
COG_1x = readRDS(paste0("C:/Users/cjcco/Box/Best Practices paper/COG_1x.rds"))
COG_4x = readRDS(paste0("C:/Users/cjcco/Box/Best Practices paper/COG_4x.rds"))

# add column for grid resolution
COG_1x[, "Grid_Resolution"] = "Fine"
COG_4x[, "Grid_Resolution"] = "Coarse (4x)"

# combine dataframes
COG_all = rbind(COG_1x,COG_4x)

# plot COG Eastings (y) by Year (x) with lower/upper 95% CIs
p1 = ggplot(COG_all, aes(x=year, y=X,color=factor(Grid_Resolution))) + 
  geom_point(data = COG_1x, aes(x=year,y=X), size=3) +
  geom_point(data = COG_4x, aes(x=year+0.1,y=X), size=3) +
  geom_line(data=COG_1x, aes(x=year,y=X)) +
  geom_errorbar(data=COG_1x, aes(x=year,ymin=X_lwr, ymax=X_upr), width=.1) +
  geom_line(data=COG_4x, aes(x=year+0.1,y=X)) +
  geom_errorbar(data=COG_4x, aes(x=year+0.1,ymin=X_lwr, ymax=X_upr), width=.1) +
  ggtitle("COG Eastings Estimates by Year w/ 95% CIs") + xlab("Year") + 
  ylab("COG Eastings (km)") +
  scale_x_continuous(breaks = seq(2003, 2018, by = 1)) +
  scale_y_continuous(breaks = seq(375, 525, by = 25)) + 
  scale_color_viridis(discrete = TRUE, begin = 0, end = 0.8, breaks=c("Fine","Coarse (4x)")) + 
  theme_classic()

# plot COG Northings (y) by Year (x) with lower/upper 95% CIs
p2 = ggplot(COG_all, aes(x=year, y=Y,color=factor(Grid_Resolution))) + 
  geom_point(data = COG_1x, aes(x=year,y=Y), size=3) +
  geom_point(data = COG_4x, aes(x=year+0.1,y=Y), size=3) +
  geom_line(data=COG_1x, aes(x=year,y=Y)) +
  geom_errorbar(data=COG_1x, aes(x=year,ymin=Y_lwr, ymax=Y_upr), width=.1) +
  geom_line(data=COG_4x, aes(x=year+0.1,y=Y)) +
  geom_errorbar(data=COG_4x, aes(x=year+0.1,ymin=Y_lwr, ymax=Y_upr), width=.1) +
  ggtitle("COG Northings Estimates by Year w/ 95% CIs") + xlab("Year") + 
  ylab("COG Northings (km)") +
  scale_x_continuous(breaks = seq(2003, 2018, by = 1)) +
  scale_y_continuous(breaks = seq(4300, 5000, by = 100)) + 
  scale_color_viridis(discrete = TRUE, begin = 0, end = 0.8, breaks=c("Fine","Coarse (4x)")) + 
  theme_classic()

grid.arrange(p2, p1, nrow = 2)

#####################

# [NO CIs] plot COG Eastings (x) and Northings (y) [data points are Years]
ggplot(NULL, aes(x=X, y=Y)) + 
  geom_point(data = COG_1x, aes(x=X,y=Y, color = "magenta3"), size=3) +
  geom_point(data = COG_4x, aes(x=X,y=Y, color = "steelblue3"), size=3) +
  geom_line(data=COG_1x, aes(x=X,y=Y, color = "magenta3")) +
  geom_line(data=COG_4x, aes(x=X,y=Y, color = "steelblue3")) +
  ggtitle("COG Eastings (x) and Northings (y) by Year") + xlab("Eastings") + 
  ylab("Northings") +
  scale_color_identity(guide = "legend",  labels = c("Fine (1x)", "Coarse (4x)"), name = "Grid Resolution") +
  geom_text(data=COG_1x, aes(label=year), hjust=0, vjust=0) +
  geom_text(data=COG_4x, aes(label=year), hjust=0, vjust=0)

# plot COG Eastings (x) and Northings (y) [data points are Years] with lower/upper 95% CIs
ggplot(NULL, aes(x=X, y=Y)) + 
  geom_point(data = COG_1x, aes(x=X,y=Y, color = "magenta3"), size=3) +
  geom_point(data = COG_4x, aes(x=X,y=Y, color = "steelblue3"), size=3) +
  geom_line(data=COG_1x, aes(x=X,y=Y, color = "magenta3")) +
  geom_errorbar(data=COG_1x, aes(ymin=Y_lwr, ymax=Y_upr), width=.1, colour="magenta3") +
  geom_errorbar(data=COG_1x, aes(xmin=X_lwr, xmax=X_upr), width=.1, colour="magenta3") +
  geom_line(data=COG_4x, aes(x=X,y=Y, color = "steelblue3")) +
  geom_errorbar(data=COG_4x, aes(ymin=Y_lwr, ymax=Y_upr), width=.1, colour="steelblue3") +
  geom_errorbar(data=COG_4x, aes(xmin=X_lwr, xmax=X_upr), width=.1, colour="steelblue3") +
  ggtitle("COG Eastings (x) and Northings (y) by Year w/ 95% CIs") + xlab("Eastings") + 
  ylab("Northings") +
  scale_color_identity(guide = "legend",  labels = c("Fine (1x)", "Coarse (4x)"), name = "Grid Resolution") +
  geom_text(data=COG_1x, aes(label=year), hjust=0, vjust=0) +
  geom_text(data=COG_4x, aes(label=year), hjust=0, vjust=0)

# Coefficient of Variation (CV) for Eastings across years
COG_1x_mean_E = mean(COG_1x$X)
COG_1x_sd_E = sd(COG_1x$X)
COG_1x_cv_E = COG_1x_sd_E/COG_1x_mean_E
COG_1x_stderr_E = COG_1x_sd_E/sqrt(length(COG_1x$X))

COG_4x_mean_E = mean(COG_4x$X)
COG_4x_sd_E = sd(COG_4x$X)
COG_4x_cv_E = COG_4x_sd_E/COG_4x_mean_E
COG_4x_stderr_E = COG_4x_sd_E/sqrt(length(COG_4x$X))

# Coefficient of Variation (CV) for Northings across years
COG_1x_mean_N = mean(COG_1x$Y)
COG_1x_sd_N = sd(COG_1x$Y)
COG_1x_cv_N = COG_1x_sd_N/COG_1x_mean_N
COG_1x_stderr_N = COG_1x_sd_N/sqrt(length(COG_1x$Y))

COG_4x_mean_N = mean(COG_4x$Y)
COG_4x_sd_N = sd(COG_4x$Y)
COG_4x_cv_N = COG_4x_sd_N/COG_4x_mean_N
COG_4x_stderr_N = COG_4x_sd_N/sqrt(length(COG_4x$Y))

#CVs are approx. equal (CVs --> 1x: N- 0.024, E- 0.05; 4x: N- 0.026, E- 0.05), so COG estimate precision 
#is about the same between resolutions. However, the accuracy
#of estimates differ (Means +- St Dev --> 1x: N- 4518+-107, E- 464+-21; 4x: N- 4540+-120, E- 459+-23), 
#with coarse resolution biasing COG estimates towards the North and West; 
#whereas fine resolution comparatively has COG estimates towards the South and East. 
#This is most apparent in the most recent year (2018) where the estimate of COG is further 
#Northwest for the coarse resolution compared to the fine resolution.
