## Biomass Estimates Plots ##

library(ggplot2)

# data
biomass_1x = readRDS(paste0("C:/Users/cjcco/Box/Best Practices paper/biomass_estimate_1x.rds"))
biomass_4x = readRDS(paste0("C:/Users/cjcco/Box/Best Practices paper/biomass_estimate_4x.rds"))

# standardized (biomass estimate time series divided by max. biomass estimate) plot
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

# note: added +0.2 to year to offset points/line/error bars so they don't overlap
ggplot(biomass_est_all, aes(x=year, y=est, color=factor(Grid_Resolution))) + 
  geom_point(data = biomass_1x_rel, aes(x=year,y=est), size=3) +
  geom_line(data=biomass_1x_rel,aes(x=year,y=est)) +
  geom_errorbar(data=biomass_1x_rel, aes(x=year,ymin=lwr, ymax=upr), width=.1) +
  geom_point(data = biomass_4x_rel, aes(x=year+0.1,y=est), size=3) +
  geom_line(data=biomass_4x_rel,aes(x=year+0.1,y=est)) +
  geom_errorbar(data=biomass_4x_rel, aes(x=year+0.1,ymin=lwr, ymax=upr), width=.1) + 
  scale_x_continuous(breaks = seq(from = 2003, to = 2018, by = 1)) + 
  scale_color_viridis(discrete = TRUE, begin = 0, end = 0.8, breaks=c("Fine (1x)","Coarse (4x)")) +
  ggtitle("Relative (to each time series max. estimate) Biomass Estimates by Year w/ 95% CIs") +
  xlab("Year") +
  ylab("Relative Biomass Estimate") + theme_classic()

# Coefficient of Variation (CV) across years
biomass_1x_mean = mean(biomass_1x$est)
biomass_1x_sd = sd(biomass_1x$est)
biomass_1x_cv = biomass_1x_sd/biomass_1x_mean

biomass_4x_mean = mean(biomass_4x$est)
biomass_4x_sd = sd(biomass_4x$est)
biomass_4x_cv = biomass_4x_sd/biomass_4x_mean

# CVs for each year (Annual CVs)
# CV = std. dev. / mean; use std. err. instead of std. dev.
# use log_est instead of est because std. err. is in log space
biomass_1x_cvs = data.frame("year" = year)
biomass_1x_cvs[, "CV"] = biomass_1x$se/biomass_1x$log_est
mean(biomass_1x_cvs$CV) # grand mean

biomass_4x_cvs = data.frame("year" = year)
biomass_4x_cvs[, "CV"] = biomass_4x$se/biomass_4x$log_est
mean(biomass_4x_cvs$CV) # grand mean

# plot biomass estimate (y) by year (x) with lower/upper CI, for all prediction grids [normal scale]
ggplot(NULL, aes(x=year, y=est)) + 
  geom_point(data = biomass_1x, aes(x=year, y=est, col="magenta3"), size=3) +
  geom_point(data = biomass_4x, aes(x=year, y=est, col="steelblue3"), size=3) +
  geom_line(data=biomass_1x, aes(x=year,y=est, colour="magenta3")) +
  geom_errorbar(data=biomass_1x, aes(ymin=lwr, ymax=upr), width=.1, colour="magenta3") +
  geom_line(data=biomass_4x, aes(x=year,y=est, colour="steelblue3")) +
  geom_errorbar(data=biomass_4x, aes(ymin=lwr, ymax=upr), width=.1, colour="steelblue3") +
  ggtitle("Biomass Estimates by Year w/ 95% CIs") + xlab("Year") + 
  ylab("Biomass Estimate") +
  scale_x_continuous(breaks = seq(2003, 2018, by = 1)) +
  scale_color_identity(guide = "legend",  labels = c("Fine (1x)", "Coarse (4x)"), name = "Grid Resolution")

# log scale (ln, natural log) CIs for easier plotting
biomass_1x[, "log_lwr"] = log(biomass_1x$lwr)
biomass_1x[, "log_upr"] = log(biomass_1x$upr)
biomass_4x[, "log_lwr"] = log(biomass_4x$lwr)
biomass_4x[, "log_upr"] = log(biomass_4x$upr)

# plot biomass estimate (y) by year (x) with lower/upper CI, for all prediction grids [log scale (ln, natural log)]
ggplot(NULL, aes(x=year, y=log_est)) + 
  geom_point(data = biomass_1x, aes(x=year, y=log_est, col="magenta3"), size=3) +
  geom_point(data = biomass_4x, aes(x=year, y=log_est, col="steelblue3"), size=3) +
  geom_line(data=biomass_1x, aes(x=year,y=log_est, colour="magenta3")) +
  geom_errorbar(data=biomass_1x, aes(ymin=log_lwr, ymax=log_upr), width=.1, colour="magenta3") +
  geom_line(data=biomass_4x, aes(x=year,y=log_est, colour="steelblue3")) +
  geom_errorbar(data=biomass_4x, aes(ymin=log_lwr, ymax=log_upr), width=.1, colour="steelblue3") +
  scale_x_continuous(breaks = seq(2003, 2018, by = 1)) +
  scale_color_identity(guide = "legend",  labels = c("Fine (1x)", "Coarse (4x)"), name = "Grid Resolution") +
  ggtitle("Biomass Estimates (log scale) by Year w/ 95% CIs") + xlab("Year") + 
  ylab("Biomass Estimate (log scale)")
