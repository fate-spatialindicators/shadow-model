## Make plots for sdmTMB West Coast ##

library(sdmTMB)
library(ggplot2)
library(viridis)
library(tidyverse)
library(fpc)
library(gridExtra)
library(sp)
library(broom)
library(ggforce) # for plotting ellipses
library(ggsidekick)

# get coastlines and transform to projection and scale of data
shore <- rnaturalearth::ne_countries(continent = "north america", scale = "medium", returnclass = "sp")
shore <- sp::spTransform(shore, CRS = "+proj=utm +zone=10 ellps=WGS84")
shore <- fortify(shore)
shore$long <- shore$long/1000
shore$lat <- shore$lat/1000

# select years to predict and gather replicate prediction data for each of these years
years = 2003:2018
#wc_grid = readRDS("C:/Users/cjcco/Box/Best Practices paper/wc_grid_1x.rds")
wc_grid = readRDS("wc_grid_1x.rds")
Predict_data_years = wc_grid
Predict_data_years$year = 2003

for(i in 2:length(unique(years))) {
  wc_grid$year = unique(years)[i]
  Predict_data_years = rbind(Predict_data_years, wc_grid)
}
rm(wc_grid)

qq_plots = list()
residuals_plots = list()
prediction_plots = list()
COG_plots_N = list()
COG_plots_E = list()
COG_plots_N_E = list()

# plotting functions
plot_map_point <- function(dat, column = "omega_s") {
  ggplot(dat, aes_string("X", "Y", colour = column)) +
    annotation_map(shore, color = "black", fill = "white", size=0.1) +
    geom_point(size=0.1) +
    xlab("Longitude") +
    ylab("Latitude")
}
plot_map_raster <- function(dat, column = "omega_s") {
  ggplot(dat, aes_string("X", "Y", fill = column)) +
    annotation_map(shore, color = "black", fill = "white", size=0.1) +
    geom_raster() +
    scale_fill_viridis_c() +
    xlab("Longitude") +
    ylab("Latitude")
}

#d = readRDS(paste0("C:/Users/cjcco/Box/Best Practices paper/sablefish_600_density_depth_varying600knots.rds"))
d = readRDS("sablefish_600_density_depth_varying_new.rds")

# make timeseries plots of COG, biomass index with 95% CI from model estimates
p_All = predict(d, newdata = Predict_data_years, return_tmb_object = TRUE)  
biomass_estimates = get_index(p_All, bias_correct = TRUE) # bias correction on
COG = get_cog(p_All)


COG_plots_N = ggplot(filter(COG, coord == "Y"), aes(year, est)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "grey70") +
  geom_line(color = "black") +
  theme(axis.title = element_blank(), title = element_text(size = rel(0.9)),
        plot.margin = unit(c(0,0,0,3), "pt"),
        legend.position = "none") +
  scale_x_continuous(breaks=seq(2005, 2015, 5))

COG_plots_E = ggplot(filter(COG, coord == "X"), aes(year, est)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "grey70") +
  geom_line(color = "black") +
  theme(axis.title = element_blank(), title = element_text(size = rel(0.9)),
        plot.margin = unit(c(0,0,0,3), "pt"),
        legend.position = "none") +
  scale_x_continuous(breaks=seq(2005, 2015, 5))

# COGs in 2 dimensions as ellipses
cogs_wide_est <- reshape2::dcast(COG, year ~ coord, value.var = "est")
cogs_wide_lwr <- reshape2::dcast(COG, year ~ coord, value.var = "lwr") %>%
  mutate(X_lwr = X, Y_lwr = Y)
cogs_wide_upr <- reshape2::dcast(COG, year ~ coord, value.var = "upr") %>%
  mutate(X_upr = X, Y_upr = Y)
cogs_wide <- cogs_wide_est %>% 
  left_join(select(cogs_wide_lwr, year, X_lwr, Y_lwr)) %>%
  left_join(select(cogs_wide_upr, year, X_upr, Y_upr))

cogs_wide <- mutate(cogs_wide, diameter_x = X_upr - X_lwr, diameter_y = Y_upr - Y_lwr)

COG_plots_N_E <- ggplot(cogs_wide, aes(X, Y, color = year, fill = year)) + 
  geom_path() +
  ggforce::geom_ellipse(aes(x0 = X, y0 = Y, a = diameter_x/2, b = diameter_y/2, angle = 0), alpha = 0.1) +
  geom_point() +
  scale_color_viridis_c(option = "C") +
  scale_fill_viridis_c(option = "C") +
  ggsidekick::theme_sleek() +
  theme(axis.title = element_blank(), axis.text.x = element_blank(), legend.position = c(0.9,0.82))

# save results
#save.image(file = "C:/Users/cjcco/Box/Best Practices paper/plotdata_all_1x_600knots.Rdata")
save.image(file = "plotdata_all_1x_600knots_biascorrect_new.Rdata")



###############################################################################################
# TO DO: revise figs below and file paths

# COG timeseries plots from model output
ggsave(filename = "C:/Users/cjcco/Box/new stuff/figures/COG_model_est_N.pdf",
       plot = arrangeGrob(grobs = COG_plots_N, ncol = 5, bottom = "Year",
                          left = grid::textGrob("COG Northings (km)", rot = 90, vjust = 0.2)),
       width = 12, height = 8, units = c("in"))
ggsave(filename = "C:/Users/cjcco/Box/new stuff/figures/COG_model_est_E.pdf",
       plot = arrangeGrob(grobs = COG_plots_E, ncol = 5, bottom = "Year",
                          left = grid::textGrob("COG Eastings (km)", rot = 90, vjust = 0.2)),
       width = 12, height = 8, units = c("in"))
# COG as ellipses
ggsave("C:/Users/cjcco/Box/new stuff/figures/COG_model_ellipses.pdf",
       plot = arrangeGrob(grobs = COG_plots_N_E, ncol = 5, bottom = "COG Northings (km)",
                          left = grid::textGrob("COG Eastings (km)", rot = 90, vjust = 0.2)), 
       width = 19, height = 8)

# plot of predictions from full model (all fixed + random effects)
ggsave(filename = "C:/Users/cjcco/Box/new stuff/figures/predicted_density_maps.pdf",
       plot = marrangeGrob(prediction_plots, nrow = 1, ncol = 1),
       width = 7, height = 9, units = c("in"))

# plot only spatiotemporal random effects
ggsave(filename = "C:/Users/cjcco/Box/new stuff/figures/st_maps.pdf",
       plot = marrangeGrob(spatiotemporal_plots, nrow = 1, ncol = 1),
       width = 7, height = 9, units = c("in"))

# qq plots
pdf(file = "C:/Users/cjcco/Box/new stuff/figures/qqnorm.pdf", width = 12, height = 12)
par(mfrow=c(2,5))
for(spp in 1:length(species)) {
  plot(qq_plots[[spp]], pch = ".", main = paste0(species[spp],"_qq")); abline(a = 0, b = 1)
}
dev.off()
# residuals maps
ggsave(filename = "C:/Users/cjcco/Box/new stuff/figures/residuals_maps.pdf",
       plot = marrangeGrob(residuals_plots, nrow = 1, ncol = 1),
       width = 7, height = 9, units = c("in"))

