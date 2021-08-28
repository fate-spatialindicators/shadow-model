#### Generate Biomass and COG Estimates; plots ####

library(sdmTMB)
library(ggplot2)
library(viridis)
library(tidyverse)
library(fpc)
library(gridExtra)
library(sp)
library(broom)
#devtools::install_github("seananderson/ggsidekick")
library(ggsidekick)

# select years to predict and gather replicate prediction data for each of these years
years = 2003:2018
wc_grid = readRDS("data/wc_grid_1x.rds")
#wc_grid = readRDS("wc_grid_4x.rds")
Predict_data_years = wc_grid
Predict_data_years$year = 2003

for(i in 2:length(unique(years))) {
  wc_grid$year = unique(years)[i]
  Predict_data_years = rbind(Predict_data_years, wc_grid)
}
rm(wc_grid)

# load fitted model
m = readRDS("results/fit_spatial_depth_full.rds")

# make timeseries plots of COG, biomass index with 95% CI from model estimates
p = predict(m, newdata = Predict_data_years, return_tmb_object = TRUE)
rm(Predict_data_years)
saveRDS(p, "results/predictions_1x.rds")

# compute indices (takes a lot of memory for 1x resolution predictions)
gc()
biomass_estimates = get_index(p, bias_correct = TRUE) # bias correction on
COG = get_cog(p, bias_correct = TRUE)

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

###############################################################################################
# TO DO: add biomass figs, etc.

# COG timeseries plots from model output
ggsave(filename = "plots/COG_model_est_N.pdf",
       plot = arrangeGrob(grobs = COG_plots_N, ncol = 5, bottom = "Year",
                          left = grid::textGrob("COG Northings (km)", rot = 90, vjust = 0.2)),
       width = 12, height = 8, units = c("in"))
ggsave(filename = "plots/COG_model_est_E.pdf",
       plot = arrangeGrob(grobs = COG_plots_E, ncol = 5, bottom = "Year",
                          left = grid::textGrob("COG Eastings (km)", rot = 90, vjust = 0.2)),
       width = 12, height = 8, units = c("in"))
