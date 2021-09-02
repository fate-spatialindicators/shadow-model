## Make plots of biomass and center of gravity (COG) ##

library(ggplot2)
library(viridis)

# get population density predictions
p_1 = readRDS("results/predictions_1x.rds")
p_4 = readRDS("results/predictions_4x.rds")

# Fine Resolution/1x:
load("C:/Users/cjcco/Box/Best Practices paper/plotdata_all_1x.Rdata")

# Revised plots [only every 3 years for easier plotting/viewing]
cogs_for_plotting = cogs_wide[c(1,4,7,10,13,16),]

p1 = ggplot(cogs_for_plotting, aes(X, Y, color = factor(year))) +
  geom_path(data = cogs_for_plotting, aes(x= X,y= Y, group=1, colour=factor(year)), size=1.0,alpha = 0.7) +
  geom_point(size=3) +
  scale_y_continuous(limits = c(4390, 5000), breaks=(seq(4400, 5000, 100))) +
  scale_x_continuous(limits = c(375, 500), breaks=(seq(375, 500, 25))) +
  geom_segment(aes(x = X_lwr, xend = X_upr, y = Y, yend = Y), lwd = 1) +
  geom_segment(aes(x = X, xend = X, y = Y_lwr, yend = Y_upr), lwd = 1) +
  scale_color_viridis(discrete = TRUE,begin = 0, end = 0.8) +
  xlab("COG Eastings (km)") +
  ylab("COG Northings (km)") +
  ggtitle("Fine") +
  geom_text(aes(label = year), vjust = -1, hjust = -0.2) +
  theme_classic()
#ggsave("C:/Users/cjcco/Box/Best Practices paper/new and revised figures/COG_crosses_1x.pdf", width = 19, height = 8)


# Coarse Resolution/4x:
load("C:/Users/cjcco/Box/Best Practices paper/plotdata_all_4x.Rdata")

cogs_for_plotting = cogs_wide[c(1,4,7,10,13,16),]

p2 = ggplot(cogs_for_plotting, aes(X, Y, color = factor(year))) +
  geom_path(data = cogs_for_plotting, aes(x= X,y= Y, group=1, colour=factor(year)), size=1.0,alpha = 0.7) +
  geom_point(size=3) +
  scale_y_continuous(limits = c(4390, 5000), breaks=(seq(4400, 5000, 100))) +
  scale_x_continuous(limits = c(375, 500), breaks=(seq(375, 500, 25))) +
  geom_segment(aes(x = X_lwr, xend = X_upr, y = Y, yend = Y), lwd = 1) +
  geom_segment(aes(x = X, xend = X, y = Y_lwr, yend = Y_upr), lwd = 1) +
  scale_color_viridis(discrete = TRUE,begin = 0, end = 0.8) +
  xlab("COG Eastings (km)") +
  ylab("COG Northings (km)") +
  ggtitle("Coarse (4x)") +
  geom_text(aes(label = year), vjust = -1, hjust = -0.2) +
  theme_classic()
#ggsave("C:/Users/cjcco/Box/Best Practices paper/new and revised figures/COG_crosses_4x.pdf", width = 19, height = 8)

grid.arrange(p1, p2, nrow = 1)
