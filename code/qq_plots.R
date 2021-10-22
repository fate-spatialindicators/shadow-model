model_1 <- readRDS("results/fit_spatial_depth_full.rds")
model_2 <- readRDS("results/fit_IID_depth_full.rds")

model_3 <- readRDS("results/fit_spatial_nodepth_full.rds")

model_1$data$resid <- residuals(model_1)
model_2$data$resid <- residuals(model_2)
model_3$data$resid <- residuals(model_3)

model_1$data$model <- "Spatial + depth"
model_2$data$model <- "Spatiotemporal + depth"
model_3$data$model <- "Spatial"

library(ggplot2)

p1 <- ggplot(rbind(model_1$data,model_2$data,model_3$data), aes(sample = resid)) +
  stat_qq_line(col="red",alpha=0.5) +
  stat_qq(alpha=0.01, size=0.5, col="black") +
  xlab("Theoretical quantiles") +
  ylab("Sample quantiles") +
  theme_bw() +
  facet_wrap(~model, scale="free", ncol = 2) +
  theme(strip.background =element_rect(fill="white"))

pdf("qq_plot.pdf")
p1
dev.off()


p1 <- ggplot(rbind(model_1$data,model_2$data,model_3$data), aes(x = resid)) +
  geom_histogram(bins=100) +
  theme_bw() +
  facet_wrap(~model, nrow=1) +
  theme(strip.background =element_rect(fill="white")) +
  xlab("Residual") +
  ylab("Frequency") +
  coord_cartesian(xlim=c(-4,4))
p2 <- ggplot(rbind(model_1$data,model_2$data,model_3$data), aes(sample = resid)) +
  stat_qq_line(col="red",alpha=0.5) +
  stat_qq(alpha=0.02, size=0.5) +
  xlab("Theoretical quantiles") +
  ylab("Sample quantiles") +
  theme_bw() +
  facet_wrap(~model, nrow = 1) +
  theme(strip.background =element_rect(fill="white"))

pdf("qq_plot_with_hist.pdf")
gridExtra::grid.arrange(p1,p2,nrow=2)
dev.off()
