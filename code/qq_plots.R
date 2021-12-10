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

# simulation based:
if (packageVersion("sdmTMB") >= "0.0.20.9000") {
  model_1 <- readRDS("results/fit_spatial_depth_full.rds")
  model_2 <- readRDS("results/fit_IID_depth_full.rds")
  model_3 <- readRDS("results/fit_spatial_nodepth_full.rds")

  get_resid <- function(obj) {
    sim <- simulate(obj, nsim = 500L)
    pred_fixed <- obj$family$linkinv(predict(obj, newdata = NULL)$est_non_rf)
    DHARMa::createDHARMa(
      simulatedResponse = sim,
      observedResponse = obj$data$cpue_kg_km2,
      fittedPredictedResponse = pred_fixed
    )
  }
  res1 <- get_resid(model_1)
  res2 <- get_resid(model_2)
  res3 <- get_resid(model_3)

  plot_resid <- function(res, main = "") {
    gap::qqunif(
      res$scaledResiduals,
      pch = 19,
      bty = "n",
      logscale = FALSE,
      col = "#00000010",
      cex = 0.5,
      ann = FALSE, xlim = c(0, 1), ylim = c(0, 1)
    )
    mtext(side = 3, text = main, line = 0.5, cex = 0.8)
    mtext(side = 1, line = 2, text = "Expected", cex = 0.8)
    mtext(side = 2, line = 2, text = "Observed", cex = 0.8)
    box()
  }
  png("plots/residuals-dharma-comparison.png", res = 250,
    width = 9, height = 3, units = "in")
  par(mar = c(3, 4, 2, 1), cex = 0.8, mfrow = c(1, 3),
    cex.main = 0.8, cex.axis = 0.9, xaxs = "i", yaxs = "i")
  plot_resid(res3, "Spatial")
  plot_resid(res1, "Spatial + depth")
  plot_resid(res2, "Spatiotemporal + depth")
  dev.off()
}
