library(sdmTMB)

# work with Gaussian for simplicity:
# dat <- dplyr::filter(pcod_2011, density > 0)
# dat$density <- log(dat$density)
# dat <- dplyr::filter(pcod_2011, density > 0)
# mesh <- make_mesh(dat, c("X", "Y"), n_knots = 100)
dat <- pcod_2011
mesh <- make_mesh(dat, c("X", "Y"), cutoff = 10)

# base model:
m <- sdmTMB(
  density ~ 0 + as.factor(year) + s(depth, k = 3),
  data = dat,
  spde = mesh,
  time = "year",
  fields = "IID",
  silent = FALSE,
  share_range = TRUE,
  family = tweedie(),
  # priors = sdmTMBpriors(
  #   matern_s = pc_matern(range_gt = 5, sigma_lt = 5, range_prob = 0.01, sigma_prob = 0.01),
  #   matern_st = pc_matern(range_gt = 5, sigma_lt = 2.5, range_prob = 0.01, sigma_prob = 0.01)
  # )
)
m

library(future)
plan(multisession)

cutoffs <- c(20, 10, 8, 6, 5)
mcv <- lapply(cutoffs, function(k) {
  cat("Cutoff:", k, "\n")
  mesh <- make_mesh(dat, c("X", "Y"), cutoff = k)
  cat("Mesh n:", mesh$mesh$n, "\n")
  set.seed(928394)
  sdmTMB_cv(
    density ~ 0 + as.factor(year) + s(depth, k = 3),
    data = dat,
    spde = mesh,
    time = "year",
    fields = "IID",
    family = tweedie(),
    # priors = sdmTMBpriors(
      # matern_s = pc_matern(range_gt = 5, sigma_lt = 5, range_prob = 0.05, sigma_prob = 0.05),
      # matern_st = pc_matern(range_gt = 5, sigma_lt = 2.5, range_prob = 0.01, sigma_prob = 0.01)
    # ),
    share_range = TRUE,
    k_folds = 8L
  )
})
lls <- purrr::map_dbl(mcv, "sum_loglik")

plot(cutoffs, lls)
# hmm...

# check more naively with mean squared error:
mse <- list()
for (i in seq_along(mcv)) {
  mse[[i]] <- (mcv[[i]]$data$cv_predicted - mcv[[i]]$data$density)^2
}
plot(cutoffs, purrr::map_dbl(mse, mean))

ll_check <- list()
for (i in seq_along(mcv)) {
    ll_check[[i]] <- sum(mcv[[i]]$data$cv_loglik)
}
plot(cutoffs, unlist(ll_check))

sqe <- (mcv[[1]]$data$cv_predicted - mcv[[1]]$data$density)^2
plot(sqe, mcv[[1]]$data$cv_loglik)

sqe <- (mcv[[2]]$data$cv_predicted - mcv[[2]]$data$density)^2
plot(sqe, mcv[[2]]$data$cv_loglik)

sqe <- (mcv[[4]]$data$cv_predicted - mcv[[4]]$data$density)^2
plot(sqe, mcv[[4]]$data$cv_loglik)

test1 <- purrr::map_dbl(mcv, function(.m) {
  sum(.m$data$cv_loglik)
})
plot(test1, lls)
plot(cutoffs, test1)

# calculate log likelihoods by hand to check:
for (j in 1:max(mcv[[1]]$data$cv_fold)) {
  phi <- exp(mcv[[1]]$models[[j]]$model$par[["ln_phi"]])
  actual <- mcv[[1]]$data[mcv[[1]]$data$cv_fold == j, ]$density
  pred <- mcv[[1]]$data[mcv[[1]]$data$cv_fold == j, ]$cv_predicted
  calc <- sum(dnorm(x = actual, mean = pred, sd = phi, log = TRUE))
  cat("Calculated again: ", calc, "\n")
  cat("sdmTMB_cv: ", mcv[[1]]$fold_loglik[j], "\n")
}

out <- list()
for (i in seq_along(mse)) {
  calc_ll <- list()
  for (j in 1:max(mcv[[1]]$data$cv_fold)) {
    phi <- exp(mcv[[i]]$models[[j]]$model$par[["ln_phi"]])
    actual <- mcv[[i]]$data[mcv[[i]]$data$cv_fold == j, ]$density
    pred <- mcv[[i]]$data[mcv[[i]]$data$cv_fold == j, ]$cv_predicted
    calc <- sum(dnorm(x = actual, mean = pred, sd = phi, log = TRUE))
    calc_ll[[j]] <- calc
  }
  out[[i]] <- sum(unlist(calc_ll))
}
# hmm...

plot(cutoffs, unlist(out))
plot(purrr::map_dbl(mcv, "sum_loglik"), unlist(out))

purrr::map_dbl(mcv, "sum_loglik")
unlist(out)

# look at predictions?

library(ggplot2)
plot_map <- function(dat, column = "est") {
  ggplot(dat, aes_string("X", "Y", fill = column)) +
    geom_raster() +
    scale_fill_viridis_c() +
    facet_wrap(~year) +
    coord_fixed()
}

nd <- dplyr::filter(qcs_grid, year %in% unique(dat$year))
p_coarse <- predict(mcv[[1]]$models[[1]], newdata = nd)
g1 <- plot_map(subset(p_coarse, year == 2017), "est") + ggtitle("Coarse")
tidy(mcv[[1]]$models[[1]], "ran_pars", conf.int = TRUE)

p_fine <- predict(mcv[[length(cutoffs)]]$models[[1]], newdata = nd)
g2 <- plot_map(subset(p_fine, year == 2017), "est") + ggtitle("Fine")
# mcv[[length(cutoffs)]]$models[[1]]
tidy(mcv[[length(cutoffs)]]$models[[1]], "ran_pars", conf.int = TRUE)

cowplot::plot_grid(g1, g2)

g1 <- plot_map(subset(p_coarse, year == 2017), "omega_s") + ggtitle("Coarse") +
  scale_fill_gradient2()
g2 <- plot_map(subset(p_fine, year == 2017), "omega_s") + ggtitle("Fine") +
  scale_fill_gradient2()
cowplot::plot_grid(g1, g2)

g3 <- plot_map(subset(p_coarse, year == 2017), "epsilon_st") + ggtitle("Coarse") +
  scale_fill_gradient2()
g4 <- plot_map(subset(p_fine, year == 2017), "epsilon_st") + ggtitle("Fine") +
  scale_fill_gradient2()
cowplot::plot_grid(g1, g2, g3, g4, nrow = 2)

# ballpark similar?
tidy(m, "ran_pars", conf.int = TRUE)
tidy(mcv[[1]]$models[[1]], "ran_pars", conf.int = TRUE)
tidy(mcv[[length(cutoffs)]]$models[[1]], "ran_pars", conf.int = TRUE)

out <- purrr::map_dfr(seq_along(mcv), function(i) {
  tidy(mcv[[i]]$models[[1]], "ran_pars", conf.int = TRUE)
}, .id = "cutoff_id")
out <- dplyr::left_join(out, data.frame(cutoff_id = unique(out$cutoff_id), cutoff = cutoffs))

tweedie_p <- purrr::map_dbl(seq_along(mcv), function(i) {
  stats::plogis(mcv[[i]]$models[[1]]$model$par[["thetaf"]]) + 1
})
out <- bind_rows(out, data.frame(cutoff = cutoffs, estimate = tweedie_p, term = "tweedie_p"))

lls <- purrr::map_dbl(mcv, "sum_loglik")
# plot(cutoffs, lls)
out <- bind_rows(out, data.frame(cutoff = cutoffs, estimate = lls, term = "cv_loglik"))

library(dplyr)
filter(out, cutoff < 20) %>%
  ggplot(aes(estimate, y = cutoff,
    xmin = conf.low, xmax = conf.high)) +
  geom_pointrange() +
  facet_wrap(~term, scales = "free")

# without cross-validation?
mesh <- make_mesh(dat, c("X", "Y"), cutoff = 10)
m_coarse <- sdmTMB(
  density ~ 0 + as.factor(year) + s(depth, k = 3),
  data = dat,
  spde = mesh,
  time = "year",
  fields = "IID",
  silent = FALSE,
  family = tweedie()
)
mesh <- make_mesh(dat, c("X", "Y"), cutoff = 5)
m_fine <- sdmTMB(
  density ~ 0 + as.factor(year) + s(depth, k = 3),
  data = dat,
  spde = mesh,
  time = "year",
  fields = "IID",
  silent = FALSE,
  family = tweedie()
)

# *negative* log lik:
m_coarse$model$objective
m_fine$model$objective

# manual 2-fold cross-validation check:

dat$fold <- sample(c(1L, 2L), nrow(dat), replace = TRUE)

dat_fit <- dplyr::filter(dat, fold == 1L)
dat_check <- dplyr::filter(dat, fold == 2L)

mesh <- make_mesh(dat_fit, c("X", "Y"), cutoff = 10)
m_coarse <- sdmTMB(
  density ~ 0 + as.factor(year) + s(depth, k = 3),
  data = dat_fit,
  spde = mesh,
  time = "year",
  fields = "IID",
  silent = FALSE,
  family = tweedie()
)
pred_coarse <- predict(m_coarse, newdata = dat_check)

mesh <- make_mesh(dat_fit, c("X", "Y"), cutoff = 5)
m_fine <- sdmTMB(
  density ~ 0 + as.factor(year) + s(depth, k = 3),
  data = dat_fit,
  spde = mesh,
  time = "year",
  fields = "IID",
  silent = FALSE,
  family = tweedie()
)
pred_fine <- predict(m_fine, newdata = dat_check)


sum(sdmTMB:::ll_tweedie(m_coarse, withheld_y = pred_coarse$density, withheld_mu = exp(pred_coarse$est)))
sum(sdmTMB:::ll_tweedie(m_fine, withheld_y = pred_fine$density, withheld_mu = exp(pred_fine$est)))

# back to Gaussian?

dat_fit <- dplyr::filter(dat, fold == 1L, density > 0) %>%
  mutate(density = log(density))
dat_check <- dplyr::filter(dat, fold == 2L, density > 0) %>%
  mutate(density = log(density))

mesh <- make_mesh(dat_fit, c("X", "Y"), cutoff = 10)
m_coarse <- sdmTMB(
  density ~ 0 + as.factor(year) + s(depth, k = 3),
  data = dat_fit,
  spde = mesh,
  time = "year",
  fields = "IID",
  silent = FALSE
  # family = tweedie()
)
pred_coarse <- predict(m_coarse, newdata = dat_check)

mesh <- make_mesh(dat_fit, c("X", "Y"), cutoff = 5)
m_fine <- sdmTMB(
  density ~ 0 + as.factor(year) + s(depth, k = 3),
  data = dat_fit,
  spde = mesh,
  time = "year",
  fields = "IID",
  silent = FALSE
  # family = tweedie()
)
pred_fine <- predict(m_fine, newdata = dat_check)

sum(sdmTMB:::ll_gaussian(m_coarse, withheld_y = pred_coarse$density, withheld_mu = exp(pred_coarse$est)))
sum(sdmTMB:::ll_gaussian(m_fine, withheld_y = pred_fine$density, withheld_mu = exp(pred_fine$est)))
