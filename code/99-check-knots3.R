# with sim data

library(ggplot2)
library(sdmTMB)
library(dplyr)
library(future)
# plan(sequential)
plan(multisession)

set.seed(9283)
x <- runif(500, -1, 1)
y <- runif(500, -1, 1)
N <- length(x)
plot(x, y)
time_steps <- 1
X <- model.matrix(~ x1, data.frame(x1 = rnorm(N * time_steps)))
head(X)
loc <- data.frame(x = x, y = y)
mesh <- make_mesh(loc, xy_cols = c("x", "y"), cutoff = 0.02)
s <- sdmTMB_sim(
  x = x, y = y, mesh = mesh, X = X,
  betas = c(0.2, 0.3), time_steps = time_steps,
  phi = 0.2, thetaf = 1.3, range = 0.2, sigma_O = 0.4,
  seed = 1234, family = tweedie(link = "log")
)

## Trial:
# mesh <- make_mesh(s, xy_cols = c("x", "y"), cutoff = 0.3)
# plot(mesh)
# mesh$mesh$n
# m <- sdmTMB(
#   data = s, formula = observed ~ 1 + x1, silent = FALSE,
#   spde = mesh, family = tweedie())

cutoffs <- c(0.4, 0.3, 0.2, 0.1, 0.05, 0.02)
mcv <- lapply(cutoffs, function(k) {
  cat("Cutoff:", k, "\n")
  mesh <- make_mesh(s, c("x", "y"), cutoff = k)
  cat("Mesh n:", mesh$mesh$n, "\n")
  set.seed(123)
  sdmTMB_cv(
    observed ~ 1 + x1,
    data = s,
    spde = mesh, # constant mesh
    # mesh_args = list(cutoff = k, xy_cols = c("x", "y")), # create mesh each fold
    silent = TRUE,
    parallel = TRUE,
    priors = sdmTMBpriors(matern_s = pc_matern(range_gt = 0.1, sigma_lt = 2)),
    family = tweedie(link = "log"),
    k_folds = 8L
  )
})
lls <- purrr::map_dbl(mcv, "sum_loglik")
vert <- purrr::map_dbl(mcv, function(x) x$models[[1]]$spde$mesh$n)
plot(cutoffs, lls, type = "o")
plot(vert, lls, type = "o")

# predictions?

plot_map <- function(dat, column = "omega_s") {
  ggplot(dat, aes_string("x", "y", fill = column)) +
    geom_raster() +
    scale_fill_gradient2(limits = ) +
    # facet_wrap(~year) +
    coord_fixed()
}

nd <- expand.grid(x = seq(0, 1, length.out = 50), y = seq(0, 1, length.out = 50))
nd$x1 <- rnorm(nrow(nd))

p <- purrr::map_dfr(seq_along(mcv), function(i) {
  p <- predict(mcv[[i]]$models[[1]], newdata = nd)
  p$cutoff <- cutoffs[i]
  p$vertices <- mcv[[i]]$models[[1]]$spde$mesh$n
  p$cv_log_lik <- mcv[[i]]$sum_loglik
  p
})

p <- group_by(p, cutoff) %>%
  mutate(ll_relative = cv_log_lik - max(p$cv_log_lik))
p$meta <- paste0("Vert.: ", stringr::str_pad(p$vertices, 3L), ", ",
  "CV LL: ", round(p$ll_relative, 2))
p$meta2 <- paste0("Cut.: ", stringr::str_pad(p$cutoff, 2L, "right"), ", ",
  "CV LL: ", round(p$ll_relative, 2))

ggplot(p, aes(x, y, fill = omega_s)) +
  geom_raster() +
  scale_fill_gradient2() +
  facet_wrap(~meta2) +
  coord_fixed(expand = FALSE)
}

ggplot(p, aes(x, y, fill = est)) +
  geom_raster() +
  scale_fill_viridis_c() +
  facet_wrap(~meta) +
  coord_fixed(expand = FALSE)

