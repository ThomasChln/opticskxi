context('optics plots')

iris_scaled <- scale(iris[, -5])
pcafort <- fortify_pca(iris_scaled)
optics_obj <- dbscan::optics(iris_scaled)
dbscan_obj <- dbscan::dbscan(iris_scaled, eps = .75)

test_ggplot_optics <- function() {
  expect_is(ggplot_optics(optics_obj), 'gg')
  expect_is(ggplot_optics(optics_obj, dbscan_obj$cluster), 'gg')
}
test_that('ggplot_optics', test_ggplot_optics())

kxi_prms <- expand.grid(n_xi = 1:2, dim_red = 'PCA', dist = 'euclidean',
  pts = c(5, 10), n_dimred_comp = 2:3)
kxi <- opticskxi_pipeline(iris_scaled, kxi_prms)

test_ggplot_kxi <- function() {
  expect_is(ggplot_kxi_metrics(kxi), 'gg')
}
test_that('ggplot_kxi', test_ggplot_kxi())

test_gtable_kxi <- function() {
  expect_is(gtable_kxi_profiles(kxi), 'gtable')
}
test_that('gtable_kxi', test_gtable_kxi())
