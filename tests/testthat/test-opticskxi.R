context('opticskxi')

set.seed(1)

params <- expand.grid(n_xi = 2:3,
                      dim_red = c('identity', 'PCA', 'ICA'),
                      dist = c('euclidean', 'correlation'),
                      pts = c(5, 10),
                      n_dimred_comp = 2:3)

kxi <- opticskxi_pipeline(scale(iris[-5]), params)



test_opticskxi_pipeline <- function() {

  expect_equal(dim(kxi), c(40, 9))

  expect_equal(as.vector(table(kxi$clusters[[1]])), c(59, 91))

  expect_equal(length(kxi$optics[[1]]), 8)

  expect_equal(names(kxi$metrics[[1]]),
               c('avg.silwidth', 'bw.ratio', 'ch', 'pearsongamma', 'dunn',
                 'dunn2', 'entropy', 'widestgap', 'sindex'))
}
test_that('opticskxi_pipeline', test_opticskxi_pipeline())



best_kxi <- get_best_kxi(kxi, 'bw.ratio')

test_get_best_kxi <- function() {

  expect_equal(as.vector(table(best_kxi$clusters)), c(28, 49, 72),
               check.attributes = FALSE)

  best_kxi <- get_best_kxi(kxi, 'avg.silwidth', rank = 2)

  expect_equal(as.vector(table(best_kxi$clusters)), c(100, 49))
}
test_that('get_best_kxi', test_get_best_kxi())



test_residuals_table <- function() {

  resid_tab <- residuals_table(best_kxi$clusters, iris$Species)
  expect_is(resid_tab, 'matrix')

  latex_table <- utils::capture.output(print_vignette_table(resid_tab, 'iris'))
  expect_is(latex_table, 'character')
}
test_that('residuals_table', test_residuals_table())
