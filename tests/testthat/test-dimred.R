context("dimension reduction")

test_fortify_dimred <- function() {
  pca <- prcomp(iris[-5])

  expect_is(fortify_dimred(pca$x), 'data.frame')
  expect_is(fortify_dimred(pca$x, pca$rotation), 'data.frame')
}
test_that('fortify_dimred', test_fortify_dimred())

test_fortify_pca <- function() {
  expect_is(fortify_pca(iris[-5]), 'data.frame')
}
test_that('fortify_pca', test_fortify_pca())

test_fortify_ica <- function() {
  expect_is(fortify_ica(iris[-5], 3), 'data.frame')
}
test_that('fortify_ica', test_fortify_ica())
