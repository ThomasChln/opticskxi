
#' dist_matrix
#'
#' Dispatch of amap::Dist, cosine_dist, and norm_inprod methods.
#'
#' @param data    Rectangular numeric matrix [Observations, Features]
#' @param method  Methods accepted by amap::Dist or cosine and norm_inprod
#' @param n_cores Number of cores
#' @return Distance symmetric matrix
#'
#' @export
dist_matrix <- function(data, method = 'euclidean', n_cores = 1) {

  amap_methods <- c("euclidean", "maximum", "manhattan", "canberra",
    "binary", "pearson", "correlation", "spearman", "kendall",
    "abspearson", "abscorrelation")
  methods <- c(amap_methods, 'cosine', 'norm_inprod')

  if (!method %in% methods) stop('Invalid distance method')

  switch(method, cosine = cosine_dist(data),
         norm_inprod = -norm_inprod(data), {
           amap::Dist(data, method, nbproc = n_cores)
         })
}

#' stddev_mean
#'
#' Get mean of standard deviations of matrix columns
#'
#' @param m Numeric matrix
#' @return Mean of standard deviations of matrix columns
#'
#' @export
stddev_mean <- function(m) {
  m <- as.matrix(m)
  stopifnot(is.matrix(m) && is.numeric(m))
  mean(sqrt(apply(m, 2, var)))
}


#' norm_inprod 
#'
#' Normalized inner product with transposed input matrix
#'
#' @param m Numeric matrix
#' @return Numeric matrix
#'
#' @export
norm_inprod <- function(m) {
  m_inprod <- inprod(m - rowMeans(m))
  m_inprod <- apply(m_inprod, 1, function(i) i / as.vector(sqrt(i %*% i)))
  rownames(m_inprod) <- rownames(m)

  as.dist(m_inprod)
}

# inner product
inprod <- function(x, y) {
  if (missing(y)) y = x
  x %*% t(y)
}



#' Cosine similarity between vectors and/or matrices.
#'
#' Inputs will be L2 normalized, then matrix multiplied (y is transposed).
#' If second input is missing, first input will be recycled, which enables to
#' efficiently compute cosine similarities between the rows of a rectangular
#' matrix.
#'
#' @param x Numeric vector or matrix
#' @param y Numeric vector or matrix. If missing, copied from parameter x.
#' @return Symmetric numeric similarity matrix
#'
#' @export
cosine_simi = function(x, y) {

  x = normalize(x, norm = 'l2')
  y = if (missing(y)) x else normalize(y, norm = 'l2')

  x %*% t(y)
}

cosine_dist = function(x, method) as.dist(1 - cosine_simi(x))


#' Matrix normalization
#'
#' Normalize matrix rows using given norm. Copied from text2vec package.
#'
#' @param m \code{matrix} (sparse or dense).
#' @param norm \code{character} the method used to normalize term vectors
#' @return normalized matrix
#' @export
normalize = function(m, norm = c("l1", "l2", "none")) {

  stopifnot(inherits(m, "matrix") || inherits(m, "sparseMatrix"))
  norm = match.arg(norm)

  if (norm == "none") return(m)

  norm_vec = switch(norm,
                    l1 = 1 / rowSums(m),
                    l2 = 1 / sqrt(rowSums(m ^ 2)))

  # case when sum row elements == 0
  norm_vec[is.infinite(norm_vec)] = 0

  if (inherits(m, "sparseMatrix")) {
    Matrix::rowScale(m, norm_vec)
  } else {
    m * norm_vec
  }
}
