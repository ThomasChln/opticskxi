
#' Fortify a dimension reduction object
#'
#' @param m_dimred   Projection matrix
#' @param m_vars     Rotation matrix (optional)
#' @param v_variance Explained variance (optional)
#' @param sup_vars   Optional supplementary variables
#' @param var_digits Explained variance percent digits
#' @return Data frame
#' @seealso \link{fortify_pca}, \link{fortify_ica}
#' @examples
#' pca <- prcomp(iris[-5])
#' df_pca <- fortify_dimred(pca$x)
#' @export
fortify_dimred <- function(m_dimred, m_vars = NULL, v_variance = NULL,
  sup_vars = NULL, var_digits = 1) {
  if (is.null(rownames(m_dimred))) {
    m_dimred %<>% `rownames<-`(seq_len(nrow(.)))
  }
  df_dimred <- cbind.data.frame(m_dimred, DIMRED_VARTYPE = 'OBS',
    DIMRED_VARNAME = rownames(m_dimred),
    stringsAsFactors = FALSE)
  if (!is.null(sup_vars)) df_dimred %<>% cbind(sup_vars)

  if (!is.null(m_vars)) {
    df_vars <- data.frame(m_vars, DIMRED_VARTYPE = 'VAR',
      DIMRED_VARNAME = rownames(m_vars))
    df_dimred %<>% plyr::rbind.fill(df_vars)
  }

  if (!is.null(v_variance)) {
    v_variance %<>% stats::setNames(colnames(m_dimred))
    df_variance <- do.call(data.frame, as.list(v_variance, var_digits)) %>%
      cbind(DIMRED_VARTYPE = 'Explained_variance', DIMRED_VARNAME = NA)
    df_dimred %<>% plyr::rbind.fill(df_variance)
  }

  df_dimred
}

#' Get and fortify PCA
#'
#' @param m_data   Input matrix
#' @param ...      Passed to stats::prcomp
#' @param sup_vars Optional supplementary variables
#' @return Fortified dimension reduction 
#' @seealso \link{fortify_dimred}, \link{fortify_ica}
#' @examples
#' df_pca <- fortify_pca(iris[-5])
#' df_pca <- fortify_pca(iris[-5], sup_vars = iris[5])
#' @export
fortify_pca <- function(m_data, ..., sup_vars = NULL) {
  stats::prcomp(m_data, ...) %$%
    fortify_dimred(x, rotation, stats::setNames(sdev ^ 2, colnames(x)),
      sup_vars = sup_vars)
}

#' Get and fortify ICA
#'
#' @param m_data Input matrix
#' @param ...    Passed to fastICA::fastICA
#' @param sup_vars Optional supplementary variables
#' @return Fortified dimension reduction 
#' @seealso \link{fortify_dimred}, \link{fortify_pca}
#' @examples
#' df_ica <- fortify_ica(iris[-5], n.comp = 2)
#' @export
fortify_ica <- function(m_data, ..., sup_vars = NULL) {
  set.seed(0)
  ica <- fastICA::fastICA(m_data, ...)
  ica$S %<>% `colnames<-`(paste0('IC', seq_len(ncol(.))))
  (ica$K %*% ica$W) %>%
    `dimnames<-`(list(colnames(m_data), colnames(ica$S))) %>%
    fortify_dimred(ica$S, ., sup_vars = sup_vars)
}
