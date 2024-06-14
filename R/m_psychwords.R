#' A dataset containing words by embeddings matrix
#' 
#' @description Data containing Glove embeddings of psychological related
#' words, useful for demonstrating the use of the modified opticskxi pipeline
#' psychkxi.
#' @name m_psychwords
#' @docType data
#' @usage data("m_psychwords")
#' @format A matrix with 800 words in rows and 100 embedding dimensions in
#' columns.
#' @details 
#' The dataset contains 2 main hierarchical clusters (each has subclusters).
#' 
#' @examples
#' data('m_psychwords')
#' df_params = expand.grid(n_xi = 9:10, pts = c(15, 20), dist = 'cosine',
#'   dim_red = 'ICA', n_dimred_comp = c(10, 15))
#' df_kxi = opticskxi:::psych_kxi_ensemble_models(m_psychwords, df_params)
#' 
NULL

