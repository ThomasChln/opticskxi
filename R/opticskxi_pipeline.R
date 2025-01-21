
#' OPTICS k-Xi models comparison pipeline 
#'
#' Computes OPTICS k-Xi models based on a parameter grid, binds results in a
#' data frame, and computes distance based metrics for each model.
#'
#' @param m_data Data matrix
#' @param df_params Parameter grid for the OPTICS k-Xi function call and
#'                  optional dimension reduction.
#'                  Required columns: n_xi, pts, dist.
#'                  Optonal columns: dim_red, n_dim_red.
#' @param metrics_dist Distance used for metrics, either euclidean or cosine.
#' @param max_size_ratio Maximum size ratio of clusters.
#'                       E.g. for 0.8, if a cluster is larger than 80\% of
#'                       points it will be removed.
#' @param n_min_clusters Minimum number of clusters. Ignored if 0.
#' @param n_cores Number of cores
#'
#' @return Input parameter data frame with with results binded in columns
#'         optics, clusters and metrics.
#'
#' @seealso \link{get_best_kxi}, \link{ggplot_kxi_metrics},
#' \link{gtable_kxi_profiles}
#'
#' @examples
#'
#' data('hla')
#' m_hla <- hla[-c(1:2)] %>% scale
#'
#' df_params_hla <- expand.grid(n_xi = 3:5, pts = c(20, 30),
#'   dist = c('manhattan', 'euclidean'))
#'
#' df_kxi_hla <- opticskxi_pipeline(m_hla, df_params_hla)
#'
#' ggplot_kxi_metrics(df_kxi_hla, n = 8)
#' gtable_kxi_profiles(df_kxi_hla) %>% plot
#'
#' best_kxi_hla <- get_best_kxi(df_kxi_hla, rank = 2)
#' clusters_hla <- best_kxi_hla$clusters
#'
#' fortify_pca(m_hla, sup_vars = data.frame(Clusters = clusters_hla)) %>%
#'   ggpairs('Clusters', ellipses = TRUE, variables = TRUE)
#'
#' @export
opticskxi_pipeline <- function(m_data,
  df_params = expand.grid(n_xi = 1:10, pts = c(20, 30, 40),
                          dist = c('euclidean', 'abscorrelation'),
                          dim_red = c('identity', 'PCA', 'ICA'),
                          n_dimred_comp = c(5, 10, 20)),
  metrics_dist = c('euclidean', 'cosine'), max_size_ratio = 1,
  n_min_clusters = 0, n_cores = 1) {

  if (!all(c('n_xi', 'pts', 'dist') %in% names(df_params))) { 
    stop('Missing required columns in parameter grid.')
  }
  metrics_dist = match.arg(metrics_dist)

  # add identity if no dim_red column
  if (!'dim_red' %in% names(df_params)) {
    df_params %<>% cbind(dim_red = 'identity')
  }

  # dimred
  uniq_names <- c('dim_red', 'n_dimred_comp')
  df_params <- fetch_dimred(m_data, df_params, uniq_names, n_cores)

  # distance
  uniq_names %<>% c('dist')
  df_params %<>% derive_column(uniq_names, c('m_dimred', 'dist'), dist_matrix,
    'm_dist', c('data', 'method'), mc.cores = n_cores)

  # optics
  uniq_names %<>% c('pts')
  df_params %<>% derive_column(uniq_names, c('m_dist', 'pts'), dbscan::optics,
    'optics', c('x', 'minPts'), mc.cores = n_cores)

  # kxi
  df_params <- fetch_opticskxi(df_params, n_cores)

  # max_size_ratio
  if (max_size_ratio < 1) {
    df_params$clusters = df_params[c('optics', 'clusters')] %>%
      t %>% data.frame %>% lapply(rm_huge_clusters, max_size_ratio)
  }

  df_params$n_clusters = lapply(df_params$clusters, function(clusters) {
      length(unique(na.omit(clusters)))
    })

  # n_min_clusters
  if (n_min_clusters > 0) df_params %<>% subset(n_clusters >= n_min_clusters)

  # metrics
  m_dist <- switch(metrics_dist, euclidean = dist, cosine_dist)(m_data) %>%
    as.matrix

  df_params$metrics <- parallel::mclapply(df_params$clusters,
                                          get_kxi_metrics, m_dist,
                                          mc.cores = n_cores)

  df_params
}

#' Get best k-Xi model
#'
#' Select k-Xi clustering model based on a metric and a rank
#'
#' @param df_kxi Data frame returned by opticsxi_pipeline
#' @param metric Metric to choose best model
#' @param rank   Rank(s) of model to choose, ordered by decreasing metric
#' @return df_kxi row with specified metric and rank,
#'         simplified to a list if only one rank selected
#' @seealso \link{opticskxi_pipeline}
#' @export
get_best_kxi <- function(df_kxi, metric = 'avg.silwidth', rank = 1) {
  if (all(unlist(df_kxi$metrics) == 0)) stop('All metrics equal 0')
  metric_value <- as.matrix(as.data.frame(df_kxi$metrics))[metric, ]
  if (all(metric_value == 0)) stop(paste('All', metric, 'values equal 0'))

  df_kxi %<>% `[`(order(metric_value, decreasing = TRUE)[rank], )
  if (length(rank) == 1) {
    df_kxi %<>% as.list
    df_kxi[c('optics', 'clusters', 'metrics')] %<>% lapply(`[[`, 1)
  }

  df_kxi 
}

fetch_dimred <- function(m_data, df_params, uniq_names, n_cores) {
  # remove duplicates, number of components if no dim red, and rownames
  df_params[df_params$dim_red == 'identity', 'n_dimred_comp'] <- NA
  df_params %<>% unique %>% `rownames<-`(NULL)

  # fetch dimension reductions
  df_params$m_data <- list(m_data)
  df_params <- derive_column(df_params, uniq_names,
    c('m_data', 'dim_red', 'n_dimred_comp'), .fetch_dimred, 'm_dimred',
    mc.cores = n_cores)
}

.fetch_dimred <- function(m_data, dim_red, n_dimred_comp) {

  n_dimred_comp <- as.numeric(n_dimred_comp)

  switch(dim_red, identity = m_data,
    PCA = stats::prcomp(m_data)$x[, seq_len(n_dimred_comp)],
    ICA = {
      fastICA::fastICA(m_data, n_dimred_comp)$S %>%
        `colnames<-`(paste0('IC', seq_len(ncol(.))))
    })
}

fetch_opticskxi <- function(df_params, n_cores) {
  # call opticsxi on each row
  df_opt <- data.frame(t(df_params[c('optics', 'n_xi', 'pts')]),
    stringsAsFactors = FALSE)
  df_params$clusters <- parallel::mclapply(df_opt,
    function(params) do.call(opticskxi, params), mc.cores = n_cores)

  df_params
}

# apply fun function to each unique row of dataframe df's columns df_names,
# store in column col_name, and merge to full dataframe df
# ... is passed to parallel::mclapply which calls function fun on unique rows
derive_column <- function(df_prm, uniq_names, df_names, fun, col_name,
  param_names = df_names, ...) {

  uniq_idxs <- which(!duplicated(df_prm[uniq_names]))
  uniq_params <- df_prm[uniq_idxs, df_names]
  uniq_df <- df_prm[uniq_idxs, uniq_names]
  uniq_df[[col_name]] <- parallel::mclapply(
    data.frame(t(uniq_params), stringsAsFactors = FALSE), .derive_column, fun,
    param_names, ...)

  # drop the data object, the first parameter, from the dataframe and merge
  df_prm %<>% `[`(-match(df_names[1], names(.)))
  merge(uniq_df, df_prm, by = uniq_names)
}

.derive_column <- function(params, fun, param_names) {
  do.call(fun, as.list(stats::setNames(params, param_names)))
}


get_kxi_metrics <- function(clusters, m_dist,
  metrics = c('avg.silwidth', 'bw.ratio', 'ch', 'pearsongamma', 'dunn',
    'dunn2', 'entropy', 'widestgap', 'sindex')) {
  if (length(table(clusters)) < 2) return(rep(0, length(metrics)))
  ids <- which(!is.na(clusters))

  m_dist[ids, ids] %>% fpc::cluster.stats(clusters[ids]) %>%
    `[[<-`('bw.ratio', 1 / .$wb.ratio) %>% `[`(metrics) %>% unlist
}
