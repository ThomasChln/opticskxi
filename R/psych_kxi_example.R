
#' Example pipeline for ensemble models
#'
#' Example pipeline for ensemble models on mental health related natural
#' language processing
#'
#' @param m_data Data matrix Data frame returned by optics
#' @param n_models Number of best models to return
#' @param metrics Names of metrics to use. Any of those computed by
#'                opticskxi_pipeline, e.g. 'sindex', 'ch', 'dunn',
#'                'dunn2', 'widestgap', 'entropy' etc.
#'                NULL for all (8).
#' @param metrics_exclude Names of metrics to exclude.
#'                        Typically used with metrics = NULL. E.g. 'entropy'.
#' @param model_subsample Ratios of best models to consider.
#' @param n_models_subsample Number of best models when subsampling.
#' @param ...    Passed to function psych_kxi_pipeline
#'
#' @return Input parameter data frame with with results binded in columns
#'         optics, clusters and metrics.
#'         Subsetted to best models according to ensemble metrics.
#'
#' @examples
#' data('m_psychwords')
#' m_psychwords = m_psychwords[1:200, 1:20]
#'
#' df_params = expand.grid(n_xi = 4:5, pts = c(5, 10), dist = 'cosine',
#'                         dim_red = 'ICA', n_dimred_comp = 5)
#'
#' df_kxi = psych_kxi_ensemble_models(m_psychwords, df_params,
#'                                    n_min_clusters = 2,
#'                                    n_models = 4,
#'                                    metrics = c('avg.silwidth', 'dunn'),
#'                                    model_subsample = c(0.4, 0.6),
#'                                    n_models_subsample = 4)
#'
#' @export
psych_kxi_ensemble_models = function(m_data, ..., n_models = 4, metrics = NULL,
                                     metrics_exclude = NULL,
                                     model_subsample = c(0.1, 0.2, 0.5),
                                     n_models_subsample = 10) {

  df_params = psych_kxi_pipeline(m_data, ...)
  l_metrics = lapply(model_subsample * nrow(df_params), ensemble_metrics,
                     df_params, metrics, metrics_exclude, n_models_subsample)

  best_models = ensemble_models(l_metrics, n_models)

  df_best_params = data.frame()

  for (best_model in best_models) {

    best_params = as.list(best_model) %>% {
      subset(df_params,
             n_dimred_comp == .$n_dimred_comp & pts == .$pts & n_xi == .$n_xi)
    }
    
    df_best_params %<>% rbind(best_params)
  }

  df_best_params
}

#' Example pipeline for mental health natural language processing
#'
#' Removes too large clusters and models with less than a minimum number of
#' clusters.
#'
#' @param m_data    Data matrix
#' @param df_params Parameter grid for the OPTICS k-Xi function call and
#'                  optional dimension reduction.
#'                  Required columns: n_xi, pts, dist.
#'                  Optonal columns: dim_red, n_dim_red.
#' @param max_size_ratio Maximum size ratio of clusters
#' @param n_min_clusters Minimum number of clusters
#' @param n_cores   Number of cores
#'
#' @return Input parameter data frame with with results binded in columns
#'         optics, clusters and metrics.
#'
#' @export
psych_kxi_pipeline = function(m_data,
    df_params = expand.grid(n_xi = 8:15, pts = c(15, 20, 25, 30),
                            dist = "cosine", dim_red = "ICA",
                            n_dimred_comp = c(10, 15, 20, 25)),
    max_size_ratio = 0.15, n_min_clusters = 5, n_cores = 1) {

  message(n_cores)
  uniq_names <- c("dim_red", "n_dimred_comp")
  df_params <- fetch_dimred(m_data, df_params, uniq_names, 
      n_cores)
  uniq_names %<>% c("dist")

  df_params %<>% derive_column(uniq_names, c("m_dimred", "dist"), 
      cosine_dist, "m_dist", c("x", "method"), mc.cores = n_cores)

  uniq_names %<>% c("pts")
  df_params %<>% derive_column(uniq_names, c("m_dist", "pts"), 
      dbscan::optics, "optics", c("x", "minPts"), mc.cores = n_cores)

  m_dist <- cosine_dist(m_data) %>% as.matrix

  df_params <- fetch_opticskxi(df_params, n_cores)

  df_params$clusters = df_params[c('optics', 'clusters')] %>%
      t %>% data.frame %>% lapply(rm_huge_clusters, max_size_ratio)

  df_params$n_clusters = lapply(df_params$clusters, function(clusters) {
      length(unique(na.omit(clusters)))
    })
  df_params %<>% subset(n_clusters >= n_min_clusters)

  df_params$metrics <- parallel::mclapply(df_params$clusters, 
      get_kxi_metrics, m_dist, mc.cores = n_cores)

  df_params
}

cosine_dist = function(x, method) as.dist(1 - text2vec::sim2(x))
