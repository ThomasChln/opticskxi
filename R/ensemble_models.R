
#' Select best models based on ensemble metrics
#'
#' Call ensemble_metrics with varying numbers of rank thresholds to
#' consider and sum up the ranks from those calls.
#'
#' @param df_kxi Output of opticskxi_pipeline function.
#'               Dataframe with models' parameters and OPTICS k-Xi results
#' @param n_models Number of best models to return
#' @param metrics Names of metrics to use. Any of those computed by
#'                opticskxi_pipeline, e.g. 'sindex', 'ch', 'dunn',
#'                'dunn2', 'widestgap', 'entropy' etc.
#'                NULL for all (8).
#' @param metrics_exclude Names of metrics to exclude.
#'                        Typically used with metrics = NULL. E.g. 'entropy'.
#' @param model_subsample Ratios of best models to consider.
#' @param n_models_subsample Number of best models when subsampling.
#'
#' @return Input object df_kxi subsetted to best models according to ensemble
#'         metrics.
#'
#' @examples
#' data('m_psych_embeds')
#' m_psych_embeds = m_psych_embeds[1:200, 1:20]
#'
#' df_params = expand.grid(n_xi = 4:5, pts = c(5, 10), dist = 'cosine',
#'                         dim_red = 'ICA', n_dimred_comp = 5)
#'
#' df_kxi = opticskxi_pipeline(m_psych_embeds, df_params,
#'                             metrics_dist = 'cosine',
#'                             n_min_clusters = 2, n_cores = 1,
#'                             metrics = c('avg.silwidth', 'dunn'))
#'
#' df_kxi = ensemble_models(df_kxi, n_models = 4,
#'                          model_subsample = c(0.4, 0.6),
#'                          n_models_subsample = 4)
#'
#' @export
ensemble_models = function(df_kxi, n_models = 4,
                           metrics = NULL, metrics_exclude = NULL, 
                           model_subsample = c(0.1, 0.2, 0.5),
                           n_models_subsample = 10) {

  l_metrics = lapply(model_subsample * nrow(df_kxi), ensemble_metrics, 
                     df_kxi, metrics, metrics_exclude, n_models_subsample)

  df_best_params = ensemble_metrics_bootstrap(l_metrics, n_models)

  df_models = merge(df_kxi, df_best_params,
                         by = c('n_xi', 'pts', 'dist', 'dim_red',
                                'n_dimred_comp'))


  ord_idxs = df_models %$% order(n_occur, n_xi, pts, n_dimred_comp)

  df_models[ord_idxs, ]
}

#' Select models based on ensemble metrics
#'
#' Typically we will call ensemble_metrics with varying numbers of ranks to
#' consider and this function will sum up the ranks from those calls.
#'
#' @param l_ensemble_metrics Output of function ensemble_metrics
#' @param n_models Number of best models to return
#'
#' @return List of parameters of best models
#'
ensemble_metrics_bootstrap = function(l_ensemble_metrics, n_models = 4) {
  
  df_metrics = lapply(l_ensemble_metrics, `[[`, 2) %>%
    do.call(rbind.data.frame, .)

  models = Reduce(paste, df_metrics) %>% table %>%
    sort(decreasing = TRUE) %>%
    head(n_models)
 
  l_models = names(models) %>% strsplit(' ')

  df_models = do.call(rbind.data.frame, l_models) %>%
    setNames(names(df_metrics)) %>% cbind(n_occur = as.vector(models))
}


#' Compute ensemble metrics
#'
#' Use models' rankings over several metrics to select best model.
#' Several approaches can be taken to sum the models' rankings, and instead of
#' summing the ranks of all models over all metrics, we prefer to rank only the
#' top models for each metrics, and set 0 to all other. This behavior is
#' controlled by the n_top parameter. In a second step, we sum the ranks and
#' return only the top models, and this is controlled by the n_models
#' parameter.
#' The output is a list of the rankings matrix, for quality control purposes,
#' and the selected models' parameters data frame, which is used by the
#' ensemble_models function.
#'
#' @param n_top Threshold of number of models to rank
#' @param df_params Output of opticskxi_pipeline
#' @param metrics Names of metrics to use. Any of those computed by
#'                opticskxi_pipeline, e.g. 'sindex', 'ch', 'dunn',
#'                'dunn2', 'widestgap', 'entropy' etc.
#'                NULL for all (8).
#' @param metrics_exclude Names of metrics to exclude.
#'                        Typically used with metrics = NULL. E.g. 'entropy'.
#' @param n_models Number of best models to return
#'
#' @return List of metrics' rankings matrix and best models' parameters data
#'         frame.
#'
#' @export
ensemble_metrics = function(n_top = 0, df_params, metrics = NULL,
                            metrics_exclude = NULL, n_models = 10) {

  m_metrics = sapply(df_params$metrics, identity) %>% apply(1, order)
  
  if (!is.null(metrics_exclude)) {
    m_metrics = m_metrics[, -match(metrics_exclude, colnames(m_metrics))]
  }

  if (!is.null(metrics)) m_metrics = m_metrics[, metrics]

  if (n_top > 0) {
    m_metrics = m_metrics - (nrow(m_metrics) - floor(n_top))
    m_metrics %<>% ifelse(. < 0, 0, .)
  }

  ensemble_idx = m_metrics %>% rowSums %>% order(decreasing = TRUE) %>%
    head(n_models)

  list(m_metrics[ensemble_idx, ], 
       df_params[ensemble_idx,
                 c('n_xi', 'pts', 'dist', 'dim_red', 'n_dimred_comp')])
}

