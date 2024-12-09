
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
#' @export
ensemble_models = function(l_ensemble_metrics, n_models = 4) {
  
  df_metrics = lapply(l_ensemble_metrics, `[[`, 2) %>%
    do.call(rbind.data.frame, .)

  Reduce(paste, df_metrics) %>% table %>% sort %>% tail(n_models) %>% rev %>%
    names %>% strsplit(' ') %>% lapply(setNames, names(df_metrics))
}


#' Compute ensemble metrics
#'
#' Use models' rankings over several metrics to select best model.
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
#' @return List of metrics matrix and df_params subsetted to best models
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

  ensemble_idx = m_metrics %>% rowSums %>% order %>% tail(n_models)

  list(m_metrics[ensemble_idx, ], 
       df_params[ensemble_idx, c('n_dimred_comp', 'pts', 'n_xi')])
}

