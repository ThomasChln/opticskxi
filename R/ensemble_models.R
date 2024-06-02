
ensemble_models = function(l_ensemble_metrics, n_models = 4) {
  
  df_metrics = lapply(l_ensemble_metrics, `[[`, 2) %>%
    do.call(rbind.data.frame, .)

  Reduce(paste, df_metrics) %>% table %>% sort %>% tail(n_models) %>% rev %>%
    names %>% strsplit(' ') %>% lapply(setNames, names(df_metrics))
}

# if metrics = NULL, all metrics used
ensemble_metrics = function(n_top = 0, df_params, 
  metrics = NULL,#c('sindex', 'ch', 'dunn', 'dunn2', 'widestgap', 'entropy'),
  metrics_exclude = NULL,#'entropy',
  n_models = 10) {

  m_metrics = sapply(df_params$metrics, identity) %>% apply(1, order)
  
  if (!is.null(metrics_exclude)) {
    m_metrics = m_metrics[, -match(metrics_exclude, colnames(m_metrics))]
  }

  if (!is.null(metrics)) {
    m_metrics = m_metrics[, metrics]
  }

  if (n_top > 0) {
    m_metrics = m_metrics - (nrow(m_metrics) - floor(n_top))
    m_metrics %<>% ifelse(. < 0, 0, .)
  }
  ensemble_idx = m_metrics %>% rowSums %>% order %>% tail(n_models)

  list(m_metrics[ensemble_idx, ], 
       df_params[ensemble_idx, c('n_dimred_comp', 'pts', 'n_xi')])
}

