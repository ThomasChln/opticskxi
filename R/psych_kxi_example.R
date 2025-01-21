
# Example pipeline for ensemble models on mental health related natural
# language processing
psych_kxi_ensemble_models = function(m_data, df_params, n_min_clusters, ...) {

  df_kxi = psych_kxi_pipeline(m_data, df_params, n_min_clusters = n_min_clusters)
  df_kxi = ensemble_models(df_kxi, ...)
}

# Example pipeline for mental health natural language processing
psych_kxi_pipeline = function(m_data,
    df_params = expand.grid(n_xi = 8:15, pts = c(15, 20, 25, 30),
                            dist = "cosine", dim_red = "ICA",
                            n_dimred_comp = c(10, 15, 20, 25)),
    max_size_ratio = 0.15, n_min_clusters = 5, n_cores = 10) {

  opticskxi_pipeline(m_data, df_params, 'cosine', max_size_ratio,
                     n_min_clusters, n_cores)
}

psych_kxi_pipeline_old = function(m_data,
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

