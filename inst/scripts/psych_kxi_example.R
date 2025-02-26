
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
