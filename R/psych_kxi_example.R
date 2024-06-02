
psych_kxi_ensemble_models = function(m_data) {

  df_params = psych_kxi_pipeline(m_data)
  l_metrics = lapply(c(0.1, 0.2, 0.5) * nrow(df_params), ensemble_metrics,
                     df_params)

  best_models = ensemble_models(l_metrics)

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



psych_kxi_pipeline = function(m_data,
    df_params = expand.grid(n_xi = 8:15, pts = c(15, 20, 25, 30),
                            dist = "cosine", dim_red = "ICA",
                            n_dimred_comp = c(10, 15, 20, 25)),
    max_size_ratio = 0.15, n_min_clusters = 5, n_cores = 3) {

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

  df_params$clusters = df_params[c('optics', 'clusters')] %>% t %>% data.frame %>%
      lapply(rm_huge_clusters, max_size_ratio)

  df_params$n_clusters = lapply(df_params$clusters, function(clusters) {
      length(unique(na.omit(clusters)))
    })
  df_params %<>% subset(n_clusters >= n_min_clusters)

  df_params$metrics <- parallel::mclapply(df_params$clusters, 
      get_kxi_metrics, m_dist, mc.cores = n_cores)

  df_params
}

cosine_dist = function(x, method) as.dist(1 - text2vec::sim2(x))
