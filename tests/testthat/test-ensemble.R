
test_ensemble_models <- function() {
 
  data('m_psych_embeds')                                                        
                                                                                
  df_params = expand.grid(n_xi = 8:15, pts = c(15, 20, 25, 30),                 
                          dist = "cosine", dim_red = "ICA",                     
                          n_dimred_comp = c(10, 15, 20, 25))                    
                                                                                
  df_kxi = opticskxi_pipeline(m_psych_embeds, df_params,                        
                              metrics_dist = 'cosine',                          
                              max_size_ratio = 0.15, n_min_clusters = 5)        


  gt = (gtable_kxi_profiles(df_kxi))
  expect_true(TRUE)

  gt = (gtable_kxi_profiles(df_kxi, metric = 'bw.ratio'))
  expect_true(TRUE)

  gt = (gtable_kxi_profiles(df_kxi, metric = 'dunn'))
  expect_true(TRUE)

  gg = (ggplot_kxi_metrics(df_kxi, n = 15,                                       
                           metric = c("avg.silwidth", "bw.ratio", "dunn")))      
  expect_true(TRUE)

  ensemble_obj = ensemble_metrics(n_top = 50, df_params = df_kxi)
  expect_true(TRUE)

  df_ensemble_kxi = ensemble_models(df_kxi, n_models = 4,
                                    model_subsample = c(0.1, 0.2, 0.5))
  expect_true(TRUE)

  gt = (gtable_kxi_profiles(df_ensemble_kxi))
  expect_true(TRUE)


  df_ica = fortify_ica(m_psych_embeds, n.comp = 4,
                       sup_vars = data.frame(Clusters = df_ensemble_kxi$clusters[[1]]))

  gg = ggpairs(df_ica, 'Clusters', ellipses = TRUE, axes = 1:4) %>% grid::grid.draw()
  expect_true(TRUE)


  best_kxi <- get_best_kxi(df_kxi, metric = 'dunn')

  gg = fortify_ica(m_psych_embeds, n.comp = 4,
              sup_vars = data.frame(Clusters = best_kxi$clusters)) %>%
    ggpairs('Clusters', ellipses = TRUE, axes = 1:4) %>% grid::grid.draw()
  expect_true(TRUE)

}
test_that('ensemble_models', test_ensemble_models())


