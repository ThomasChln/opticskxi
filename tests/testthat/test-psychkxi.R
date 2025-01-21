
data('m_psych_embeds')


test_psych_kxi_ensemble_models <- function() {

  # these parameters make more sense

  # df_params = expand.grid(n_xi = 9:10, pts = c(15, 20), dist = 'cosine',
  #                         dim_red = 'ICA', n_dimred_comp = c(10, 15))
 
  # df_kxi = psych_kxi_ensemble_models(m_psychwords, df_params,
  #                                    n_models = 4,
  #                                    model_subsample = c(0.4, 0.6),
  #                                    n_models_subsample = 8)

  df_params = expand.grid(n_xi = 4:5, pts = c(5, 10), dist = 'cosine',
                          dim_red = 'ICA', n_dimred_comp = 5)

  df_kxi = psych_kxi_ensemble_models(m_psych_embeds, df_params,
                                     n_min_clusters = 2,
                                     n_models = 4,
                                     metrics = c('avg.silwidth', 'dunn'),
                                     model_subsample = c(0.4, 0.6),
                                     n_models_subsample = 4)

  expect_is(gtable_kxi_profiles(df_kxi), 'gtable')
  best_clusters = df_kxi$clusters[[1]]

  expect_equal(length(best_clusters), nrow(m_psych_embeds))
  expect_true(!all(is.na(best_clusters)))
  expect_true(any(is.na(best_clusters)))


  # results very diff when integrating, need to confirm if normal seed variability

  df_kxi = psych_kxi_pipeline_old(m_psych_embeds)

  df_kxi_test = opticskxi_pipeline(m_psych_embeds,
                                   data.frame(n_xi = 8:11, pts = 15,
                                              dist = 'cosine', dim_red = 'ICA',
                                              n_dimred_comp = 15),
                                   metrics_dist = 'cosine',                          
                                   max_size_ratio = 0.15, n_min_clusters = 5)
}
test_that('psych_kxi_ensemble_models', test_psych_kxi_ensemble_models())



