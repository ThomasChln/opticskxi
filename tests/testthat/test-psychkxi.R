
data('m_psych_embeds')


test_psych_kxi_ensemble_models <- function() {

  # these parameters make more sense

  # df_params = expand.grid(n_xi = 9:10, pts = c(15, 20), dist = 'cosine',
  #                         dim_red = 'ICA', n_dimred_comp = c(10, 15))
 
  # df_kxi = psych_kxi_ensemble_models(m_psychwords, df_params,
  #                                    n_models = 4,
  #                                    model_subsample = c(0.4, 0.6),
  #                                    n_models_subsample = 8)

  m_psych_embeds = m_psych_embeds[1:200, 1:20]
 
  df_params = expand.grid(n_xi = 4:5, pts = c(5, 10), dist = 'cosine',
                          dim_red = 'ICA', n_dimred_comp = 5)
 
  df_kxi = opticskxi_pipeline(m_psych_embeds, df_params,
                              metrics_dist = 'cosine',
                              n_min_clusters = 2, n_cores = 1,
                              metrics = c('avg.silwidth', 'dunn'))
 
  df_kxi = ensemble_models(df_kxi, n_models = 4,
                           model_subsample = c(0.4, 0.6),
                           n_models_subsample = 4)

  expect_is(gtable_kxi_profiles(df_kxi), 'gtable')
  best_clusters = df_kxi$clusters[[1]]

  expect_equal(length(best_clusters), nrow(m_psych_embeds))
  expect_true(!all(is.na(best_clusters)))
  expect_true(any(is.na(best_clusters)))

}
test_that('psych_kxi_ensemble_models', test_psych_kxi_ensemble_models())



