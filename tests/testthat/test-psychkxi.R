
data('m_psychwords')
df_kxi = opticskxi:::psych_kxi_ensemble_models(m_psychwords)

test_psych_kxi_ensemble_models <- function() {
    expect_is(opticskxi::gtable_kxi_profiles(df_kxi), 'gtable')
    best_clusters = df_kxi$clusters[[1]]
    expect_equal(length(best_clusters), nrow(m_psychwords))
    expect_true(!all(is.na(best_clusters)))
    expect_true(any(is.na(best_clusters)))
}
test_that('psych_kxi_ensemble_models', test_psych_kxi_ensemble_models())
