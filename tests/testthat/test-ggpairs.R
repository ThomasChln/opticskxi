context('ggpairs')

test_ggpairs <- function() {
  data('multishapes')
  ggpairs(multishapes[1:2]) %>%
    expect_is('gg')

  dbscan_shapes <- dbscan::dbscan(multishapes[1:2], eps = 0.15)
  gg_shapes <- multishapes[1:2] %>%
    cbind(group = dbscan::optics(.) %>% opticskxi(5)) %>%
    ggpairs(group = 'group', ellipses = TRUE) %>%
    expect_is('gg')

  data('hla')
  m_hla <- hla[-c(1:2)]
  fortify_ica(m_hla, 2) %>% ggpairs(variables = TRUE, n_vars = 2) %>%
    expect_is('gg')
  expect_is(ggpairs(m_hla, axes = 1:3), 'gtable')
}
test_that('ggpairs', test_ggpairs())
