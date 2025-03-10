test_cosine_simi = function() {
  data('m_embeds')

  # vector to vector
  myres = opticskxi::cosine_simi(m_embeds[1, , drop = FALSE],
                                 m_embeds[2, , drop = FALSE])
  thebench = text2vec::psim2(m_embeds[1, , drop = FALSE],
                             m_embeds[2, , drop = FALSE])
  expect_equal(as.numeric(myres), as.numeric(thebench))

  # vector to matrix
  thebench = text2vec::psim2(rbind(m_embeds[1, , drop = FALSE],
                                   m_embeds[2, , drop = FALSE]),
                             m_embeds[3:4, , drop = FALSE])
  myres = opticskxi::cosine_simi(m_embeds[1, , drop = FALSE], m_embeds[3:4, ])
  expect_equal(as.numeric(myres[1]), as.numeric(thebench[1]))

  myres = opticskxi::cosine_simi(m_embeds[2, , drop = FALSE], m_embeds[3:4, ])
  expect_equal(as.numeric(myres[2]), as.numeric(thebench[2]))
}

# we're dropping this test because we're dropping text2vec::psim2
#test_that('cosine_simi', test_cosine_simi())

