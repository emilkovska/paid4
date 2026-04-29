test_that("Exclusion of diseases return the correct LHCE", {
  expect_equal(round(paid("Netherlands", related.diseases = c(14,25))[["lhce"]][1,1,"mean"]),
               216498)
})
