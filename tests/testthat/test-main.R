test_that("Exclusion of diseases return the correct LHCE", {
  expect_equal(round(paid("Netherlands", related.diseases = 9:26)[["lhce"]][1,1,"mean"]),
               149307)
})
