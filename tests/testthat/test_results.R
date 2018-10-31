context("DW-NOMINATE results")

data(senate)
load('senate_results.rda')

test_that("DW-NOMINATE results matches known correct results", {
  r2 = dwnominate(senate, start=r1$start)
  max1D_diff = max(abs(r2$legislators$coord1D - r1$legislators$coord1D))
  max2D_diff = max(abs(r2$legislators$coord2D - r1$legislators$coord2D))
  expect_equal(max1D_diff, 0)
  expect_equal(max2D_diff, 0)
})
