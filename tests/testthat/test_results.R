context("DW-NOMINATE results")

data(senate)
load('senate_results.rda')

test_that("DW-NOMINATE results closely correlate with known correct results", {
  r2 = dwnominate(senate, start=r1$start)
  cor1D = cor(r1$legislators$coord1D, r2$legislators$coord1D)
  cor2D = cor(r1$legislators$coord2D, r2$legislators$coord2D)
  expect_gt(cor1D, .999)
  expect_gt(cor2D, .95)
})
