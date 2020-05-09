context("dwnominate results")

data(nhsen)
nhsen_wnom = dwnominate:::nhsen_wnom

test_that("dwnominate results match original results", {
  # given the same data and starting estimates, dwnominate should generate
  # results very close to the original results from a very old version of
  # dwnominate
  curr_legs = dwnominate(nhsen, 'name', nhsen_wnom)$legislators
  orig_legs = dwnominate:::orig_results$legislators
  cor1D = cor(orig_legs$coord1D, curr_legs$coord1D)
  cor2D = cor(orig_legs$coord2D, curr_legs$coord2D)
  expect_gt(abs(cor1D), .999)
  expect_gt(abs(cor2D), .99)
})

test_that("dwnominate constant model matches wnominate", {
  # The constant model is basically the same as wnominate, just computed
  # differently, so the results should be practically identical. I'm cheating by
  # using the wnominate results as starting estimates for dwnominate, but I
  # think this is still an informative test. (common_space doesn't work well
  # with the second dimension of the New Hampshire data, so wnominate starting
  # estimates work much better.)
  dw0 = dwnominate(nhsen, 'name', nhsen_wnom, model = 0)
  dw0_legs = subset(dw0$legislators, !duplicated(ID))
  wnom_legs = nhsen_wnom$legislators
  dw0_legs = dw0_legs[match(dw0_legs$ID, wnom_legs$ID), ]
  cor1D = cor(dw0_legs$coord1D, wnom_legs$coord1D, use = 'complete.obs')
  cor2D = cor(dw0_legs$coord2D, wnom_legs$coord2D, use = 'complete.obs')
  expect_gt(abs(cor1D), .99)
  # the second dimension is a bit iffy
  expect_gt(abs(cor2D), .8)
})
