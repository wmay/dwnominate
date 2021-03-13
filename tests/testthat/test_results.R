context("dwnominate results")

data(nhsenate)
# precomputed results
nhsen_wnom = dwnominate:::nhsen_wnom
orig_legs = dwnominate:::orig_results$legislators

test_that("dwnominate results match original results", {
  # Given the same data and starting estimates, dwnominate should generate
  # results very close to the results from the original, command line version of
  # DW-NOMINATE. The only practical difference is that IMSL and EISPACK routines
  # have been replaced with LAPACK, leading to slightly different results.
  # The original version's input files round the starting estimates to 3 digits,
  # so for comparison these should also be rounded.
  nhsen_wnom_rounded = nhsen_wnom
  nhsen_wnom_rounded$legislators = within(nhsen_wnom_rounded$legislators, {
    coord1D = round(coord1D, 3)
    coord2D = round(coord2D, 3)
  })
  capture.output({
    res <- suppressMessages(dwnominate(nhsenate, 'name', nhsen_wnom_rounded))
  })
  curr_legs = res$legislators
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
  # Make an ID that matches the wnominate results
  wnom_legs = nhsen_wnom$legislators
  nhsenate2 = lapply(nhsenate, function(x) {
    x$legis.data$ID = wnom_legs$ID[match(x$legis.data$name, wnom_legs$name)]
    x
  })
  capture.output({
    dw0 <- suppressMessages(dwnominate(nhsenate2, 'ID', nhsen_wnom, model = 0))
  })
  dw0_legs = subset(dw0$legislators, !duplicated(ID))
  dw0_legs = dw0_legs[match(dw0_legs$ID, wnom_legs$ID), ]
  cor1D = cor(dw0_legs$coord1D, wnom_legs$coord1D, use = 'complete.obs')
  cor2D = cor(dw0_legs$coord2D, wnom_legs$coord2D, use = 'complete.obs')
  expect_gt(abs(cor1D), .99)
  # the second dimension is a bit iffy
  expect_gt(abs(cor2D), .8)
})
