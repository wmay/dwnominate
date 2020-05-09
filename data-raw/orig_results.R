# Get dwnominate results from a much older version of the package. The old
# version was pretty clunky, but I hadn't yet made any substantial changes to
# the fortran code, so the results serve as a useful reference for unit tests.

# remotes::install_github('cran/oc') # if needed
remotes::install_github('wmay/dwnominate', ref = '195d257')
library(dwnominate)
library(wnominate)
load('../data/nhsen.rda')

# need an integer ID field
leg_names = unique(unlist(lapply(nhsen, function(x) x$legis.data$name)))
nhsen = lapply(nhsen, function(x) {
  x$legis.data$ID = match(x$legis.data$name, leg_names)
  x
})

nhsen_merged = dwnominate:::merge.rollcall(rc_list = nhsen)
nhsen_wnom = wnominate(nhsen_merged, polarity = rep(1, 2))
orig_results = dwnominate(nhsen, id = 'ID', wnom = nhsen_wnom)

# replace the invented ID with name, for convenience later
nhsen_wnom$legislators$ID = leg_names[nhsen_wnom$legislators$ID]
orig_results$legislators$ID = leg_names[orig_results$legislators$ID]

usethis::use_data(nhsen_wnom, orig_results, internal = TRUE, overwrite = T)

# clean up the mess left by the old package
unwanted_files = list.files(pattern = '\\.[^R]+$')
file.remove(unwanted_files)
