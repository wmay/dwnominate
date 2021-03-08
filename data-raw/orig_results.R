# Get DW-NOMINATE results from the original fortran code, to use for regression
# testing

# I can't legally distribute the original file because it contains proprietary
# IMSL code, so download it instead
if (!file.exists('DW-NOMINATE/DW-NOMINATE.FOR')) {
  dir.create('DW-NOMINATE')
  setwd('DW-NOMINATE')
  download.file('https://legacy.voteview.com/k7ftp/wf1/DW-NOMINATE.FOR',
                'DW-NOMINATE.FOR')
  # Comment `gettim` lines. gfortran can't compile them, and they only record
  # the time anyway
  system("sed -e '/gettim/ s/^/C/' -i DW-NOMINATE.FOR")
  setwd('..')
}
if (!file.exists('DW-NOMINATE/a.out')) {
  dir.create('DW-NOMINATE')
  setwd('DW-NOMINATE')
  # compile the file with gfortran
  system("gfortran DW-NOMINATE.FOR -std=legacy")
  setwd('..')
}

library(dwnominate)
library(wnominate)
source('write_files.R')
data(nhsenate)

# need an integer ID field
leg_names = unique(unlist(lapply(nhsenate, function(x) x$legis.data$name)))
nhsen = lapply(nhsenate, function(x) {
  x$legis.data$ID = match(x$legis.data$name, leg_names)
  x
})

nhsen_merged = dwnominate:::merge.rollcall(rc_list = nhsen)
nhsen_wnom = wnominate(nhsen_merged, polarity = c(1, 1))
# orig_results = dwnominate(nhsen, id = 'ID', wnom = nhsen_wnom)

setwd('DW-NOMINATE')
# write files
write_input_files(nhsen, nhsen_wnom, sessions = c(1, length(nhsen)), dims = 2,
                  model = 1, niter = c(1, 5), beta = 5.9539, w = 0.3463,
                  lid = 'ID')

# run dwnominate
system("./a.out")

# read results
party_dict = setNames(0, 'party')
nlegs = sum(sapply(nhsen, function(x) x$n))
nrcs = sum(sapply(nhsen, function(x) x$m))
orig_results = read_output_files(party_dict, dims = 2, iters = c(1, 5),
                                 nunlegs = nlegs, nunrcs = nrcs)

setwd('..')

# save both the wnominate starting values, and the original results
usethis::use_data(nhsen_wnom, orig_results, internal = T, overwrite = T)

# clean up
unwanted_files = setdiff(list.files('DW-NOMINATE', full.names = T),
                         'DW-NOMINATE/DW-NOMINATE.FOR')
file.remove(unwanted_files)
