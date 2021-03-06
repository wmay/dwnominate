
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dwnominate

[![R-CMD-check](https://github.com/wmay/dwnominate/workflows/R-CMD-check/badge.svg)](https://github.com/wmay/dwnominate/actions)
[![Codecov test
coverage](https://codecov.io/gh/wmay/dwnominate/branch/master/graph/badge.svg)](https://codecov.io/gh/wmay/dwnominate?branch=master)

DW-NOMINATE dynamic roll call scaling in R

## Installation:

On MacOS, `dwnominate` installation requires the gfortran and Clang
compilers provided by CRAN
[here](https://cran.r-project.org/bin/macosx/tools/).

From R, run:

``` r
install.packages('remotes')
remotes::install_github('wmay/dwnominate')
```

## DW-NOMINATE

Running DW-NOMINATE:

``` r
library(dwnominate)
# get a list of `rollcall` objects
data(nhsenate)
results <- dwnominate(nhsenate)
plot(results)
```

![](man/figures/README-example-1.png)<!-- -->

`dwnominate()` takes as its main argument a list of `rollcall` objects
from the
[`pscl`](https://cran.r-project.org/web/packages/pscl/index.html)
package. The results are returned as a `dwnominate` object with
estimates of legislator and roll call coordinates. Get detailed
information about DW-NOMINATE options with `?dwnominate` and
`help(package=dwnominate)`.

## Citing

For citation information, run `citation('dwnominate')`.
