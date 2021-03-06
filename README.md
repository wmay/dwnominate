# dwnominate

[![R-CMD-check](https://github.com/wmay/dwnominate/workflows/R-CMD-check/badge.svg)](https://github.com/wmay/dwnominate/actions)
[![Codecov test coverage](https://codecov.io/gh/wmay/dwnominate/branch/master/graph/badge.svg)](https://codecov.io/gh/wmay/dwnominate?branch=master)

DW-NOMINATE dynamic roll call scaling in R

## Installation:

On MacOS, `dwnominate` installation requires the gfortran and Clang compilers provided by CRAN [here](https://cran.r-project.org/bin/macosx/tools/).

From R, run:

```R
install.packages('remotes')
remotes::install_github('wmay/dwnominate')
```

## DW-NOMINATE
Running DW-NOMINATE:

```R
library(dwnominate)
# get a list of `rollcall` objects
data(nhsenate)
results <- dwnominate(nhsenate)
plot(results)
```
![image](https://user-images.githubusercontent.com/4205859/28497526-9f421d4c-6f57-11e7-988d-0c4226eba992.png)

`dwnominate()` takes as its main argument a list of `rollcall` objects from the [`pscl`](https://cran.r-project.org/web/packages/pscl/index.html) package. The results are returned as a `dwnominate` object with estimates of legislator and roll call coordinates. Get detailed information about DW-NOMINATE options with `?dwnominate` and `help(package=dwnominate)`.

## Citing

For citation information, run `citation('dwnominate')`.
