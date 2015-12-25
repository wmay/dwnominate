# dwnominate
A convenient R interface to Keith Poole's DW-NOMINATE fortran program

To install:

```
library(devtools)
install_github("wmay/dwnominate")
```

To run DW-NOMINATE:

```
dwnominate(rc_list, wnom_list = NA)
```

- rc_list: a list of `rollcall` objects from the `pscl` package
- wnom_list (optional): a list of corresponding W-NOMINATE results (class `nomObject`)  from the `wnominate` package. If no W-NOMINATE results are provided, W-NOMINATE will be run to get starting values for DW-NOMINATE.
