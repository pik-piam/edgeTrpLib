# Helper functions for EDGE transport calculations

R package **edgeTrpLib**, version **1.7.0**

[![CRAN status](https://www.r-pkg.org/badges/version/edgeTrpLib)](https://cran.r-project.org/package=edgeTrpLib)  [![R build status](https://github.com/johannah-pik/edgeTrpLib/workflows/check/badge.svg)](https://github.com/johannah-pik/edgeTrpLib/actions) [![codecov](https://codecov.io/gh/johannah-pik/edgeTrpLib/branch/master/graph/badge.svg)](https://app.codecov.io/gh/johannah-pik/edgeTrpLib) [![r-universe](https://pik-piam.r-universe.dev/badges/edgeTrpLib)](https://pik-piam.r-universe.dev/ui#builds)

## Purpose and Functionality

This package is highly specialized and created solely to not duplicate helper functions.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("edgeTrpLib")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Alois Dirnaichner <dirnaichner@pik-potsdam.de>.

## Citation

To cite package **edgeTrpLib** in publications use:

Dirnaichner A, Rottoli M (2022). _edgeTrpLib: Helper functions for EDGE transport calculations_. R package version 1.7.0.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {edgeTrpLib: Helper functions for EDGE transport calculations},
  author = {Alois Dirnaichner and Marianna Rottoli},
  year = {2022},
  note = {R package version 1.7.0},
}
```
