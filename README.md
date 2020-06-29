# edgeTrpLib 

is a R package with core functions for the EDGE Transport model. It was created
to avoid code duplication for functions used both in the generation of data for
the calibration of REMIND (demand pathways) and in the iterative GAMS solving
procedure of REMIND (i.e., the `EDGE_transport.R` script).

## Installation

The package can be installed from github via
```
devtools::install_github("https://github.com/pik-piam/edgeTrpLib.git", "master")
```
