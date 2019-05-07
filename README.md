# edgeTrpLib 

is a R package with core functions for the EDGE Transport model. It was created
to avoid code duplication for functions used both in the generation of data for
the calibration of REMIND (demand pathways) and in the iterative GAMS solving
procedure of REMIND (i.e., the `EDGE_transport.R` script).

## Installation

The installation requires the use of a deploy token:

```
require(git2r)
require(devtools)
cred <- cred_user_pass(username="gitlab+deploy-token-2", password="17g9jJM8s5TJ67LswyHW")
install_git("https://gitlab.pik-potsdam.de/REMIND/edgetrplib.git",
            credentials = cred, dependencies = F)
```