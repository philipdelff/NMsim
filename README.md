# NMexec
Start Nonmem jobs from R

## Install
Easiest way to install NMexec is using the remotes package to install with R:
library(remotes)
install_github("philipdelff/NMexec")

If you want to use the subproblems method to simulate variablility,
you need the developer version of NMdata. This is until we get
NMdata 0.0.14 on CRAN. If you are not planning to use subproblems,
the NMdata version on CRAN or MPN is fine.

If you plan to use subproblems:
install_github("philipdelff/NMdata")
library("NMexec")
