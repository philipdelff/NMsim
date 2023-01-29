# NMexec
`NMexec` is an R package that can start Nonmem jobs from within
R. Most notably, it can simulate Nonmem models (using the `NMsim`
function) based on just a simulation data set and a path to a
estimation control stream. `NMexec` needs to be able to run `Nonmem`
(and currently also `PSN`) to work.

## Install
Easiest way to install NMexec is using the remotes package to install with R:

    library(remotes)
    install_github("philipdelff/NMexec")

`NMexec` makes extensive use of functionality provided by the `NMdata`
package. For most recent features of `NMexec` to work, make sure to at
least keep `NMdata` updated to latest CRAN or MPN realease. In case
you need a very recent feature, you may need to install `NMdata` from
github too:

    install_github("philipdelff/NMdata")
    install_github("philipdelff/NMexec")
    library("NMexec")

## Simulate a Nonmem model from R
With a simulation data (`simdat`) and an estimated Nonmem run (stored in `models/run1.mod`) at hand, it is this simple:

    simres <- NMsim("models/run1.mod",simdat,
	dir.sim="simulations",suffix.sim="example",seed=123)
	
`NMexec` includes functions to very easily create simulation datasets. 

Please see see examples here:
https://github.com/philipdelff/NMexec/tree/main/inst/examples/R/NMexec_examples.R
	
## Run Nonmem from R
There are several other packages out there that can do this, and
`NMexec` may not be your best choice if this feature is all you are
looking for. However, running `Nonmem` using `NMexec` does provide one
important advantage in that it saves the input data together with the
Nonmem control streams. This ensures that output data can be merged
with input data as it went into the model, even if the input data file
should be modified or lost.

`NMexec` will submit model runs to a cluster by default. This can be
switched off for running Nonmem locally.


`NMsim` will then 
- Save `simdat` 
create a simulation Nonmem run based on `models/run1.mod`, store it in `simulations/NMsim_1_example.mod`,
- Run Nonmem on the created 
