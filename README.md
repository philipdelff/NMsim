# NMsim
`NMsim` is an R package that can start Nonmem jobs from within
R. Most notably, it can simulate Nonmem models (using the `NMsim`
function) based on just a simulation data set and a path to a
estimation control stream. `NMsim` needs to be able to run `Nonmem`
(and currently also `PSN`) to work.

## Install
Easiest way to install `NMsim` is using the `remotes` package to install with R:

    library(remotes)
    install_github("philipdelff/NMsim")

`NMsim` makes extensive use of functionality provided by the `NMdata`
package. For most recent features of `NMsim` to work, make sure to at
least keep `NMdata` updated to latest CRAN or MPN realease. In case
you need a very recent feature, you may need to install `NMdata` from
github too:

    install_github("philipdelff/NMdata")
    install_github("philipdelff/NMsim")
    library("NMsim")

## Simulate a Nonmem model from R
With a simulation data set (`simdat`) and an estimated Nonmem run
(stored in `models/run1.mod`) at hand, it is this simple:

    simres <- NMsim("models/run1.mod",simdat,
	dir.sim="simulations",suffix.sim="example",seed=123)

`NMsim` will then 
- Save `simdat` 
create a simulation Nonmem run based on `models/run1.mod`, store it in `simulations/NMsim_1_example.mod`,
- Run Nonmem on the created 
- Read output datasets from simulation run and merge them with simulation input data
- Return the combined dataset

`NMsim` includes functions to very easily create simulation datasets. 

Please see examples here:
https://github.com/philipdelff/NMsim/tree/main/inst/examples/R/NMexec_examples.R
	
## Run Nonmem from R
There are several other packages out there that can do this, and
`NMsim` may not be your best choice if this feature is all you are
looking for. However, running `Nonmem` using the `NMexec` function
provided by `NMsim` has one important advantage in that it saves the
input data together with the Nonmem control streams. This ensures that
output data can be merged with input data as it went into the model,
even if the input data file should be modified or lost.

- Saves input data with Nonmem model
- Provides a simple R command for submission of Nonmem jobs
- Optionally handles cluster configuration
- Saves the xml file by default

`NMexec` will submit model runs to a cluster by default. This can be
switched off for running Nonmem locally. Please notice the jobs are
submitted to a cluster in a very specific way using `PSN`. If your
setup is different, this is for now not supported. Please use
`NMexec(sge=FALSE)` in that case (which may not be desirable). Notice
that simulations are not done on a cluster by default so you may still
be able to use `NMsim`.
