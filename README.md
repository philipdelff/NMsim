# NMsim
`NMsim` is an R package that can modify and start Nonmem jobs from
within R. Most notably, it can simulate Nonmem models (using the
`NMsim` function) based on just a simulation data set and a path to a
estimation control stream. It will also retrive and combine output tables with input data once Nonmem has finished and return the results to R. 

## Install
Easiest way to install `NMsim` is using the `remotes` package to install with R:

    library(remotes)
    install_github("philipdelff/NMsim")

`NMsim` makes extensive use of functionality provided by the `NMdata`
package. For most recent features of `NMsim` to work, make sure to at
least keep `NMdata` updated to latest CRAN or MPN realease. 

## Simulate a Nonmem model from R
In its simplest use, a simulation of the model stored in
"path/to/file.mod" using the simulation input data set stored in the
variable `data.sim` this way:

```{r}
simres <- NMsim(path.mod=/path/to/file.mod,
                data=data.sim)
```
`NMsim` will then do the following:
* Create a simulation input control stream based on `file.mod` 
  ($SIMULATE instead of $ESTIMATION) 
* Update and fix initial values based on estimate (from `file.ext`)
* Modify the control stream to use the simulation dataset `data.sim`
  as input control stream (replace `$INPUT` and `$DATA`)
* Run Nonmem on the generated simulation control stream
* Collect output data tables, combine them, and merge with the input
  data (`data.sim`)
* Return the collected data

## How NMsim works and what is required
The strength of `NMsim` is that it does not simulate, translate or
otherwise interpret the model. Instead, it automates the simulation
workflow with Nonmem and wraps it all into one R function. This
eliminates the need for re-implementation of a model. On the other
hand, this also means that `NMsim` can't work without Nonmem.

`NMsim` can use Nonmem directly or via `PSN`. If `NMsim` is run where
Nonmem can't be executed, `NMsim` can still prepare the simulation
control stream and datafile.

`NMsim` is in itself a small R package. It makes extensive use of
functionality to handle Nonmem data and control streams provided by
the R package
[`NMdata`](https://cran.r-project.org/web/packages/NMdata/index.html).

## Available types of simulations
Three types of simulations are currently supported:
- Simulation of new subjects (default or explicitly with type.sim="default")
- Simulation of a typical subject (ETAs equal 0, type.sim="typical")
- Simulation of subjects already estimated in Nonmem model (type.sim="known")

With a simulation data set (`simdat`) and an estimated Nonmem run
(input control stream stored in `models/run1.mod`) at hand, it is this simple to simulate new subjects:

```{r}
simres <- NMsim("models/run1.mod",simdat,
                dir.sim="simulations",suffix.sim="example",seed=123)
```

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
