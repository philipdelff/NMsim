# NMsim
`NMsim` is an R package that can modify and start Nonmem jobs from
within R. Most notably, it can simulate Nonmem models (using the
`NMsim` function) based on just a simulation data set and a path to an
estimation control stream. It will also retrive and combine output tables with input data once Nonmem has finished and return the results to R. 

## Install
Easiest way to install `NMsim` is using the `remotes` package to install with R:

```{r}
library(remotes)
install_github("philipdelff/NMsim")
```

`NMsim` makes extensive use of functionality provided by the `NMdata`
package. For most recent features of `NMsim` to work, make sure to at
least keep `NMdata` updated to latest CRAN or MPN release. 

## Simulate a Nonmem model from R
In its simplest use, a simulation of the model stored in
"path/to/file.mod" using the simulation input data set stored in the
variable `data.sim` this way:

```{r}
simres <- NMsim(path.mod=/path/to/file.mod,
                data=data.sim)
```
`NMsim` will then do the following:

* Save the simulation input data for Nonmem
* Create a simulation input control stream based on `file.mod` 
  ($INPUT and $DATA matching simulation data set, $SIMULATE instead of $ESTIMATION) 
* Update and fix initial values based on estimate (from `file.ext`)
* Run Nonmem on the generated simulation control stream
* Collect output data tables, combine them, and merge with the simulation input
  data
* Return the collected data

## How NMsim works 
One strength of `NMsim` is that it does not simulate, translate or
otherwise interpret the model. Instead, it automates the simulation
workflow with Nonmem and wraps it all into one R function. This
eliminates the need for re-implementation of a model. On the other
hand, this also means that `NMsim` can't work without Nonmem, and that
it does not provide anything beyond what is in the model.

`NMsim` can use Nonmem directly or via `PSN`. If `NMsim` is run where
Nonmem can't be executed, `NMsim` can still prepare the simulation
control stream and datafile. 

`NMsim` is in itself a small R package. It makes extensive use of
functionality to handle Nonmem data and control streams provided by
the R package
[`NMdata`](https://cran.r-project.org/package=NMdata).

## Supported types of simulations
`NMsim` has a flexible way to define simulation methods. The following
methods are currently provided:

- Simulation of new subjects (default or explicitly with `method.sim=NMsim_default`)
- Simulation of a typical subject (ETAs equal 0, `method.sim=NMsim_typical`)
- Simulation of subjects already estimated in Nonmem model (`method.sim=NMsim_known`) 
- Simulation with parameter uncertain based on a Nonmem covariance step (`method.sim=NMsim_VarCov`)
- Simulation "as is" in case you already prepared a simulation control stream and just want to automate the use of it in combination with simulation data sets (`method.sim=NMsim_asis`)

In addition, `NMsim` can simulate multiple models at a time. E.g., if a
bootstrap run of a model is available, NMsim can run the simulation
with each of the bootstrap models and collect all the results in one
dataset. This provides a robust and easy way to simulate a Nonmem
model with uncertainty.

You can also write your own methods, if you have some other
Nonmem-based simulation (or other job) you want to automate using
`NMsim`.

Many features are available. Prominent ones are:
- Can use submit jobs to clusters (so running the simulation on say
1,000 model estimates from a bootstrap is actually not that hard).
- Simulation replicates using Nonmem `SUBPROBLEMS` feature avaible
  through the `subproblems` argument
- Simulations of models on transformed observations can be
  automatically transformed back using the `transform` argument.

Since `NMsim` does not change the model code (like $PRED, $PK,
$ERROR), it cannot add residual variability to the simulation if this
is not simulated in the model code already. It does provide a way to
add residual variability in R after the simulation has been run
`addResVar()`. If you want to discuss how to add this as generally as
possible, please reach out on
[github](https://github.com/philipdelff/NMsim/issues).

## Supported model types
The methods currently provided by `NMsim` will work with (many or
most) Pop PK models and most continuous-scale PD models. Methods are
currently not provided for for time-to-event models. Also, depending
on the coding of the models, other censored data models may not work
out of the box, because the model may not have a single variable (in
Nonmem) that simulates the wanted information for all data rows, as
their interpretation may depend on other values.

The input data set must contain whatever variables are needed by the
Nonmem model. A common issue is if the Nonmem model uses a covariate
that is not in the simulation input data set. `NMdata`'s
[NMcheckData](https://philipdelff.github.io/NMdata/reference/NMcheckData.html)
is a good help identifying input data issues before running Nonmem -
and when Nonmem acts unexpectedly.

## NMsim and speed
Nonmem may not be the fastest simulator out there. But actually most
often, the reason Nonmem is slow at providing a simulation result is
that it takes a long time writing the `$TABLE` files (yes, that can
account for 90% or more of the time Nonmem spends). `NMsim` provides a
simple way to get around this. The argument `text.table` can be used
to define only the columns needed in the simulation output (which may
be as little as `PRED`, `IPRED`, and a couple more - remember the
input data is merged back automatically). As a result, `NMsim` may
still be slower than a re-implementation in a different framework. But
it's extremely easy to do.

## Requirements
Currently, `NMsim` can only run Nonmem on Unix/Linux systems. It
wouldn't be too big a deal to add support for Windows, so reach out if
you need it. It is possible to run `R` on Windows and run Nonmem on a
Unix/Linux system through SSH if needed.

NMsim does not need PSN but can use it. However, not all features are
available with PSN, so for some features you will have to specify the path to the
Nonmem executable (say `path.nonmem=/path/to/nmfe75` or any Nonmem
executable you want to use). Specifically of the simulation types
currently available, simulation of known subjects is not possible
using PSN (but works if a Nonmem executable is provided). On the other hand, submitting jobs to clusters is currently only supported via PSN.

If PSN is used, `NMsim` uses PSN's `execute` to run models. In
addition, `NMsim` can use PSN's `update_inits` to update initial
values in control streams. `NMsim` does also include its own simple
function to do this if `PSN` is not available.

## Is `NMsim` trustworthy?
Importantly, `NMsim` does not modify, translate or simulate the model
itself. It does modify control stream sections `$INPUT`, `$DATA`,
`$ESTIMATION`, `$SIMULATION`, `$THETA`, `$OMEGA`, `$SIGMA`, `$TABLE`
as needed. The fact that `NMsim` allows for skipping the
re-implementation but just simulates the Nonmem model as is,
eliminates the risk of discrepancies between the estimated model and
the simulated model.

The produced control stream is saved together with simulation data set
open for manual inspection and can obviously be run with Nonmem
independently of `NMsim`.

## Easily create simulation datasets
`NMsim` includes functions (`NMcreateDoses` and `addEVID2`) to very
easily create simulation data sets. While one certainly does not need to
use these functions to use `NMsim`, they do add to the package
providing a framework that enables a complete simulation workflow in
only 5-15 simple lines of R code. 

	
## Run Nonmem from R
There are several other packages out there that can do this, and
`NMsim` may not be your best choice if this feature is all you are
looking for. However, running Nonmem using the `NMexec()` function
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