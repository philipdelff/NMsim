# Changelog

## NMsim 0.2.8

CRAN release: 2026-09-08

### New Features

- The `carry.out` argument is better implemented in NMsim and
  NMreadSim(). `carry.out` is used to specify what columns from the
  input data set to include in the simulation results (while
  `table.vars` is used to control what variables to include in Nonmem
  `$TABLE`). This can be supplied to
  [`NMsim()`](https://nmautoverse.github.io/NMsim/reference/NMsim.md),
  and
  [`NMreadSim()`](https://nmautoverse.github.io/NMsim/reference/NMreadSim.md)
  reapplies `carry.out`. It is useful when performing very large
  simulations where a wide input data set us memory demanding to merge
  onto the simulation results.

- [`NMreadSim()`](https://nmautoverse.github.io/NMsim/reference/NMreadSim.md)
  has a new argument, `reread.tmp`, which can be used to force a re-read
  of the Nonmem outputs (in contrast to the compressed simulation
  results). This is especially useful if changing `carry.out`.

- [`sampleCovs()`](https://nmautoverse.github.io/NMsim/reference/sampleCovs.md)
  can now sample with or without replacement - see new argument
  `replace`.

- [`sampleCovs()`](https://nmautoverse.github.io/NMsim/reference/sampleCovs.md)
  supports grouping for successful (parallel) simulations. This is
  normally only used if
  [`sampleCovs()`](https://nmautoverse.github.io/NMsim/reference/sampleCovs.md)
  is used in combination with
  [`NMsim_EBE()`](https://nmautoverse.github.io/NMsim/reference/NMsim_EBE.md),
  so if sampling from observed subjects to simulate with their empirical
  Bayes’ estimates. This will likely lead to duplicated `ID` values.
  [`sampleCovs()`](https://nmautoverse.github.io/NMsim/reference/sampleCovs.md)
  now returns a column that can be used for splitting (using say
  [`data.table::split()`](https://rdrr.io/pkg/data.table/man/split.html))
  the data set into data sets without duplicated `ID` values.
  [`sampleCovs()`](https://nmautoverse.github.io/NMsim/reference/sampleCovs.md)
  also offers the arguments `col.idcgrp` to name the returned column and
  `idcgrp.redist` to redistribute the split to obtain (close to) equally
  sized splits.

- The `qsub` execution path can now be specified using
  [`NMdata::NMdataConf()`](https://nmautoverse.github.io/NMdata/reference/NMdataConf.html),
  like this:

``` r

NMdata::NMdataConf(path.qsub="/path/to/qsub")
```

This should normally not be necessary, but in some setups, `qsub` is not
in the path to executables available to the shell that executes
`Nonmem`, using `qsub` if `sge=TRUE`. On linux, you can most often find
the `qsub` path by opening a terminal and executing

``` shell
which qsub
```

### Bugfixes

- [`typicalize()`](https://nmautoverse.github.io/NMsim/reference/typicalize.md)
  (invoked by `NMsim(typical=TRUE)`) now drops sections with priors
  (like `$OMEGAP`, `$OMEGAPD`) when the parameter section (like
  `$OMEGA`) is “typicalized”. This should resolve issues related to
  typical-subject simulations of models with between-occasion
  variability.

- [`NMreadSim()`](https://nmautoverse.github.io/NMsim/reference/NMreadSim.md)
  would not always respect the `check.time` argument. Now fixed.

- Paths would in some (not very common) cases get wrongly processed by
  NMsim, causing issues when reading simulation results. Fixed.

## NMsim 0.2.7

CRAN release: 2026-03-19

### New Features

- [`prioritizePaths()`](https://nmautoverse.github.io/NMsim/reference/prioritizePaths.md)
  is a convenient function that allows the user to provide multiple
  candidate paths and automatically pick the first one available. This
  is convenient using `NMsim` because the Nonmem installation path often
  varies from system to system when sharing code between users, or even
  when as a user porting scripts between systems. For instance, this
  could be to accommodate running on a linux and a windows system, and
  sets the default to the first of the two available.

&nbsp;

    library(NMdata)
    NMdataConf(path.nonmem = prioritizePaths(c(
      "/opt/NONMEM/nm75/run/nmfe75",
      "C:/nm75g64/run/nmfe75.bat"))
      )

### Bugfixes

- In some cases, `NMsim` needs to replace zeros by tiny numbers in
  Nonmem control streams and it uses 1E-30 for this. In some cases, that
  would be printed without scientific notation leading to too long
  number representations, and Nonmem will throw an error. This has been
  fixed so it now makes sure to print 1E-30 with scientific notation.
  Thank you Chengjun Jiang for reporting this
  (<https://github.com/NMautoverse/NMsim/issues/40>).

## NMsim 0.2.6

CRAN release: 2025-11-03

### New Features

- `NMsim_NWPRI` gains an argument, `add.diag`, to add a value to
  variance-covariance diagonal. This can be used in case the
  variance-covariance matrix has (typically numerically very small)
  negative values.

### Bugfixes

- `NMsim_NWPRI` would fail on models with `$OMEGA BLOCK(N) SAME`
  structures. This is often used for between-occasion variability. This
  has been fixed, even though only single omega parameters can be used
  with `SAME` at this point. Also, each SAME block must be written out,
  as the `SAME(N)` notation for repeating `SAME` blocks is still not
  supported by NMsim. Thanks to Brian Reilly for working on this.

- `NMsim_EBE` now works when no data set is supplied. This can be used
  to get individual `PRED` and `IPRED`, especially if these are
  evaluated differently in the estimation control stream (e.g. using M3,
  `PRED` gets a different interpretation at censoring).

- `name.sim` argument to
  [`NMsim()`](https://nmautoverse.github.io/NMsim/reference/NMsim.md)
  now supports strings ending in periods, e.g. `name.sim=string..` now
  works.

- `dir.sims` and `dir.res` relative paths starting with `../` now work.
  Example: `dir.sims="../simtmp"` would fail. Fixed.

- [`sampleCovs()`](https://nmautoverse.github.io/NMsim/reference/sampleCovs.md)
  would fail if input data set did not include an `EVID` column. Fixed.

### Other Improvements

- `NMsim_VarCov` has become faster, reducing the time spent setting up
  simulations by around 1/3, depending on the simulation problem.

## NMsim 0.2.5

CRAN release: 2025-09-06

### New features

- Method to run “typical subject” simulations has been improved and the
  code has been simplified. This is controlled using the `typical`
  argument in
  [`NMsim()`](https://nmautoverse.github.io/NMsim/reference/NMsim.md).
  The basic use of this argument is `typical=FALSE` (default, not a
  typical subject simulation) or `typical=TRUE`. If `typical=TRUE`,
  `$OMEGA` parameters are fixed at zero. If the model has `OMEGA` prior
  parameters `$OMEGAP` and `$OMEGAPD`, those are fixed to zero too.
  Instead of a logical (`TRUE/FALSE`), a character vector can now be
  supplied to specify what oarameter types to fix at zero. E.g. to also
  drop residual variability, use `typical=c("omega","sigma")` (remember
  to add the prior parameter types if needed).

- A new
  [`NMsim()`](https://nmautoverse.github.io/NMsim/reference/NMsim.md)
  argument `dir.sim.sub` controls whether a subdirectory is created in
  `dir.sims` for Nonmem execution. When using
  [`NMsim()`](https://nmautoverse.github.io/NMsim/reference/NMsim.md)
  for estimation, it may be more convenient to use `dir.sim.sub=FALSE`
  to get all model executions in the same directory.

- [`NMcreateDoses()`](https://nmautoverse.github.io/NMsim/reference/NMcreateDoses.md)
  has a new argument, `N`, allowing for creation of multiple subjects.

### Bugfixes

[`NMsim_NWPRI()`](https://nmautoverse.github.io/NMsim/reference/NMsim_NWPRI.md)
in combination with `typical=TRUE` would fail in some cases. This has
been fixed with the new implementation of the `typical` argument.

### Other improvements

[`NMsim()`](https://nmautoverse.github.io/NMsim/reference/NMsim.md)
argument `name.sim` supports stings with parenthesis.

## NMsim 0.2.4

CRAN release: 2025-07-16

### New features

- [`NMsim()`](https://nmautoverse.github.io/NMsim/reference/NMsim.md)’s
  support for handling and control of initial values (the `inits`
  argument) has been greatly improved. In recent versions a new method
  for reading, updating, and writing the parameter definitions in
  control streams has been implemented. This allows for modification of
  this information and for fixing/unfixing parameters. The downside is
  it comes with some limitations to control stream syntax. With 0.2.4
  those limitations are getting rare. The original much simpler method
  is still available and provides a robust alternative for many
  simulation purposes and is kept as a fallback workaround.

- [`NMsim()`](https://nmautoverse.github.io/NMsim/reference/NMsim.md)’s
  `init` argument supports `ext` and `inits.tab` formats. These
  interfaces to specifying parameter values greatly improves flexibility
  for programming, and for specifying multiple new parameter sets for
  series of model runs.

- [`NMreadSim()`](https://nmautoverse.github.io/NMsim/reference/NMreadSim.md)
  gains the `skip.missing` argument. In case some model runs fail or
  haven’t finished, this allows
  [`NMreadSim()`](https://nmautoverse.github.io/NMsim/reference/NMreadSim.md)
  to read whatever it can and skip the ones missing.

- Summary function included on NMsim simulation results. There is still
  room for improvement - try it out with
  [`summary()`](https://rdrr.io/r/base/summary.html) on results from
  [`NMsim()`](https://nmautoverse.github.io/NMsim/reference/NMsim.md)
  and
  [`NMreadSim()`](https://nmautoverse.github.io/NMsim/reference/NMreadSim.md).

### Bugfixes

- `NMREP` is a data column that
  [`NMsim()`](https://nmautoverse.github.io/NMsim/reference/NMsim.md)
  automatically adds to count subproblems when `subproblem` is used.
  This was not added when simulating PREDPP (`$PRED`) models and when
  not providing a simulaiton data set (VPC style). Thanks to Ahmed
  Abulfathi for reporting.

- In some cases `NMsim_NMWPRI()` in combination with `typical=TRUE`
  would create wrong `$PRIOR` dimensions. Fixed.

### Other improvements

- [`NMaddSamples()`](https://nmautoverse.github.io/NMsim/reference/NMaddSamples.md)
  only inserts `MDV` column if `MDV` is already present in `data`.

- [`NMexec()`](https://nmautoverse.github.io/NMsim/reference/NMexec.md)
  uses the `-maxlim=2` option when executing Nonmem. This is implemented
  on both the internal execution method and when PSN’s execute is used.

## NMsim 0.2.3

CRAN release: 2025-05-20

### New features

- [`NMaddSamples()`](https://nmautoverse.github.io/NMsim/reference/NMaddSamples.md)
  gains a `by` argument. This makes
  [`NMaddSamples()`](https://nmautoverse.github.io/NMsim/reference/NMaddSamples.md)
  easy to use for generation of both nominal sampling schemes and
  recreation of observed sampling schemes.

- Streamlining of
  [`NMsim_VarCov()`](https://nmautoverse.github.io/NMsim/reference/NMsim_VarCov.md)
  and
  [`samplePars()`](https://nmautoverse.github.io/NMsim/reference/samplePars.md).
  [`NMsim_VarCov()`](https://nmautoverse.github.io/NMsim/reference/NMsim_VarCov.md)
  now always calls
  [`samplePars()`](https://nmautoverse.github.io/NMsim/reference/samplePars.md)
  internally.
  [`samplePars()`](https://nmautoverse.github.io/NMsim/reference/samplePars.md)
  uses the `method` argument to switch between `mvrnorm`(multivariate
  normal distribution) and `simpar` (inverse Wishart distribution.

- [`NMsim_VarCov()`](https://nmautoverse.github.io/NMsim/reference/NMsim_VarCov.md)
  gains `method.sample` argument which is passed as `method` to
  [`samplePars()`](https://nmautoverse.github.io/NMsim/reference/samplePars.md).
  This means
  [`NMsim_VarCov()`](https://nmautoverse.github.io/NMsim/reference/NMsim_VarCov.md)
  can be used to simulated with parameter uncertainty using either
  `mvrnorm` or `simpar`.

Messages have been implemented in samplePars() to summarize number of
truncations in case `mvrnorm` results in negative variance parameters.

### Bugfixes

NMsim_VarCov() sampling of OMEGA/SIGMA. Affects simulation with
between-subject variability. Big thanks to Sanaya Shroff for reporting
and fixing.

### Other Improvements

- Improved defaults for whether to suppress Nonmem messages (`nmquiet`
  argument). Particularly, when `NMsim` is not waiting on Nonmem runs,
  it will by default suppress those messages. Basically, NMsim will now
  by default only show Nonmem terminal messages if it is waiting, and
  only one Nonmem model is executed. Notice, that `quiet=TRUE` implies
  suppressing both `NMsim` and Nonmem messages and `nmquiet=TRUE`.

### Other Changes

- The deprecated
  [`addEVID2()`](https://nmautoverse.github.io/NMsim/reference/addEVID2.md)
  is kept as a snapshot of when `NMsamples()` was introduced. The new
  `by` argument is not compatible with the default behavior of the
  deprecated
  [`addEVID2()`](https://nmautoverse.github.io/NMsim/reference/addEVID2.md).
  This is to ensure reproducibility of existing code on the user side.

## NMsim 0.2.2

CRAN release: 2025-05-06

### New features

- [`sampleCovs()`](https://nmautoverse.github.io/NMsim/reference/sampleCovs.md)
  is a new function to conveniently sample subject-level covariates from
  an existing data set.

### Bugfixes

- There was a bug introduced in version 0.2.1 leading
  [`NMsim()`](https://nmautoverse.github.io/NMsim/reference/NMsim.md) to
  throw an error saving some data sets. The issue is fixed.

- The default Nonmem `$TABLE` format is insufficient with NMsim’s way to
  create row counters when simulating 1e5 rows or more. NMsim is now by
  default using a format with a much higher resolution. Thanks to Sanaya
  Shroff for helping debugging this.

## NMsim 0.2.1

CRAN release: 2025-04-24

### New Features

- [`NMaddSamples()`](https://nmautoverse.github.io/NMsim/reference/NMaddSamples.md)
  replaces the
  [`addEVID2()`](https://nmautoverse.github.io/NMsim/reference/addEVID2.md)
  function.
  [`addEVID2()`](https://nmautoverse.github.io/NMsim/reference/addEVID2.md)
  will still work and redirects to
  [`NMaddSamples()`](https://nmautoverse.github.io/NMsim/reference/NMaddSamples.md)
  for backward compatibility. Your existing work will still work but you
  will get a (suppressable) message about the name change. Especially
  due to work on optimal sampling with `NMsim` by Ahmed Abulfathi and
  myself, we need a more flexible interface for adding samples.

- [`NMaddSamples()`](https://nmautoverse.github.io/NMsim/reference/NMaddSamples.md)
  comes with a new argument `DV`. By default
  [`NMaddSamples()`](https://nmautoverse.github.io/NMsim/reference/NMaddSamples.md)
  (used to be,
  [`addEVID2()`](https://nmautoverse.github.io/NMsim/reference/addEVID2.md))
  adds rows without `DV` values and with `EVID=2` and `MDV=1`. If `DV`
  is supplied,
  [`NMaddSamples()`](https://nmautoverse.github.io/NMsim/reference/NMaddSamples.md)
  will include that value in the `DV` column and by default use `EVID=0`
  and `MDV=0`. An example where this is useful is when generating
  datasets for `$DESIGN` where `DV=0` is often used.

- [`samplePars()`](https://nmautoverse.github.io/NMsim/reference/samplePars.md)
  is a new function that replaces
  [`sampleParsSimpar()`](https://nmautoverse.github.io/NMsim/reference/sampleParsSimpar.md).
  [`samplePars()`](https://nmautoverse.github.io/NMsim/reference/samplePars.md)
  takes the `method` argument which can be used to switch between
  multivariate normal distribution `method="mvrnorm"` and using the
  `simpar` package to get use Inverse-Wishart distribution for `$OMEGA`
  and `$SIGMA` parameters. Notice, both methods are fully automated in
  NMsim - all you need to write is the path to a control stream and the
  number (`nsims`) of parameter sets wanted.

- [`simPopEtas()`](https://nmautoverse.github.io/NMsim/reference/simPopEtas.md)
  by default does not overwrite an existing `.phi` file.
  [`simPopEtas()`](https://nmautoverse.github.io/NMsim/reference/simPopEtas.md)
  is used to generate sampled `ETA`s for use in future model simulations
  with the same synthetic population (`ETA`s). If the `.phi` file that
  stores the `ETA`s gets overwritten using a new seed, it will affect
  simulations using that `.phi` file. Overwriting the `.phi` file with
  different seeds should therefore be avoided, and this new behavior of
  protecting the generated `.phi` files reduces that risk.

### Bugfixes

- [`NMsim_VarCov()`](https://nmautoverse.github.io/NMsim/reference/NMsim_VarCov.md)
  would not include `$OMEGA` and `$SIGMA` blocks which lead to errors in
  Nonmem. This bug was likely introduced in NMsim 0.1.6 and has now been
  fixed.

- Version 0.2.0 gave some warnings about non-existing columns. The
  warnings are benign and can be safely ignored. They are avoided in
  Version 0.2.1.

- Updating initial values in models using `SAME` argument on random
  effects (e.g. in between-occasion variability) would fail if the
  effects were fixed. Resolved. Thanks to Sergio Iadevaia for reporting
  this.

- Data sets with commas in character columns would make `NMsim` fail.
  Support for such data sets had not been carried over with the new data
  handling approach implemented in version 0.2.0. This has now been
  resolved.

- [`NMcreateDoses()`](https://nmautoverse.github.io/NMsim/reference/NMcreateDoses.md)
  had a bug in the `addl.lastonly` feature which would throw errors when
  `TIME` was longer than two. `addl.lastonly=TRUE` is default and means
  that if the length of the `ADDL` argument is one, it will be applied
  to the last dose only. This is for the common case where initial doses
  (say a load or an initial escalation phase) are followed by a
  maintenance regimen. The bug has been resolved.

- [`NMaddSamples()`](https://nmautoverse.github.io/NMsim/reference/NMaddSamples.md)
  would fail if using the `TAPD` argument when `TAPD` was already
  available as a column in data. Fixed.

### Other Improvements

- The configuration of job submission is simplified when jobs are run in
  parallel with single-core processing.

- Checks have been included in
  [`summarizeCovs()`](https://nmautoverse.github.io/NMsim/reference/summarizeCovs.md)
  for whether `NA`’s are produced.
  [`summarizeCovs()`](https://nmautoverse.github.io/NMsim/reference/summarizeCovs.md)
  is used to summarize simulated covariate effects (typically for forest
  plots). Especially, if for some reason the reference exposure is zero
  (likely an error by the modeler),
  [`summarizeCovs()`](https://nmautoverse.github.io/NMsim/reference/summarizeCovs.md)
  would throw an error. Now
  [`summarizeCovs()`](https://nmautoverse.github.io/NMsim/reference/summarizeCovs.md)
  will try to identify this, give useful messages back, and report the
  estimates.

- Consistent ordering of columns in simulation results independently of
  `col.id` and `col.time`.

## NMsim 0.2.0

CRAN release: 2025-03-13

### New Features

- A greatly improved handling of data files has been implemented. This
  improves speed, reduced memory and disk usage, and adds features. It
  is fully backward compatible with the user interface of earlier
  versions.
  - Data sets are only saved once for each model.
  - Output tables are written and read more efficently. This is obtained
    by a combination of default `TABLE` options and an efficient method
    to read the tables if those options have not been changed by the
    user. If the user does change them, a more robust but slower and
    more memory intensive method is used.
  - Input and output data are now by default merged using a row
    identifier inserted by NMsim. This makes NMsim robust to Nonmem code
    that does not simulate all rows in input data.
  - Subproblems are automatically counted in the `NMREP` column in
    output. Force the inclusion of this column using the `nmrep`
    argument if needed even if not using `subproblems`.
- Variables from input data to be included in results can be specified
  using the new `carry.out` argument. The default behavior by
  [`NMsim()`](https://nmautoverse.github.io/NMsim/reference/NMsim.md) is
  to include all variables from input data on the result. However, if
  the data set contains many rows or columns, this can be memory
  demanding. Now you can minimize memory use and maximize speed by
  limiting the variables in both input and output. For example

&nbsp;

    simres <- NMsim(file.mod,data,
                    table.vars=c("PRED","IPRED","Y"),
                    carry.out=c("ID","TIME",`EVID`)
                    )

`simres` will in this case contain only `PRED`, `IPRED`, and `Y` from
the output table, and only `ID`, `TIME`, and `EVID` from the input
`data.frame` (`data`).
[`NMsim()`](https://nmautoverse.github.io/NMsim/reference/NMsim.md) also
takes a new argument `table.format` which can be used to adjust the
table format. But normally, that is not be necessary. There is no reason
to list `ID` or any other column from the input data in `table.vars`
since they can be carried over directly fro the input data, avoiding
potential loss of accuracy in writing and reading to and from text
files. You do not need to worry about merging input and output data
correctly -
[`NMsim()`](https://nmautoverse.github.io/NMsim/reference/NMsim.md)
handles that internally using its own row identifier.

- Handling of Nonmem data filters. In case a sim is run without an
  simulation input data set,
  [`NMsim()`](https://nmautoverse.github.io/NMsim/reference/NMsim.md) by
  default reuses the estimation data set and the `IGNORE` and `ACCEPT`
  statements in the estimation control stream. This is very useful for
  visual predictive check (VPC) simulations. However, the aim may be to
  run the simulation on the same data set with differnt inclusions. A
  common example of the is if the estimation was run without samples
  below the quantification limit (M1), but the simulation also be
  performed on those samples. This can now be done by passing new
  filters to
  [`NMsim()`](https://nmautoverse.github.io/NMsim/reference/NMsim.md).
  In fact, this can even be done by first reading the filters from the
  control stream, then easily editing them, before passing them to
  [`NMsim()`](https://nmautoverse.github.io/NMsim/reference/NMsim.md).

### Other changes

- [`NMsim()`](https://nmautoverse.github.io/NMsim/reference/NMsim.md)’s
  argument `modify.model` has been renamed to `modify`. This is to align
  argument names with other arguments available for model modification,
  namely `inits`, `sizes`, and `filters`.

- [`NMcreateDoses()`](https://nmautoverse.github.io/NMsim/reference/NMcreateDoses.md)
  requires AMT to be provided.

- [`addEVID2()`](https://nmautoverse.github.io/NMsim/reference/addEVID2.md)
  only requires `CMT` argument when the column of the same name is
  present in `data`. Not all models require `CMT` and this change allows
  for building such data sets with
  [`NMcreateDoses()`](https://nmautoverse.github.io/NMsim/reference/NMcreateDoses.md)
  and
  [`addEVID2()`](https://nmautoverse.github.io/NMsim/reference/addEVID2.md).

## NMsim 0.1.6

CRAN release: 2025-02-05

- A major improvement is implemented on `NMSim_NWPRI()`, the simulation
  method that leverages the Nonmem `NWPRI` subroutine to simulate models
  with parameter uncertainty. This method was first included in NMsim
  0.1.3 but - as was clearly declared in that version - it could only be
  trusted for simulation of `THETA`s. After further development in both
  `NMsim` and `NMdata` to support this as well as bugfixes in the new
  Nonmem 7.6.0, NMsim provides full support for simulation with
  parameter uncertainty using the inverse Wishart distribution through
  this simple interface.

- A new arguments `inits` is introduced to manually specify parameter
  (initial) values. This is the values that go into `$THETA`, `$OMEGA`
  and `SIGMA` sections of the control stream. To simulate with the final
  estimated values (stored in a `.ext` file), simply add
  `inits=list("theta(1)"=list(init=1.2))`. For simulation, only the
  parameter values (init) may be of interest, but if you are using NMsim
  for estimation too, bounds and whether parameters are fixed can now
  also controlled. `BLOCK` structures in `$OMEGA` and `SIGMA` can
  currently not be changed (say, correlation of two random effects
  cannot be introduced or removed).

- The `$SIZES` can now easily be controlled using the simple `sizes`
  argument in
  [`NMsim()`](https://nmautoverse.github.io/NMsim/reference/NMsim.md).
  It leverages a new function `NMupdateSizes()` which can be used to
  edit `$SIZES` independently of the
  [`NMsim()`](https://nmautoverse.github.io/NMsim/reference/NMsim.md)
  function. In
  [`NMsim()`](https://nmautoverse.github.io/NMsim/reference/NMsim.md)
  just add the argument like `sizes=list(PD=100)` which will update or
  add `$SIZES PD=100` as needed. See documentation for more details.

- Nonmem execution

- Improved monitoring of Nonmem jobs. In NMsim 0.1.5,
  [`NMsim()`](https://nmautoverse.github.io/NMsim/reference/NMsim.md)
  would not always catch and properly handled failed runs. On Linux,
  this is much better handled now. On Windows, failures still may not be
  caught properly - more work to be don on Windows to align with the
  approach on Linux.

- A new `post.fun` argument has been introduced in
  [`NMexec()`](https://nmautoverse.github.io/NMsim/reference/NMexec.md)
  to run additional code once Nonmem has finished. This can be used to
  automatically initiate creation of goodness of fit plots, simulations
  or any full workflows run using `Rscript` after estimation of models.

### Bugfixes

- [`overwrite()`](https://nmautoverse.github.io/NMsim/reference/overwrite.md)
  is a helper function intended to use in
  [`NMsim()`](https://nmautoverse.github.io/NMsim/reference/NMsim.md)’s
  `modify.model` argument. It would not work correctly for strings
  containing some special, at least. Fixed.

- `NMsim_NWPRI` would not always paste the full variance-covariance
  matrix for theta estimates into `$THETAPV` which would make NONMEM
  fail. Fixed.

## NMsim 0.1.5

CRAN release: 2024-11-14

### New features

- [`NMreadSim()`](https://nmautoverse.github.io/NMsim/reference/NMreadSim.md)
  has a new argument called `rm.tmp` which is used to remove the
  intermediate NONMEM files if they are successfully read. Remember,
  [`NMreadSim()`](https://nmautoverse.github.io/NMsim/reference/NMreadSim.md)
  creates a compressed results data set which will be read by
  [`NMreadSim()`](https://nmautoverse.github.io/NMsim/reference/NMreadSim.md)
  in future function calls, so unless debugging is needed on the
  simulation control streams and files returned by NONMEM running the
  simulations, it may be better to delete the intermediate files
  altogether and save the disk space.

- [`expandCovs()`](https://nmautoverse.github.io/NMsim/reference/expandCovs.md)
  has a new argument `reduce.ref` which defaults to `TRUE` meaning that
  by default there will be only one reference combination. If `FALSE`
  [`expandCovs()`](https://nmautoverse.github.io/NMsim/reference/expandCovs.md)
  will return one reference for each covariate. The forest plot can be
  evaluated with just one reference simulation.

- New function
  [`summarizeCovs()`](https://nmautoverse.github.io/NMsim/reference/summarizeCovs.md)
  introduced to summarize simulation results for forest plots. This
  function is closely related to
  [`expandCovs()`](https://nmautoverse.github.io/NMsim/reference/expandCovs.md)

- [`NMsim()`](https://nmautoverse.github.io/NMsim/reference/NMsim.md) no
  longer requires NONMEM to be available if `reuse.results=TRUE` and
  NONMEM does not need to be run.

### Bugfixes

- NMsim 0.1.4 would not submit jobs to cluster when number of cores was
  `nc=1`. Fixed. Workaround in 0.1.4, use `nc=2`.

- NMsim 0.1.4 erroneously concluded jobs had failed when sent to the
  cluster if there was no existing queue. This is due to the exit status
  of `qsub` in such cases. This has been fixed. As a workaround in
  0.1.4, just run your sim again once the queue has been initiated by
  the first NMsim called.

- When NONMEM failed in 0.1.4, NMsim might not return debugging info.
  Fixed.

## NMsim 0.1.4

CRAN release: 2024-11-02

### New features

- [`sampleParsSimpar()`](https://nmautoverse.github.io/NMsim/reference/sampleParsSimpar.md)
  is a new function that automates sampling of parameter values from an
  estimated variance-variance matrix in a successful `$COVARIANCE` step
  using the `simpar` R package from Metrum Research Group. `simpar` is
  currently not on CRAN, so the user must install it from MPN or github
  to make use of
  [`sampleParsSimpar()`](https://nmautoverse.github.io/NMsim/reference/sampleParsSimpar.md).
  The sampled parameter values can be fed directly to `NMsim` using the
  `NMsim_VarCov` method making it very easy to simulate with parameter
  uncertainty based on `simpar`. I want to thank Sanaya Shroff for her
  outstanding work on this functionality and for her exciting work
  summarizing the available methods for simulation with parameter
  uncertainty which she will be sharing at ACoP 2024. Also a big thanks
  to Eric Anderson for helping out with adjusting the github workflows
  to pull `simpar` from MPN.

- [`expandCovs()`](https://nmautoverse.github.io/NMsim/reference/expandCovs.md)
  is a new function that puts together data sets for univariately
  varying covariates while keeping other at reference values. The
  function can derive both reference values and covariate values to
  simulate at by using
  i.e. [`median()`](https://rdrr.io/r/stats/median.html) and
  [`quantile()`](https://rdrr.io/r/stats/quantile.html).

- [`NMsim()`](https://nmautoverse.github.io/NMsim/reference/NMsim.md)

  - Results are now equipped with three columns distinguishing simulated
    models. This separation of information makes it easier to summarize
    simulation results within/across models and/or within/across
    simulation of models.
    - `model`: The run name derived from `file.mod`.
    - `name.sim`: The same as provided in the `name.sim` argument.
    - `model.sim` The name of the generated model. In the simple case,
      this is `model` and `name.sim` combined. But in many cases,
      multiple models are being generated for each simulated control
      stream.
  - No longer requires a `.ext` file if updating parameter values using
    PSN’s `update_inits`. It is still recommended to keep the `.ext`
    file since it provides higher accuracy than the `.lst` file.

- [`NMexec()`](https://nmautoverse.github.io/NMsim/reference/NMexec.md)

  - When submitting all updated models,
    [`NMexec()`](https://nmautoverse.github.io/NMsim/reference/NMexec.md)
    will now by default try to detect if a model is already running
    before submitting it.

  - Provides a summary of models to be submitted before starting to do
    so.

- [`NMcreateDoses()`](https://nmautoverse.github.io/NMsim/reference/NMcreateDoses.md)

  - `ADDL` and `II` are now also separate arguments providing a simpler
    interface than the `addl` argument. The `addl` argument provides the
    advantage of being able to specify the two columns together in one
    `data.frame`, possibly including covariates.

  - `add.lastonly` is a new argument. If `TRUE` (default) and `ADDL` and
    `II` are of length 1, they are only applied to the last event in a
    dosing regimen.

  - `col.id` argument to specify name of subject id column or to omit
    altogether using `col.id=NA`.

  - Now checking that `TIME` is covering the length of all other
    arguments. In contrast to other arguments, it does not make much
    sense to try to extrapolate the `TIME` argument.

- [`addEVID2()`](https://nmautoverse.github.io/NMsim/reference/addEVID2.md)
  now has two arguments, `TIME` and `TAPD` which allow for specification
  of time since first dose and time after each dose at which to insert
  simulation records. The two can even be combined. `TIME` replaces the
  now deprecated `time.sim` argument, and `TAPD` is new.

### Bugfixes

- A bug most likely affecting most Windows users for execution of Nonmem
  has been fixed. If on Windows, you should upgrade to NMsim 0.1.4.
  Thank you to Boris Grinshpun for reporting this!

- When using `method.execute="nmsim"` there was an issue with
  parallellization. This was not a major problem in most simulation
  applications, but it should now be fixed.

- [`NMsim()`](https://nmautoverse.github.io/NMsim/reference/NMsim.md)

  - When not providing a simulation data set - typically a simulation
    for a VPC -
    [`NMsim()`](https://nmautoverse.github.io/NMsim/reference/NMsim.md)
    would fail with messages like

&nbsp;

    Error in `:=`((col.sim), ..name.sim) :
      Check that is.data.table(DT) == TRUE. Otherwise, := and `:=`(...) are defined for use in j, once only and in particular ways. See help(":=").

The issue has been fixed. If using NMsim 0.1.3 or earlier, the
workaround is to do `NMdataConf(as.fun="data.table")`. Then after having
the simulation results as a data.table, convert it with
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) or as
preferred.

Notice,
[`NMdataConf()`](https://nmautoverse.github.io/NMdata/reference/NMdataConf.html)
affects the succeeding
[`NMsim()`](https://nmautoverse.github.io/NMsim/reference/NMsim.md)
calls but also other NMdata and NMsim function calls. When the VPC
simulation has run, you may not want to continue recieving data.tables,
you should reset the default value for as.fun: `NMdataConf(as.fun=NULL)`
which will turn it back to returning data.frames by default. If you
prefer tibbles, you can do `NMdataConf(as.fun=tibble::as_tibble)`.
Generally, if you prefer to work with something that is not data.frames
(data.table and tibble the most common alternatives), it is recommended
to use
[`NMdataConf()`](https://nmautoverse.github.io/NMdata/reference/NMdataConf.html)
to customize your default.

- [`NMexec()`](https://nmautoverse.github.io/NMsim/reference/NMexec.md)
  - `NMexec` would fail running control streams named starting in
    numerals (like `1.mod`) when `sge=TRUE`. This is due to the way
    `sge` job names are generated by
    [`NMexec()`](https://nmautoverse.github.io/NMsim/reference/NMexec.md).
    Fixed by prepending “NMsim\_” in these cases.
- `NMcreateDoses`
  - Would in some cases create too many replicates if there were
    covariates on multiple arguments. Fixed.

## NMsim 0.1.3

CRAN release: 2024-08-26

### New features

- [`NMsim()`](https://nmautoverse.github.io/NMsim/reference/NMsim.md)

  - New simulation method
    [`NMsim_NWPRI()`](https://nmautoverse.github.io/NMsim/reference/NMsim_NWPRI.md)
    to simulate with parameter uncertainty. This automates simulation
    with parameter uncertainty using Nonmem’s `NWPRI` subroutine for
    models with a successful covariance step. For now this method only
    works for `THETA` since we have found that the parameter
    distributions sampled for `OMEGA` and `SIGMA` do not always match
    the model estimates and therefore cannot be trusted. To ensure that
    only `THETA` are sampled and simulated, this method should only be
    run using the `typical=TRUE` argument. This method is much faster
    than the existing methods in NMsim for simulation with parameter
    uncertainty
    ([`NMsim_VarCov()`](https://nmautoverse.github.io/NMsim/reference/NMsim_VarCov.md)).
    This method depends on `NMdata` version 0.1.7 or greater. Big thanks
    to Brian Reilly for his excellent work on this important
    contribution.

  - The [`add()`](https://nmautoverse.github.io/NMsim/reference/add.md)
    function to be used in
    [`NMsim()`](https://nmautoverse.github.io/NMsim/reference/NMsim.md)’s
    `modify.model` argument now supports prepending and appending of
    text lines to control stream sections. The default is still to
    append and `add("text",.pos="top")` will now prepend “text”.

- [`NMexec()`](https://nmautoverse.github.io/NMsim/reference/NMexec.md)

  - A “cleaning” feature has been added to
    [`NMexec()`](https://nmautoverse.github.io/NMsim/reference/NMexec.md),
    removing some of the temporary files generated by Nonmem after ended
    execution. The interface is inspired by PSN’s `clean` argument
    supporting values 0 (no cleaning), 1-4 (quite some cleaning - so far
    no difference betwen these values), and 5 for complete deletion of
    the temporary directory. When using `method.execute="PSN"` NMsim
    calls PSN’s execute passing on the `clean` value.

  - Default number of cores to be used by
    [`NMexec()`](https://nmautoverse.github.io/NMsim/reference/NMexec.md)
    can be controlled using `NMdataConf(nc=N)` where `N` is the desired
    default. Notice,
    [`NMsim()`](https://nmautoverse.github.io/NMsim/reference/NMsim.md)
    will not use this default. This is because parallellization is not
    as commonly used in simulation as in estimation.

  - A useful backup feature has been added. Before execution, any
    existing results files related to a model are by default moved to a
    backup folder. The backup files are not

- [`NMsimTestConf()`](https://nmautoverse.github.io/NMsim/reference/NMsimTestConf.md) -
  Function to summarize and test configuration. This is used internally
  and provides important debugging information.

- [`readParsWide()`](https://nmautoverse.github.io/NMsim/reference/readParsWide.md) -
  A function to read wide-format parameter tables - is now exported.
  This is useful when simulating with parameter values that have been
  sampled outside Nonmem, e.g. using the `simpar` package.

### Bugfixes

- [`NMexec()`](https://nmautoverse.github.io/NMsim/reference/NMexec.md)
  would fail on linux when run on models with multiple `$TABLE`
  statements. Fixed.

- `NMsim`’s internal method to update parameter initial values had an
  issue running on models with `$OMEGA` block structures. Fixed.

- [`NMreadSim()`](https://nmautoverse.github.io/NMsim/reference/NMreadSim.md)
  would fail if working directory had been changed. Fixed.

### Other changes

- addEVID2 will no longer add a DV=NA column if DV is not in the input
  data set.

## NMsim 0.1.2

CRAN release: 2024-07-15

No changes since 0.1.1 except for disabling a test that was failing on
some systems due to technical reasons.

## NMsim 0.1.1

CRAN release: 2024-07-03

While no critical and generally only few bugs have been found in NMsim
0.1.0, NMsim 0.2.0 includes several improvements and upgrades. The
interface feels smoother too. I want to thank Ron Keizer for feedback
and advice.

### New features

- [`NMsim()`](https://nmautoverse.github.io/NMsim/reference/NMsim.md)
  has a much reduced and improved messaging to the user. If more than
  one model or data set is supplied or generated, progress trackers will
  be shown while starting Nonmem, while waiting for Nonmem to finish,
  and while collecting the simulation results.

- The messages include information about where intermediate files and
  final results files are stored.

- [`NMexec()`](https://nmautoverse.github.io/NMsim/reference/NMexec.md)
  has improved support for estimation. `method.execute="nmsim"` and
  `method.execute="psn"` both work on linux and windows, even though
  less thoroughly tested on windows. Thank you to Boris Grinshpun for
  testing.

- Names of files containing final results from
  [`NMsim()`](https://nmautoverse.github.io/NMsim/reference/NMsim.md)
  have been renamed to be more intuitive. The previous `_paths.rds` will
  now be called `_MetaData.rds`. The results, once read and compressed,
  will be in a file called `_ResultsData.fst`. Notice, both these files
  are required to fully recover simulation results. Thanks to Brian
  Reilly for discussions on this and many other design aspects.

- It is now possible to provide specific parameters (`THETA`, `OMEGA`
  and `SIGMA`) for Nonmem simulation.
  [`NMsim()`](https://nmautoverse.github.io/NMsim/reference/NMsim.md)
  table for simulations. See argument `file.ext` and `NMsim_VarCov`’s
  argument `ext`.

- New arguments to control seeds. `NMsim` can either use R’s `set.seed`
  before generating the seeds for Nonmem. Detailed control of the seeds,
  including how many to include and the distribution of the random
  sources in Nonmem, can be controlled using the `seed.nm` argument.
  This way, the user can add random processes to the estimated control
  stream. The actual Nonmem seed values can also be provided.

- `method.sim=NMsim_typical()` has been replaced by argument
  `typical=TRUE`. This means typical subject simulations can now be
  combined with other simulations methods like `NMsim_VarCov`.

- [`NMsim()`](https://nmautoverse.github.io/NMsim/reference/NMsim.md)
  now adds a column called `sim` which carries the name of the
  simulation defined by the `name.sim` argument.

- Several checks for existence and consistency of files are implemented.

- The native Nonmem execution method now also works for estimation.

- `pnm` files are now saved with the model for transparency.

### Bugfixes

- Running `rbind` on results from `NMsim` would throw errors. Thanks to
  Simone Cassani for reporting this. Fixed.

- Using other file name extensions than `.mod` on input control streams
  in combination with `NMdataConf(file.mod)` would make NMsim fail.
  Thanks to Brian Reilly for reporting. Fixed.

### Other changes

- [`NMsim_known()`](https://nmautoverse.github.io/NMsim/reference/NMsim_known.md)
  renamed to
  [`NMsim_EBE()`](https://nmautoverse.github.io/NMsim/reference/NMsim_EBE.md).

- Generated control streams have been stripped off of the “NMsim\_”
  prefix. These files are located in `NMsim` generated folders so the
  prefix was uninformative.

- In case of multi-threaded (cluster) execution and something went wrong
  [`NMexec()`](https://nmautoverse.github.io/NMsim/reference/NMexec.md)
  used to write some output files from Nonmem in the current working
  directory. All these are now being written to the model execution
  directory for clarity and tidyness.

## NMsim 0.1.0

CRAN release: 2024-02-22

For the first time NMsim works on Windows. There may still be some
limitations but initial testing looks very promising. Make sure to set
`path.nonmem`. See the configuration vignette on the website:
[`NMsim-config.html`](https://nmautoverse.github.io/NMsim/articles/NMsim-config.html)

0.1.0 is also an important upgrade that solidifies the way NMsim reads
results from simulations. In addition to important bug fixes, it allows
for NMsim to wait on Nonmem to complete simulations - even when they are
run on a cluster. This means even large simulations with NMsim can be
integrated in scripts.

### New features

- Works on Windows - at least most features do.

- [`NMsim()`](https://nmautoverse.github.io/NMsim/reference/NMsim.md)
  and
  [`NMreadSim()`](https://nmautoverse.github.io/NMsim/reference/NMreadSim.md)
  now have `wait` arguments which controls if they will wait for Nonmem
  to finish simulating. This will also work if jobs were sent to the
  cluster.

- [`NMsim()`](https://nmautoverse.github.io/NMsim/reference/NMsim.md)
  respects the `reuse.results` argument. If `TRUE` it will use results
  file on the file system. This can be used in stead of putting
  [`NMsim()`](https://nmautoverse.github.io/NMsim/reference/NMsim.md)
  calls inside an if-statement to disable the simulation but read
  results on file.

- [`NMsim()`](https://nmautoverse.github.io/NMsim/reference/NMsim.md)
  looks for a couple of features of the provided control streams that
  are known to be able to cause issues. Warnings will be issued if these
  are found.

- `addEVID2` has a new argument, `EVID` to specify what value the `EVID`
  column should have. It can be useful sometimes to use `EVID=0` for
  simulation records.

### Bugfixes

- In some cases
  [`NMreadSim()`](https://nmautoverse.github.io/NMsim/reference/NMreadSim.md)
  would not get the path right to the simulation results leading to
  failures in reading simulation results. Fixed.

### Other changes

- Functions `NMreadExt` and `NMreadPhi` have been removed from NMsim.
  They live and are being maintained in the `NMdata` package. In NMsim,
  were deprecated and unmaintained functions.

## NMsim 0.0.10

CRAN release: 2024-02-08

NMsim 0.0.9 had an unfortunate bug in
[`NMreadSim()`](https://nmautoverse.github.io/NMsim/reference/NMreadSim.md)
which has been fixed. That bugfix is difference between 0.0.9 and
0.0.10.

## NMsim 0.0.9

CRAN release: 2024-02-07

NMsim 0.0.9 is almost identical to 0.0.8 but ensures compatibility with
older R versions.

### Bugfixes

- In some cases `NMreadSim` would not be able to read and combine
  results from models that returned different data variables. Fixed.

## NMsim 0.0.8

CRAN release: 2024-02-07

### New features

- `NMsim` 0.0.1 would generate an `rds` file with paths to simulation
  files and results for each model+data set simulated. This has been
  changed to now only generate one table per model. This makes it
  simpler to read simulation results in some cases.

- `NMreadSim` should now be the best and only way for the user to read
  `NMsim` simulation results. It interprets `rds` files (which are the
  ones intended for reading), `fst` files, tables of `NMsim` runs, and
  `NMsim` results. This makes it less confusing what can be processed by
  `NMreadSim` and also it sometimes easier to generalize code reading
  simulation results. Also, `NMsim` now always reads results using
  `NMreadSim`. This has the advantage that an fst file will always be
  produced if `NMsim` waits to read the results.

- `NMreadSim` has a new argument, `check.time` by default disabling
  checking whether a collected `fst` file is newer than the results
  files generated by `NMsim`. Normally, it’s a good thing to check this
  but some ways of sharing file files may not retain file modification
  times needed to check for this. `NMsim` will delete the `fst` files if
  it finds any so normally it should not be a problem to skip this
  check.

- `modify.model` is the argument to use to modify the control stream
  after `NMsim` is done preparing the simulation. A couple of helper
  functions are available making it really easy to add contents (very
  commonly used) or modify contents.

- `NMsim` now tries to reuse stored results if `reuse.results=TRUE`. It
  does so in a simple way - if they exist, they will be attempted read -
  so be careful to rerun simulations without this option if you change
  any arguments.

- `NMsim` will by default add a `DV` column with `NA` values if `DV` is
  not in input data. Nonmem most often needs that column, and it is
  uninformative for simulations. Disable this feature by using
  `auto.dv=FALSE`.

- The `transform` option has been integrated into the table of
  simulations created by
  [`NMsim()`](https://nmautoverse.github.io/NMsim/reference/NMsim.md).
  This means even if the results are not read by `NMsim` (because the
  simulation is not executed or it is submitted to a cluster), the
  transformation will still be applied by
  [`NMreadSim()`](https://nmautoverse.github.io/NMsim/reference/NMreadSim.md)
  later.

- `NMsim()'s` `dir.sims` and `dir.res` arguments can be controlled using
  [`NMdata::NMdataConf()`](https://nmautoverse.github.io/NMdata/reference/NMdataConf.html).
  Often these two arguments are used all the time, so it’s convenient to
  be able to configure those once and for all in a script.

### Bugfixes

- `NMreadSim` was only able to read results if the current working
  directory was the same as when `NMsim` was executed. Now fixed.

- In some cases `NMsim` would fail on models with multiple output tables
  when the `table.vars` argument was not used. Fixed.

- `NMsim`’s `sim.dir.from.scratch` argument was not respected due to a
  simple bug, leading to `dir.sims` growing each time a simulation was
  rerun.

- In case simulation data is a list of data sets `NMsim` would not order
  columns when `order.columns` was `TRUE`. Now fixed.

- In case of lists of data sets, and the list element (data set) names
  included spaces,
  [`NMsim()`](https://nmautoverse.github.io/NMsim/reference/NMsim.md)
  would throw and error. Spaces in data set names are now replaced with
  under scores (“\_“) to avoid that. It will often happen when data sets
  are split into lists using
  [`data.table::split.data.table()`](https://rdrr.io/pkg/data.table/man/split.html) -
  which is an excellent way to do this, by the way.

- Function
  [`simPopEtas()`](https://nmautoverse.github.io/NMsim/reference/simPopEtas.md)
  was not exported, so only available as `NMsim:::simPopEtas()`. Fixed.

## NMsim 0.0.7

CRAN release: 2024-01-08

### New features

- Function
  [`simPopEtas()`](https://nmautoverse.github.io/NMsim/reference/simPopEtas.md)
  to generate a population from a model. The population can be saved as
  a `phi` file to be reused in subsequent simulations. The function is
  by mistake not exported in 0.0.7 so for now you must use
  `NMsim:::simPopEtas()` to use it.

- Function
  [`NMreadSim()`](https://nmautoverse.github.io/NMsim/reference/NMreadSim.md)
  provides a very simple interface to reading simulation results.
  Especailly in cases where the simulation is being parallelized or
  otherwise spawns multiple Nonmem jobs, this is a useful feature.

- A list of simulation data sets will now be simulated with separate
  Nonmem runs. This is an efficient way to parellelize large simulation
  runs.

## NMsim 0.0.6

CRAN release: 2023-11-28

### New features

- Support for parallelization of simulations added when using PSN. It
  used to be possible to run multiple simulations simultaneously in
  separate threads. Now single simulation runs can be parallelized on
  `sge` type clusters (using `qsub`). See arguments `sge` and `nc`.

### Bugfixes

- A simple mistake would create problems in
  [`genPhiFile()`](https://nmautoverse.github.io/NMsim/reference/genPhiFile.md)
  when having more than 10 ETAs in a model. Now fixed.

## NMsim 0.0.5

CRAN release: 2023-11-11

### New features

- Full support for models estimated with SAEM. Especially, simulation of
  “known” subjects, i.e. re-using emperical Bayes estimates, is slightly
  different with these models.

- Experimental support for windows with PsN. `dir.psn` argument has to
  point to a directory where executables `execute` and `update_inits`
  are found. Thanks to Sjoerd Koopman for debugging and testing this.
  Hopefully in future versions, `PsN` will not be needed on Windows
  (like it is not needed on Linux).

- The simulation method called NMsim_known now accepts other `.phi`
  files to use than the .phi file generated by the estimation run. This
  is useful if one wants to reuse subjects generated in a previous
  simulation.

### Other/minor improvements

- NMexec now also copies out the shk (shrinkage estimates) file after a
  run. The files that will by default be copied and reported next to the
  control streams are now `xml`, `ext`, `cov`, `cor`, `coi`, `phi`,
  `shk` - in addition to output table files and the archived input data
  of course.

## NMsim 0.0.2

CRAN release: 2023-09-14

### New features

- NMsim supports `type.sim="typical"` which means all OMEGAS will be
  fixed to zero. This requires the ext file to be present.

- Experimental support for simulation of estimated subjects using
  `type.sim="known"`.
