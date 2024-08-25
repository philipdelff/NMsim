##' Simulate from an estimated Nonmem model
##'
##' Supply a data set and an estimation input control stream, and
##' NMsim can create neccesary files (control stream, data files), run
##' the simulation and read the results. It has additional methods for
##' other simulation types available, can do multiple simulations at
##' once and more. Please see vignettes for an introduction to how to
##' get the most out of this.
##'
##' @param file.mod Path(s) to the input control stream(s) to run the
##'     simulation on. The output control stream is for now assumed to
##'     be stored next to the input control stream and ending in .lst
##'     instead of .mod. The .ext file must also be present. If
##'     simulating known subjects, the .phi is necessary too.
##' @param data The simulation data as a \code{data.frame} or a list
##'     of \code{data.frame}s. If a list, the model(s) will be run on
##'     each of the data sets in the list.
##' @param dir.sims The directory in which NMsim will store all
##'     generated files. Default is to create a folder called `NMsim`
##'     next to `file.mod`.
##' @param name.sim Give all filenames related to the simulation a
##'     suffix. A short string describing the sim is recommended like
##'     "ph3_regimens".
##' @param order.columns reorder columns by calling
##'     \code{NMdata::NMorderColumns} before saving dataset and
##'     running simulations? Default is TRUE.
##' @param script The path to the script where this is run. For
##'     stamping of dataset so results can be traced back to code.
##' @param subproblems Number of subproblems to use as
##'     \code{SUBPROBLEMS} in \code{$SIMULATION} block in Nonmem. The
##'     default is subproblem=0 which means not to use
##'     \code{SUBPROBLEMS}.
##' @param reuse.results If simulation results found on file, should
##'     they be used? If TRUE and reading the results fail, the
##'     simulations will still be rerun.
##' @param transform A list defining transformations to be applied
##'     after the Nonmem simulations and before plotting. For each
##'     list element, its name refers to the name of the column to
##'     transform, the contents must be the function to apply.
##' @param seed.R A value passed to \code{set.seed()}. It may be
##'     better use \code{seed.R} rather than calling \code{set.seed()}
##'     manually because the seed can then be captured and stored by
##'     \code{NMsim()} for reproducibility. See \code{seed.nm} for
##'     finer control of the seeds that are used in the Nonmem control
##'     streams.
##' @param seed.nm Control Nonmem seeds. If a numeric, a vector or a
##'     `data.frame`, these are used as the the seed values (a single
##'     value or vector will be recycled so make sure the dimesnsions
##'     are right, the number of columns in a \code{data.frame} will
##'     dictate the number of seeds in each Nonmem control stream. Use
##'     a list with elements `values`, and `dist` and others for
##'     detailed control of the random sources. See \code{?NMseed} for
##'     details on what arguments can be passed this way.
##'
##' Default is to draw seeds betwen
##'     0 and 2147483647 (the values supported by Nonmem) for each
##'     simulation. You can pass a function that will be evaluated
##'     (say to choose a different pool of seeds to draw from).
##'
##' To avoid changing an exisiting seed in a control stream, use
##' \code{seed.nm="asis"}.
##'
##' In case \code{method.sim=NMsim_EBE}, seeds are not used.
##'
##' @param seed Deprecated. See \code{seed.R} and \code{seed.nm}.
##' @param args.psn.execute A charachter string that will be passed as
##'     arguments PSN's `execute`.
##' @param table.vars Variables to be printed in output table as a
##'     character vector or a space-separated string of variable
##'     names. The default is to export the same tables as listed in
##'     the input control stream. If \code{table.vars} is provided,
##'     all output tables in estimation control streams are dropped
##'     and replaced by a new one with just the provided variables. If
##'     many variables are exported, and much fewer are used, it can
##'     speed up NMsim significantly to only export what is needed
##'     (sometimes this is as little as "PRED IPRED"). Nonmem writes
##'     data slowly so reducing output data can make a big difference
##'     in execution time. See \code{table.options} too.
##' @param table.options A character vector or a string of
##'     space-separated options. Only used if \code{table.vars} is
##'     provided. If constructing a new output table with
##'     \code{table.vars} the default is to add two options,
##'     \code{NOAPPEND} and \code{NOPRINT}. You can modeify that with
##'     \code{table.options}. Do not try to modify output filename -
##'     \code{NMsim} takes care of that.
##' @param text.table A character string including the variables to
##'     export from Nonmem.
##' @param text.sim A character string to be pasted into
##'     $SIMULATION. This must not contain seed or SUBPROBLEM which
##'     are handled separately. Default is to include "ONLYSIM". To
##'     avoid that, use text.sim="".
##' @param method.sim A function (not quoted) that creates the
##'     simulation control stream and other necessary files for a
##'     simulation based on the estimation control stream, the data,
##'     etc. The default is called \code{NMsim_default} which will
##'     replace any estimation and covariance step by a simulation
##'     step. See details section on oter methods, and see examples
##'     and especially vignettes on how to use the different provided
##'     methods.
##' @param typical Run with all ETAs fixed to zero? Technically all ETAs=0 is obtained by replacing
##' \code{$OMEGA} by a zero matrix. Default is FALSE. 
##' @param execute Execute the simulation or only prepare it?
##'     `execute=FALSE` can be useful if you want to do additional
##'     tweaks or simulate using other parameter estimates.
##' @param sge Submit to cluster? Default is not to, but this is very
##'     useful if creating a large number of simulations,
##'     e.g. simulate with all parameter estimates from a bootstrap
##'     result.
##' @param col.row Only used if data is not supplied (which is most
##'     likely for simulations for VPCs) A column name to use for a
##'     row identifier. If none is supplied,
##'     \code{NMdataConf()[['col.row']]} will be used. If the column
##'     already exists in the data set, it will be used as is, if not
##'     it will be added.
##' @param method.execute Specify how to call Nonmem. Options are
##'     "psn" (PSN's execute), "nmsim" (an internal method similar to
##'     PSN's execute), and "direct" (just run Nonmem directly and
##'     dump all the temporary files). "nmsim" has advantages over
##'     "psn" that makes it the only supported method when
##'     type.sim="NMsim_EBE". "psn" has the simple advantage that
##'     the path to nonmem does not have to be specified if "execute"
##'     is in the system search path. So as long as you know where
##'     your Nonmem executable is, "nmsim" is recommended. The default
##'     is "nmsim" if path.nonmem is specified, and "psn" if not.
##' @param nc Number of cores used in parallelization. This is so far
##'     only supported with \code{method.execute="psn"}.
##' @param method.update.inits The initial values of all parameters
##'     are by updated from the estimated model before running the
##'     simulation. NMsim can do this with a native function or use
##'     PSN to do it - or the step can be skipped to not update the
##'     values. The possible values are
##'
##' \itemize{
##'
##' \item{"psn"}
##'     uses PSN's "update_inits". Requires a functioning PSN
##'     installation and possibly that \code{dir.psn} is correctly
##'     set. The advantages of this method are that it keeps comments
##'     in the control stream and that it is a method known to many.
##'
##' \item{"nmsim"}
##'  Uses a simple internal method to update the parameter values
##' based on the ext file.  The advantages of "nmsim" are it does not
##' require PSN, and that it does not rely on code-interpretation for generation of simulation control streams. "nmsim" fixes the whole
##' OMEGA and SIGMA matrices as single blocks making the $OMEGA and
##' $SIGMA sections of the control streams less easy to read. On the
##' other hand, this method is robust because it avoids any
##' interpretation of BLOCK structure or other code in the control
##' streams.
##'
##' \item{"none"} Do nothing. This is useful if the model to simulate
##' has not been estimated but parameter values have been manually put
##' into the respective sections in the control stream.
##'
##' On linux/mac, The default is to use "PSN" if found. On Windows, "nmsim"
##' is the default.
##' 
##' }
##' 
##' @param dir.psn The directory in which to find PSN's executables
##'     ('execute' and 'update_inits'). The default is to rely on the
##'     system's search path. So if you can run 'execute' and
##'     'update_inits' by just typing that in a terminal, you don't
##'     need to specify this unless you want to explicitly use a
##'     specific installation of PSN on your system.
##' @param path.nonmem The path to the Nonmem executable to use. The
##'     could be something like "/usr/local/NONMEM/run/nmfe75" (which
##'     is a made up example). No default is available. You should be
##'     able to figure this out through how you normally execute
##'     Nonmem, or ask a colleague.
##' @param modify.model Named list of additional control stream
##'     section edits. Note, these can be functions that define how to
##'     edit sections. This is an advanced feature which is not needed
##'     to run most simulations. It is however powerful for some types
##'     of analyses, like modifying parameter values. See vignettes
##'     for further information.
##' @param list.sections Deprecated. Use modify.model instead.
##' @param create.dirs If the directories specified in dir.sims and
##'     dir.res do not exists, should it be created? Default is TRUE.
##' @param sim.dir.from.scratch If TRUE (default) this will wipe the
##'     simulation directory before running new simulations. The
##'     directory that will be emptied is _not_ dir.sims where you may
##'     keep many or all your simulations. It is the subdirectory
##'     named based on the run name and \code{name.sim}. The reason it
##'     is advised to wipe this directory is that if you in a previous
##'     simulation created simulation runs that are now obsolete, you
##'     could end up reading those too when collecting the
##'     results. NMsim will delete previously generated simulation
##'     control streams with the same name, but this option goes
##'     further. An example where it is important is if you first ran
##'     1000 replications, fixed something and now rand 500. If you
##'     choose FALSE here, you can end up with the results of 500 new
##'     and 500 old simulations.
##' @param file.ext Optionally provide a parameter estimate file from
##'     Nonmem. This is normally not needed since `NMsim` will by
##'     default use the ext file stored next to the input control
##'     stream (replacing the file name extension with `.ext`). If
##'     using method.update.inits="psn", this argument cannot be
##'     used. If you want provide parameters to be used for the
##'     simulation, look at the `ext` argument to `NMsim_VarCov`.
##' @param auto.dv Add a column called `DV` to simulation data sets if
##'     a column of that name is not found? Nonmem is generally
##'     dependent on a `DV` column in input data but this is typically
##'     uninformative in simulation data sets and hence easily
##'     forgotten when generating simulation data sets.
##' @param as.fun The default is to return data as a data.frame. Pass
##'     a function (say `tibble::as_tibble`) in as.fun to convert to
##'     something else. If data.tables are wanted, use
##'     as.fun="data.table". The default can be configured using
##'     NMdataConf.
##' @param args.NMscanData If \code{execute=TRUE&sge=FALSE}, NMsim
##'     will normally read the results using \code{NMreadSim}. Use
##'     this argument to pass additional arguments (in a list) to that
##'     function if you want the results to be read in a specific
##'     way. This can be if the model for some reason drops rows, and
##'     you need to merge by a row identifier. You would do
##'     `args.NMscanData=list(col.row="ROW")` to merge by a column
##'     called `ROW`. This is only used in rare cases.
##'
##' @param system.type A charachter string, either \"windows\" or
##'     \"linux\" - case insensitive. Windows is only experimentally
##'     supported. Default is to use \code{Sys.info()[["sysname"]]}.
##' @param suffix.sim Deprecated. Use name.sim instead.
##' @param dir.res Provide a path to a directory in which to save rds
##'     files with paths to results. Default is to use dir.sims. After
##'     running `NMreadSim()` on these files, the original simulation
##'     files can be deleted. Hence, providing both `dir.sims` and
##'     `dir.res` provides a structure that is simple to
##'     clean. `dir.sims` can be purged when `NMreadSim` has been run
##'     and only small `rds` and `fst` files will be kept in
##'     `dir.res`. Notice, in case multiple models are simulated,
##'     multiple `rds` (to be read with `NMreadSim()`) files will be
##'     created by default. In cases where multiple models are
##'     simulated, see `file.res` to get just one file refering to all
##'     simulation results.
##' @param file.res Path to an rds file that will contain a table of
##'     the simulated models and other metadata. This is needed for
##'     subsequently retrieving all the results using
##'     `NMreadSim()`. The default is to create a file called
##'     `NMsim_..._MetaData.rds` under the \code{dir.res} directory
##'     where ... is based on the model name. However, if multiple
##'     models (\code{file.mod}) are simulated, this will result in
##'     multiple rds files. Specifying a path ensures that one rds
##'     file containing information about all simulated models will be
##'     created. Notice if \code{file.res} is supplied, \code{dir.res}
##'     is not used.
##' @param wait Wait for simulations to finish? Default is to do so if
##'     simulations are run locally but not to if they are sent to the
##'     cluster. Waiting for them means that the results will be read
##'     when simulations are done. If not waiting, path(s) to `rds`
##'     files to read will be returned. Pass them through
##'     `NMreadSim()` (which also supports waiting for the simulations
##'     to finish).
##' @param clean The degree of cleaning (file removal) to do after
##'     Nonmem execution. If `method.execute=="psn"`, this is passed
##'     to PSN's `execute`. If `method.execute=="nmsim"` a similar
##'     behavior is applied, even though not as granular. NMsim's
##'     internal method only distinguishes between 0 (no cleaning),
##'     any integer 1-4 (default, quite a bit of cleaning) and 5
##'     (remove temporary dir completely).
##' @param quiet If TRUE, messages from what is going on will be
##'     suppressed.
##' @param nmquiet Silent console messages from Nonmem? The default
##'     behaviour depends. It is FALSE if there is only one model to
##'     execute and `progress=FALSE`.
##' @param progress Track progress? Default is `TRUE` if `quiet` is
##'     FALSE and more than one model is being simulated. The progress
##'     tracking is based on the number of models completed, not the
##'     status of the individual models.
##' @param check.mod Check the provided control streams for contents
##'     that may cause issues for simulation. Default is `TRUE`, and
##'     it is only recommended to disable this if you are fully aware
##'     of such a feature of your control stream, you know how it
##'     impacts simulation, and you want to get rid of warnings.
##' @param format.data.complete For development purposes - users do
##'     not need this argument. Controls what format the complete
##'     input data set is saved in.  Possible values are `rds`
##'     (default), `fst` (experimental) and `csv`. `fst` may be faster
##'     and use less disk space but factor levels may be lost from
##'     input data to output data. `csv` will also lead to loss of
##'     additional information such as factor levels.
##' @param ... Additional arguments passed to \code{method.sim}.
##' @return A data.frame with simulation results (same number of rows
##'     as input data). If `sge=TRUE` a character vector with paths to
##'     simulation control streams.
##' @details Loosely speaking, the argument \code{method.sim} defines
##'     _what_ NMsim will do, \code{method.execute} define _how_ it
##'     does it. \code{method.sim} takes a function that converts an
##'     estimation control stream into whatever should be
##'     run. Features like replacing `$INPUT`, `$DATA`, `$TABLE`, and
##'     handling seeds are NMsim features that are done in addition to
##'     the \code{method.sim}. Also the \code{modeify.model} argument
##'     is handled in addition to the \code{method.sim}. The
##'     \code{subproblems} and \code{seed} arguments are available to
##'     all methods creating a \code{$SIMULATION} section.
##'
##' Notice, the following functions are internally available to
##' `NMsim` so you can run them by say \code{method.sim=NMsim_EBE}
##' without quotes. To see the code of that method, type
##' \code{NMsim_EBE}.
##' 
##' \itemize{
##' 
##' \item \code{NMsim_default} The default behaviour. Replaces any
##' $ESTIMATION and $COVARIANCE sections by a $SIMULATION section.
##'
##' \item \code{NMsim_asis} The simplest of all method. It does nothing (but
##' again, \code{NMsim} handles `$INPUT`, `$DATA`, `$TABLE` and
##' more. Use this for instance if you already created a simulation
##' (or estimation actually) control stream and want NMsim to run it
##' on different data sets.
##'
##' \item \code{NMsim_typical} Deprecated. Use \code{typical=TRUE} instead. 
##' 
##' \item \code{NMsim_EBE} Simulates _known_ ETAs. By default, the ETA
##' values are automatically taken from the estimation run. This is
##' what is refered to as emperical Bayes estimates, hence the name of
##' the method "NMsim_EBE". However, the user can also provide a
##' different `.phi` file which may contain simulated ETA values (see
##' the `file.phi` argument). ID values in the simulation data set
##' must match ID values in the phi file for this step to work. If
##' refering to estimated subjects, the .phi file from the estimation
##' run must be found next to the .lst file from the estimation with
##' the same file name stem (say `run1.lst` and `run1.phi`). Again, ID
##' values in the (simulation) input data must be ID values that were
##' used in the estimation too. The method Runs an \code{$ESTIMATION
##' MAXEVAL=0} but pulls in ETAs for the ID's found in data. No
##' \code{$SIMULATION} step is run which unfortunately means no
##' residual error will be simulated.
##'
##' \item \code{NMsim_VarCov} Like \code{NMsim_default} but `$THETA`,
##' `$OMEGA`, and `SIGMA` are drawn from distribution estimated in
##' covariance step. This means that a successful covariance step must
##' be available from the estimation. NB. A multivariate normal
##' distribution is used for all parameters, including `$OMEGA` and
##' `$SIGMA` which is not the correct way to do this. In case the
##' simulation leads to negative diagonal elements in $OMEGA and
##' $SIGMA, those values are truncated at zero. This method is only
##' valid for simulation of `$THETA` variability. The method accepts a
##' table of parameter values that can be produced with other tools
##' than `NMsim`. For simulation with parameter variability based on
##' bootstrap results, use \code{NMsim_default}.
##'
##' }
##' @import NMdata
##' @import data.table
##' @import utils 
##' @importFrom stats runif
##' @importFrom xfun relative_path
##' @export



NMsim <- function(file.mod,data,dir.sims, name.sim,
                  order.columns=TRUE,
                  file.ext=NULL,
                  ## tab.ext=NULL,
                  script=NULL,subproblems=NULL,
                  reuse.results=FALSE,
                  seed.R,
                  seed.nm,
                  args.psn.execute,
                  table.vars,
                  table.options,
                  text.sim="",
                  method.sim=NMsim_default,
                  typical=FALSE,
                  execute=TRUE,sge=FALSE,
                  nc=1,transform=NULL,
                  method.execute,
                  method.update.inits,
                  create.dirs=TRUE,dir.psn,
                  modify.model,
                  sim.dir.from.scratch=TRUE,
                  col.row,
                  args.NMscanData,
                  path.nonmem=NULL,
                  nmquiet,
                  progress,
                 as.fun,
                 suffix.sim,
                 text.table,
                 system.type=NULL,
                 dir.res,
                 file.res,
                 wait,
                 auto.dv=TRUE,
                 clean,
                 quiet=FALSE,
                 check.mod = TRUE,
                 seed,
                 list.sections,
                 format.data.complete="rds",
                 ...
                  ){
#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####
    
    . <- NULL
    ..name.sim <- NULL
    ..dir.res <- NULL
    DATAROW <- NULL
    data.name <- NULL
    default <- NULL
    direct <- NULL
    directory <- NULL
    dir.data.sim <- NULL
    dir.sim <- NULL
    DV <- NULL
    est <- NULL
    fn.data <- NULL
    f.exists <- NULL
    files.needed <- NULL
    files.res <- NULL
    fn.sim.tmp <- NULL
    fn <- NULL
    fn.mod <- NULL
    fn.sim <- NULL
    i <- NULL
    ID <- NULL
    is.data <- NULL
    known <- NULL
    MDV <- NULL
    model <- NULL
    mod <- NULL
    name.mod <- NULL
    NEWMODEL <- NULL
    nmsim <- NULL
    n <- NULL
    none <- NULL
    psn <- NULL
    par.type <- NULL
    path.rds <- NULL
    path.results <- NULL
    pathResFromists <- NULL
    pathSimsFromRes <- NULL
    path.sim <- NULL
    path.digests <- NULL
    path.sim.lst <- NULL
    path.data <- NULL
    ROW <- NULL
    ROWMODEL <- NULL
    ROWMODEL2 <- NULL
    rowtmp <- NULL
    run.mod <- NULL
    run.sim <- NULL
    sim <- NULL
    tab.ext <- NULL
    text <- NULL
    textmod <- NULL
    ##typical <- NULL
    value <- NULL
    variable <- NULL
    
    funs.transform <- NULL
    lst <- NULL
    NMsimTime <- NULL
    NMsimVersion <- NULL
    fn.sim.predata <- NULL
    path.rds.exists <- NULL
    pathResFromSims <- NULL
    
    ## Section end: Dummy variables, only not to get NOTE's in pacakge checks

    returnSimres <- function(simres){
        simres <- as.fun(simres)
        addClass(simres,"NMsimRes")
        return(simres)
    }
    
#### Section start: Checking aguments ####
    
    if(missing(file.mod)) stop("file.mod must be supplied. It must be one or more paths to existing control streams.")
    if(any(!file.exists(file.mod))) stop("All elements in file.mod must be paths to existing input control streams.")
    ## Check control streams for potential problems
    
    if(check.mod){
        lapply(file.mod,NMsimCheckMod)
    }
    if(missing(data)) data <- NULL


    if(missing(args.NMscanData)) args.NMscanData <- NULL
    if(!is.null(args.NMscanData)){
        if(!is.list(args.NMscanData)) stop("args.NMscanData must be a list.")
        if(any(names(args.NMscanData)=="")) stop("All elements in args.NMscanData must be named.")
    }
    args.NMscanData.default <- list(merge.by.row=FALSE)
    
    if(missing(progress)) progress <- NULL
    
    if(missing(dir.psn)) dir.psn <- NULL
    if(missing(path.nonmem)) path.nonmem <- NULL
    if(missing(method.execute)) method.execute <- NULL
    NMsimConf <- NMsimTestConf(path.nonmem=path.nonmem,dir.psn=dir.psn,method.execute=method.execute,must.work=execute)

    
    ## after definition of wait and wait.exec, wait is used by
    ## NMreadSim(), wait.exec used by NMexec().
    if(missing(wait)) wait <- !sge
    wait.exec <- !sge && wait
    ## If we already wait on the simulation, no reason to wait for
    ## data. Especially, if NMTRAN fails in exec, NMreadSim() will
    ## wait indefinitely.
    if(wait.exec) wait <- FALSE


    if(nc>1){message("nc>1 may not work depending on your system configuration. It has only been tested on linux. Please notice there are other and most often more efficient methods to speed up simulations. See discussions on the NMsim website.")}
    

    ## args.psn.execute
    if(missing(args.psn.execute)) args.psn.execute <- NULL
    ## args.psn.execute <- simpleCharArg("args.psn.execute"
    ##                                  ,args.psn.execute
    ##                                  ,default="-clean=5 -model_dir_name -nm_output=xml,ext,cov,cor,coi,phi"
    ##                                  ,accepted=NULL
    ##                                  ,clean=FALSE
    ##                                  ,lower=FALSE)
    if(missing(clean)) clean <- 1
    

    if(missing(file.ext)) file.ext <- NULL
    method.update.inits <- adjust.method.update.inits(method.update.inits,system.type=NMsimConf$system.type,dir.psn=NMsimConf$dir.psn,cmd.update.inits=cmd.update.inits,file.ext=file.ext)
    
    

### seed.R
    if(missing(seed.R)) seed.R <- NULL
    if(!is.null(seed.R)){
        set.seed(seed.R)
    }
    if(missing(seed.nm)) seed.nm <- NULL
    
    ## seed.nm
    ## seed is deprecated
    if(!missing(seed)){
        if(!is.null(seed.nm)){
            stop("`seed` and `seed.nm` supplied. Use `seed.nm` and not the deprecated `seed`.")
        }
        message("`seed` is deprecated. Use `seed.nm`. See argument `seed.R` too.")
        seed.nm <- seed
        seed <- NULL
    }


    
    ## if(missing(seed)) seed <- NULL
    ## arg.seed is the user-supplied seed. Don't confuse with seed.args
### in case of "asis", how should the user be allowed to disable touching the seed? seed.nm="asis"? 
    arg.seed.nm <- seed.nm
    do.seed <- TRUE
    if( (is.logical(seed.nm) && seed.nm==FALSE) ||
        (is.character(seed.nm) && tolower(seed.nm)=="asis")){
        do.seed <- FALSE
    }
    if(is.numeric(seed.nm) || is.data.frame(seed.nm)){
        seed.nm <- list(values=seed.nm)
    }

    
    ## name.sim
    if(!missing(suffix.sim)){
        if(!missing(name.sim)){
            stop("name.sim and suffix.sim supplied. Use name.sim and not the deprecated suffix.sim. ")
        }
        message("suffix.sim is deprecated. Use name.sim.")
        name.sim <- suffix.sim
    }
    if(missing(name.sim)) name.sim <- NULL
    name.sim <- simpleCharArg("name.sim",name.sim,"noname",accepted=NULL,lower=FALSE,clean=FALSE)
    name.sim.paths <- cleanStrings(name.sim)

    ## modelname
    ## if(missing(modelname)){
    modelname <- NULL
    ## }
    
    if(missing(col.row)) col.row <- NULL
    col.row <- NMdata:::NMdataDecideOption("col.row",col.row)

    ## as.fun
    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdata:::NMdataDecideOption("as.fun",as.fun)
    
    input.archive <- inputArchiveDefault

    if(missing(modify.model)) modify.model <- NULL
    if(!missing(list.sections)){
        if(!is.null(modify.model)){
            stop("both list.sections (deprecated argument) and modify.model supplied. Please use only modify.model.")
        }
        message("\'list.sections\' is deprecated. Please use \'modify.model\'.")
    }


###  Section end: Checking aguments

    ## if(!is.null(transform) && !transform!=FALSE) {message("transform is CURRENTLY NOT SUPPORTED. Will be back in the future.")}
    warn.notransform <- function(transform){
        if(is.null(transform)) return(invisible(NULL))
        warning("`transform` (argument) ignored since NMsim is not reading the simulation results.")
    }
    

    if(missing(table.vars)) table.vars <- NULL
    if(missing(table.options)) table.options <- NULL
### generate text.table as the combination of table.vars and table.options
    if(missing(text.table) || is.null(text.table)){
        if(missing(table.options)||is.null(table.options)){
            table.options <- c("NOPRINT","NOAPPEND")
        }
        if(!is.null(table.vars)){
            text.table <- paste(paste(table.vars,collapse=" "),paste(table.options,collapse=" "))
        }
    } else{
        if(!is.null(table.vars) || !is.null(table.options)){
            stop("argument \'text.table\' is deprecated. Please use \'table.vars\' and/or \'table.options\' instead.")
        }
        message("argument \'text.table\' is deprecated. Please use \'table.vars\' and/or \'table.options\' instead.")
    }

    if(missing(nmquiet)) nmquiet <- NULL

    if(missing(modelname)) modelname <- NULL
    ## modelname <- NMdataDecideOption("modelname",modelname)
    if(is.null(modelname)) modelname <- function(fn) fnExtension(basename(fn),"")

    
#### Section start: Defining additional paths based on arguments ####

    ## dir.sim
    
    if(missing(dir.sims)) dir.sims <- NULL
    dir.sims <- try(NMdata:::NMdataDecideOption("dir.sims",dir.sims,allow.unknown=TRUE),silent=TRUE)
    if(inherits(dir.sims,"try-error")){
        dir.sims <- NULL
    }
    ## dir.sims <- simpleCharArg("dir.sims",dir.sims,default=NULL,accepted=NULL,lower=FALSE)
    dir.sims <- simpleCharArg("dir.sims",dir.sims,file.path(dirname(file.mod),"NMsim"),accepted=NULL,lower=FALSE)
    
    if(!dir.exists(dir.sims)){
        if(!create.dirs){
            stop(paste("dir.sims does not point to an existing directory. dir.sims is\n",NMdata:::filePathSimple(dir.sims)))
        }
        res.dc <- tryCatch(dir.create(dir.sims),warning=function(w)w)
        if("warning"%in%class(res.dc)){
            stop("Problems creating dir.sims. Please check that the parent directory exists and is writable.")
        }
    }
    
    if(missing(file.res)) file.res <- NULL
    if(is.null(file.res)) {

        if(missing(dir.res)) dir.res <- NULL
        dir.res <- try(NMdata:::NMdataDecideOption("dir.res",dir.res,allow.unknown=TRUE),silent=TRUE)
        if(inherits(dir.res,"try-error")){
            dir.res <- NULL
        }
        dir.res <- simpleCharArg("dir.res",dir.res,default=dir.sims,accepted=NULL,lower=FALSE)
        
        ## if(missing(dir.res) || is.null(dir.res)) dir.res <- dir.sims
    } else {
        dir.res <- dirname(file.res)
    }
    
    if(!dir.exists(dir.res)){
        if(!create.dirs){
            stop(paste("dir.res does not point to an existing directory. dir.res is\n",NMdata:::filePathSimple(dir.res)))
        }
        ## dir.create(dir.res)
        res.dc <- tryCatch(dir.create(dir.res),warning=function(w)w)
        if("warning"%in%class(res.dc)){
            message(dir.res)
            stop("Problems creating dir.res. Please check that the parent directory exists and is writable.")
        }
        
    }

    
    relpathResFromSims <- relative_path(dir.res,dir.sims)
    relpathSimsFromRes <- relative_path(dir.sims,dir.res)
    
    if(missing(text.table)) text.table <- NULL
    
    if(missing(subproblems)|| is.null(subproblems)) subproblems <- 0

    if(subproblems>0 &&
       !is.null(table.vars)
       ## this has not been resolved in NMdata
       ## && packageVersion("NMdata")<"1.1.7"
       ){
        tabv2 <- paste(table.vars,collapse=" ")
        tabv2 <- gsub(" +"," ",tabv2 )
        if(length(strsplit(tabv2," ")[[1]])<3){
            message("Using less than three variables in table.vars in combination with subproblems may cause issues with NMdata versions <=0.1.6. If you get an error, try to add any variable or two to table.vars.")
        }
    }
    
    dt.models <- data.table(file.mod=file.mod)
    dt.models[,run.mod:=fnExtension(basename(file.mod),"")]
    dt.models[,name.mod:=run.mod]
    dt.models[,pathResFromSims:=relpathResFromSims]
    dt.models[,pathSimsFromRes:=relpathSimsFromRes]
    dt.models[,NMsimVersion:=packageVersion("NMsim")]
    dt.models[,NMsimTime:=Sys.time()]
    if(!is.null(names(file.mod))){
        
        names.mod <- names(file.mod)
        names.mod[names.mod==""] <- file.mod[names.mod==""]
        dt.models[,name.mod:=names.mod]
        rm(names.mod)
    }
    dt.models[,ROWMODEL:=.I]


### file.ext
    if(is.null(file.ext)) {
        dt.models[,file.ext:=fnExtension(file.mod,"ext"),by=.(ROWMODEL)]
    } else {
        if(length(file.ext) != length(file.mod)){
            stop("If `file.ext` is provided, it must be of same length as `file.mod`.")
        }
    }
    

### prepare data sets
    if(!is.null(data) && is.data.frame(data)) {
        ##data <- list(copy(data))
        data <- list(data)
        data <- lapply(data,as.data.table)

    }
    if(!is.null(data) && is.list(data) && !is.data.frame(data)) {
        names.data <- names(data)
        if(is.null(names.data)) {
            names.data <- as.character(1:length(data))
        } else if(""%in%names.data) {
            names.data <- gsub(" ","_",names.data)
            if(any(duplicated(names.data))) stop("If data is a list of data sets, the list elements must be uniquely named.")
            names.data[names.data==""] <- as.character(which(names.data==""))
        }

        ## data sets must not be empty
        if(any(sapply(data,function(x)x[,.N==0]))){
            stop("Empty data set provided. If `data` is a list of data sets, make sure all of them are non-empty.")
        }

        col.sim <- tmpcol(names=sapply(data,names),base="sim")
        if(col.sim != "sim") message(sprintf("column sim exists, name.sim written to column %s instead.",col.sim))
        data <- lapply(data,function(x) x[,(col.sim):=..name.sim])
        
        dt.data <- data.table(DATAROW=1:length(data),data.name=names.data)
        if(dt.data[,.N]==1) dt.data[,data.name:=""]
        dt.models <- egdt(dt.models,dt.data,quiet=TRUE)
        
        dt.models[,ROWMODEL:=.I]
    }
    if(is.null(data)){
        dt.models[,data.name:=""]
    } else {
        if(auto.dv){
            
            data <- lapply(data,function(x){
                if("DV"%in%colnames(x)){
                    x
                } else {
                    x[,DV:=NA_real_]
                    if(!"MDV"%in%colnames(x)){
                        x[,MDV:=1]
                    }
                    x
                }})
        }
        if(order.columns) data <- lapply(data,NMorderColumns)
    }

    

### name.mod and name.sim are confusing. name.mod is the name
### specified for the mod, like
### file.mod=c(ref="run01.mod"). name.sim is the name that was
### given for the who sim.
    
    ## fn.sim is the file name of the simulation control stream created by NMsim
    ## fn.sim <- sub("^run","NMsim",basename(file.mod))
    dt.models[,fn.mod:=basename(file.mod)]
### prepending NMsim to model names
    ## dt.models[,fn.sim:=fnExtension(paste0("NMsim_",name.mod),".mod")]
### dropping NMsim in front of all model names
    dt.models[,fn.sim:=fnExtension(name.mod,".mod")]

    dt.models[,fn.sim.predata:=fnAppend(fn.sim,name.sim.paths)]
    dt.models[,fn.sim:=fnAppend(fn.sim.predata,as.character(data.name)),by=.(ROWMODEL)]
    ## spaces not allowed in model names
    dt.models[,fn.sim:=gsub(" ","_",fn.sim)]
    dt.models[,run.sim:=modelname(fn.sim)]

    
    ## dir.sim is the model-individual directory in which the model will be run
    dt.models[,
              dir.sim:=file.path(dir.sims,paste(name.mod,name.sim.paths,sep="_"))]
    
    
    ## path.sim.tmp is a temporary path to the sim control stream - it
    ## will be moved to path.sim once created.
    dt.models[,fn.sim.tmp:=fnAppend(fn.sim,"tmp")]
    ## path.sim: full path to simulation control stream
    dt.models[,path.sim:=NMdata:::filePathSimple(file.path(dir.sim,fn.sim))]
    

    ## where tosave input data to be read by simulation control stream
    ## fn.data is the data file name, no path
    
    dt.models[,fn.data:=paste0("NMsimData_",fnAppend(fnExtension(name.mod,".csv"),name.sim))]
    dt.models[,fn.data:=fnAppend(fn.data,data.name),by=.(ROWMODEL)]
    ## dt.models[,fn.data:=gsub(" ","_",fn.data)]
    dt.models[,fn.data:=cleanStrings(fn.data)]

    dt.models[,dir.data.sim:=dir.sim]
#### A data-efficient way to store data sets as needed
    if(FALSE){
        dt.models[,dir.data.sim:=file.path(dir.sim,"data")]
    }

    dt.models[,path.data:=file.path(dir.data.sim,fn.data)]
    
    ## path.rds - Where to save table of runs
    if(missing(dir.res)) {
        dt.models[,dir.res:=dir.sim]
    } else {
        dt.models[,dir.res:=..dir.res]
    }
    
    if(is.null(file.res)){
        dt.models[,path.rds:=file.path(dir.res,fnAppend(fnExtension(fn.sim.predata,"rds"),"MetaData"))]
        ## dt.models[,path.rds:=pathSimMeta())]
        dt.models[,path.results:=file.path(dir.res,fnAppend(fnExtension(fn.sim.predata,"fst"),"ResultsData"))]
    } else {
        dt.models[,path.rds:=fnExtension(file.res,"rds")]
        dt.models[,path.results:=fnAppend(fnExtension(file.res,"fst"),"ResultsData")]
    }

### path.rds.exists is whether the metadata rds existed prior to this function call. We don't want to save that in dt.models
    path.rds.exists <- dt.models[,file.exists(path.rds)]
### reading results from prior run
    ## if(reuse.results && all(dt.models[,path.rds.exists==TRUE])){
    ## if(reuse.results && all(dt.models[,path.rds.exists==TRUE])){
    if(reuse.results && all(path.rds.exists==TRUE)){
        if(!quiet) message(sprintf("Reading from simulation results on file:\n%s",dt.models[,paste(unique(path.rds),collapse="\n")]))
        simres <- try(NMreadSim(dt.models[,path.rds],wait=wait,quiet=quiet,progress=progress))
        if(!inherits(simres,"try-error")) {
            return(returnSimres(simres))
        }
    }

### clear simulation directories so user does not end up with old results
    if(sim.dir.from.scratch){
        dt.models[,if(dir.exists(dir.sim)) unlink(dir.sim,recursive=TRUE),by=.(ROWMODEL)]
    }
    dt.models[,if(file.exists(path.sim)) unlink(path.sim),by=.(ROWMODEL)]
    
    dt.models[,{if(!dir.exists(dir.sim)){
                    dir.create(dir.sim)
                }
                    if(!dir.exists(dir.data.sim)){
                        dir.create(dir.data.sim)
                    }   
    },by=.(ROWMODEL)
    ]

###### Messaging to user
    if(!quiet) {
        message(sprintf("Location(s) of intermediate files and Nonmem execution:\n%s",
                        dt.models[,paste(paste0("  ",unique(dir.sim)),collapse="\n")]))
        message(sprintf("Location of final result files:\n%s\n",
                        dt.models[,paste(paste0("  ",unique(dirname(path.rds))),collapse="\n")]))
        ## It would be nice to say how many. But we dont know until after NMsim_method()
        ## message(sprintf("* Writing %d simulation control stream(s) and simulation data set(s)",dt.models[,.N]))
        message(sprintf("* Writing simulation control stream(s) and simulation data set(s)"))
    }
    
### Generate the first version of file.sim.
    ## It would not need to, but beware PSN's update_inits needs to
    ## create a new file - don't try to overwrite an existing one.
    if(method.update.inits=="none"){
        dt.models[,file.copy(file.mod,path.sim),by=ROWMODEL]
    }

##### todo all file.xyz arguments must be NULL or of equal length. And this should be done per model
    
    ## if(!file.exists(file.ext) && method.update.inits!="none"){
    ##     stop("No ext file found. Did you forget to copy it? Normally, NMsim needs that file to find estimated parameter values. If you do not have an ext file and you are running a simulation that does not need it, please use `method.update.inits=\"none\"`")
    ## }
    if(method.update.inits!="none" && any(dt.models[,!file.exists(file.ext)])){
        stop(paste("ext file(s) not found. Did you forget to copy it? Normally, NMsim needs that file to find estimated parameter values. If you do not have an ext file and you are running a simulation that does not need it, please use `method.update.inits=\"none\"`. Was expecting to find ",paste(dt.models[!file.exists(file.ext),file.ext],collapse="\n"),sep=""))
    }
    
    if(method.update.inits=="psn"){
### this next line is done already. I think it should be removed but testing needed.
        cmd.update.inits <- file.psn(NMsimConf$dir.psn,"update_inits")

        dt.models[,
        {
            if(!file.exists(fnExtension(file.mod,"lst"))){
                stop("When using method.update.inits=\"psn\", an output control stream with file name extensions .lst must be located next to the input control stream. Consider also `method.update.inits=\"nmsim\"`.")
            }
            cmd.update <- sprintf("%s --output_model=\"%s\" \"%s\"",normalizePath(cmd.update.inits,mustWork=FALSE),fn.sim.tmp,normalizePath(file.mod))
### would be better to write to another location than next to estimation model
            ## cmd.update <- sprintf("%s --output_model=%s %s",cmd.update.inits,file.path(".",fn.sim.tmp),file.mod)
            if(NMsimConf$system.type=="linux"){
                ## print(paste(cmd.update,"2>/dev/null"))
                
                sys.res <- system(paste(cmd.update,"2>/dev/null"),wait=TRUE)
                
                if(sys.res!=0){
                    stop("update_inits failed. Please look into this. Is the output control stream available? Is it in a directory where you have write-access?")
                }
            }
            if(NMsimConf$system.type=="windows"){
                cmd.update <- sprintf("\"%s\" --output_model=\"%s\" \"%s\"",normalizePath(cmd.update.inits),fn.sim.tmp,normalizePath(file.mod))
                script.update.inits <- file.path(dirname(file.mod),"script_update_inits.bat")
                writeTextFile(cmd.update,script.update.inits)
                sys.res <- shell(shQuote("tmp.bat",type="cmd") )
            }
            
            file.rename(file.path(dirname(file.mod),fn.sim.tmp),path.sim)
        },by=ROWMODEL]
    }
    if(method.update.inits=="nmsim"){
        ## edits the simulation control stream in the
        ## background. dt.models not affected.

### because we use newfile, this will be printed to newfile. If not, it would just return a list of control stream lines.
        dt.models[,NMupdateInits(file.mod=file.mod,newfile=path.sim,fix=TRUE,file.ext=file.ext),by=.(ROWMODEL)]

    }

    
    dt.models[,{
### note: insert test for whether run is needed here
        ## if data is NULL, we will re-use data used in file.mod. Adding row counter if not found.
        rewrite.data.section <- TRUE
        if(is.null(data)){
            data.this <- NMscanInput(file.mod,recover.cols=FALSE,translate=FALSE,apply.filters=FALSE,col.id=NULL)
            ## col.row <- tmpcol(data,base="ROW")
            if(!col.row %in% colnames(data.this)){
                data.this[,(col.row):=.I]
                setcolorder(data.this,col.row)
                message(paste0("Row counter was added in column ",col.row,". Use this to merge output and input data."))
                section.input <- NMreadSection(file.mod,section="input",keep.name=FALSE)
                section.input <- paste("$INPUT",col.row,section.input)
            } else {
                section.input <- FALSE
            }
            add.var.table <- col.row
            args.NMscanData.default$merge.by.row <- TRUE
            args.NMscanData.default$col.row <- col.row
            rewrite.data.section <- FALSE
            order.columns <- FALSE

            col.sim <- tmpcol(data.this,base="sim")
            if(col.sim != "sim") warning(sprintf("column sim exists, name.sim written to column %s instead.",col.sim))
            data.this[,(col.sim):=..name.sim]
        } else {
            data.this <- data[[DATAROW]]

        }

        
### save data and replace $input and $data
#### multiple todo: save only for each unique path.data
        
        
        ## format.data.complete <- "fst"
        nmtext <- NMwriteData(data.this,file=path.data,quiet=TRUE,args.NMgenText=list(dir.data="."),script=script
                             ,formats.write=c("csv",format.data.complete))
        
        ## input
        if(exists("section.input")){
            if(!isFALSE(section.input)){
                NMdata:::NMwriteSectionOne(file0=path.sim,list.sections = list(input=section.input),backup=FALSE,quiet=TRUE)
            }
        } else {
            NMdata:::NMwriteSectionOne(file0=path.sim,list.sections = nmtext["INPUT"],backup=FALSE,quiet=TRUE)
        }
        if(rewrite.data.section){
            ## data
            NMdata:::NMwriteSectionOne(file0=path.sim,list.sections = nmtext["DATA"],backup=FALSE,quiet=TRUE)    
        } else {
            ## replace data file only
            NMreplaceDataFile(files=path.sim,path.data=basename(path.data),quiet=TRUE)
        }
    },by=.(ROWMODEL)]
    
    

#### Section start: Output tables ####

    dt.models[,{
        
        fn.tab.base <- paste0("FILE=",run.sim,".tab")
        lines.sim <- readLines(path.sim)
        
        lines.tables <- NMreadSection(lines=lines.sim,section="TABLE",as.one=FALSE,simplify=FALSE)
        
        if(is.null(text.table)){
            ## replace output table name
            if(length(lines.tables)==0){
                stop("No TABLE statements found in control stream.")
            } else if(length(lines.tables)<2){
                ## notice, this must capture zero and 1.
                lines.tables.new <- list(gsub(paste0("FILE *= *[^ ]+"),replacement=fn.tab.base,lines.tables[[1]]))
            } else {
                ## I don't remember the reason for the concern this may fail. It looks OK? I think it was supposed to be a check if text.table was a list, so the user is trying to create multiple tables. I don't know if that would work.
                ## message("Number of output tables is >1. Trying, but retrieving results may fail.")
                lines.tables.new <- lapply(seq_along(lines.tables),function(n){
                    fn.tab <- fnAppend(fn.tab.base,n)
                    gsub(paste0("FILE *= *[^ ]+"),replacement=fn.tab,lines.tables[[n]])
                })
            }
        } else {
            lines.tables.new <- list(paste("$TABLE",text.table,fn.tab.base))
        }
        lines.tables.new <- paste(unlist(lines.tables.new),collapse="\n")
        if(exists("add.var.table")){
            
            lines.tables.new <- gsub("\\$TABLE",paste("$TABLE",add.var.table),lines.tables.new)
        }

        ## if no $TABLE found already, just put it last
        if(length(lines.tables)){
            location <- "replace"
        } else {
            location <- "last"
        }
        
        lines.sim <- NMdata:::NMwriteSectionOne(lines=lines.sim,newlines=lines.tables.new,section="TABLE",backup=FALSE,location=location,quiet=TRUE)
        
### save file.sim
        writeTextFile(lines=lines.sim,file=path.sim)
    },by=.(ROWMODEL)]
    
    
###  Section end: Output tables
#### DEBUG Does the sim control stream have TABLES at this point?    

#### Section start: Additional control stream modifications specified by user - modify.model ####
    if( !is.null(modify.model) ){
### This requires NMdata >=0.1.0.905
        dt.models[,{
            NMwriteSection(files=path.sim,list.sections=modify.model,quiet=TRUE,backup=FALSE)
        },by=.(ROWMODEL)]
    }
    
### Section end: Additional control stream modifications specified by user - modify.model

    ## fun simulation method
    dt.models.gen <- dt.models[,
                               method.sim(file.sim=path.sim,file.mod=file.mod,data.sim=data[[DATAROW]],...)
                              ,by=.(ROWMODEL)]
    
    
    ## when methods return just a vector of path.sim, we need to reorganize
    if(ncol(dt.models.gen)==2 && all(colnames(dt.models.gen)%in%c("ROWMODEL","V1"))){
        setnames(dt.models.gen,"V1","path.sim")
    }

    all.files <- melt(dt.models.gen,id.vars="ROWMODEL")
    all.files[,f.exists:=file.exists(value),by=.(ROWMODEL)]
    if(any(!all.files$f.exists)){
        stop(paste("Not all required files exist. The following are missing:",paste(all.files[f.exists==FALSE,value],collapse=", ")))
    }

    ## we need the new path.sim and files.needed
    cnames.gen <- colnames(dt.models.gen)
    if(!"path.sim"%in%cnames.gen) stop("path.sim must be in returned data.table")
    
    
    ## if multiple models have been spawned, and files.needed has been generated, the only allowed method.execute is "nmsim"
    if(nrow(dt.models.gen)>1 && "files.needed"%in%colnames(dt.models.gen)){
        if(NMsimConf$method.execute!="nmsim"){
            stop("Multiple simulation runs spawned, and they need additional files than the simulation input control streams. The only way this is supported is using method.execute=\"nmsim\".")
        }
    }
    
    ## if files.needed, psn execute cannot be used.
    if("files.needed"%in%colnames(dt.models.gen)){
        if(NMsimConf$method.execute=="psn"){
            stop("method.execute=\"psn\" cannot be used with simulation methods that need additional files to run. Try method.execute=\"nmsim\".")
        }
    }
    ## if multiple models spawned, direct is not allowed
    if(nrow(dt.models.gen)>1){
        if(NMsimConf$method.execute=="direct"){
            stop("method.execute=\"direct\" cannot be used with simulation methods that spawn multiple simulation runs. Try method.execute=\"nmsim\" or method.execute=\"psn\".")
        }
    }
    
    
    setnames(dt.models,"path.sim","path.sim.main")
    cols.fneed <- cnames.gen[grepl("^files.needed",cnames.gen)]
    
    ## check that all path.sim and files.needed have been generated
    dt.files <- melt(dt.models.gen,measure.vars=c("path.sim",cols.fneed),value.name="file")
    dt.files[,missing:=!file.exists(file)]
    if(dt.files[,sum(missing)]){
        message(dt.files[,.("No. of files missing"=sum(missing)),by=.(column=variable)])
        stop("All needed files must be available after running simulation method.")
    }
    
    if(length(cols.fneed)){
        dt.models.gen[,ROW:=.I]
        ## by ROW, paste contents of columns named as described in cols.fneed
        ## dt.models.gen[,files.needed:=do.call(paste,as.list(c(get(cols.fneed),sep=":"))),by=.(ROW)]
        pastetmp <- function(...)paste(...,sep=":")
        dt.models.gen[,files.needed:=do.call(pastetmp,.SD),by=.(ROW),.SDcols=cols.fneed]
        
    }
    dt.models <- mergeCheck(
        dt.models.gen[,intersect(c("ROWMODEL","path.sim","files.needed"),cnames.gen),with=FALSE],
        dt.models,
        by="ROWMODEL"
       ,quiet=TRUE)
    
    ## path.sim.lst is full path to final output control stream to be
    ## read by NMscanData. This must be derived after method.sim may
    ## have spawned more runs.
    dt.models[,path.sim.lst:=fnExtension(path.sim,".lst")]
    
    dt.models[,ROWMODEL2:=.I]
    ## dt.models[,seed:={if(is.function(seed))  seed() else seed},by=.(ROWMODEL2)]
    ## if(is.numeric(dt.models[,seed])) dt.model[,seed:=sprintf("(%s)",seed)]


### if typical
    if(typical){
        dt.mods.sim <- dt.models[,.(mod=typicalize(file.sim=path.sim,file.mod=file.mod,return.text=TRUE,file.ext=file.ext)),by=.(ROWMODEL2,path.sim)]
        ## write results
        
        ## dt.models[,writeTextFile(dt.mods.sim[ROWMODEL2==ROWMODEL,mod],file=path.sim),by=ROWMODEL2]
        dt.mods.sim[,writeTextFile(lines=mod,file=unique(path.sim)),by=ROWMODEL2]
    }

    
    if(do.seed){
        dt.models <- do.call(NMseed,c(list(models=dt.models),seed.nm))
    }
    
    
### seed and subproblems
    paste.end <- function(x,add,...){
        c(x[0:(length(x)-1)],
          paste(x[length(x)],add,...)
          )
    }
    
    if(do.seed || subproblems>0){
        dt.models[,{
            
            lines.sim <- readLines(path.sim)
            all.sections.sim <- NMreadSection(lines=lines.sim)
            names.sections <- names(all.sections.sim)
            n.sim.sections <- sum(grepl("^(SIM|SIMULATION)$",names.sections))
            if(n.sim.sections == 0 && (!is.null(arg.seed.nm) || subproblems>1) ){
                warning("No simulation section found. Subproblems and seed will not be applied.")
            }
            if(n.sim.sections > 1 ){
                warning("More than one simulation section found. Subproblems and seed will not be applied.")
            }
            if(n.sim.sections == 1 ){
                
                name.sim <- names.sections[grepl("^(SIM|SIMULATION)$",names.sections)]
                section.sim <- all.sections.sim[[name.sim]]
                
                section.sim <- gsub("\\([0-9]+\\)","",section.sim)
                
### pasting the seed after SIM(ULATION) and after ONLYSIM(ULATION) if the latter exists
                section.sim <- sub("(SIM(ULATION)*( +ONLYSIM(ULATION)*)*) *",paste("\\1",seed),section.sim)
                ## section.sim <- paste.end(section.sim,seed)
                                        #}
                if(subproblems>0){
                    section.sim <- gsub("SUBPROBLEMS *= *[0-9]*"," ",section.sim)
                    ## section.sim <- paste(section.sim,sprintf("SUBPROBLEMS=%s",subproblems))
                    section.sim <- paste.end(section.sim,sprintf("SUBPROBLEMS=%s",subproblems))
                }
                ## section.sim <- paste(section.sim,text.sim)
                section.sim <- paste.end(section.sim,text.sim)
                lines.sim <- NMdata:::NMwriteSectionOne(lines=lines.sim,section="simulation",newlines=section.sim,quiet=TRUE,backup=FALSE)
                writeTextFile(lines.sim,path.sim)
            }
        },by=.(ROWMODEL2)]
    }




######  store NMscanData arguments
    args.NMscanData.list <- c(args.NMscanData,args.NMscanData.default)
    args.NMscanData.list <- args.NMscanData.list[unique(names(args.NMscanData.list))]
    ## if(!is.null(args.NMscanData.list)){
    if(nrow(dt.models)==1){
        dt.models[,args.NMscanData:=list()]
    } else {
        dt.models[,args.NMscanData:=vector("list", .N)]
    }
    dt.models[,args.NMscanData:=list(list(args.NMscanData.list))]


###### store transform
    
    if(nrow(dt.models)==1){
        dt.models[,funs.transform:=list()]
    } else {
        dt.models[,funs.transform:=vector("list", .N)]
    }
    dt.models[,funs.transform:=list(list(transform))]

    
    
    
#### Section start: Execute ####

    
### files needed can vary, so NMexec must be run for one model at a time
    simres <- NULL

    if(execute){
##### Messaging user
        if(!quiet) {
            if(wait.exec){
                message(paste("* Executing Nonmem job(s)",ifelse(NMsimConf$method.execute=="psn","(using PSN)","")))
            } else {
                message(paste0("* Starting Nonmem job(s)",ifelse(NMsimConf$method.execute=="psn"," (using PSN)","")," in background"))
            }
        }

        dt.models[,unlink(path.rds)]
        ## file.res.data <- fnAppend(fnExtension(dt.models[,path.rds],"fst"),"res")
        if(any(file.exists(dt.models[,path.results]))){
            unlink(dt.models[file.exists(path.results),unlink(path.results)])
        }

        ## run sim
        
        if(is.null(progress)) {
            do.pb <- !quiet && dt.models[,.N]>1
        } else {
            do.pb <- progress
        }

        if(is.null(nmquiet)){
            nmquiet <- !(dt.models[,.N]==1 && !do.pb && !quiet)
        }
        
        if(do.pb){
            ## set up progress bar
            pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                                 max = dt.models[,.N], # Maximum value of the progress bar
                                 style = 3,    # Progress bar style (also available style = 1 and style = 2)
                                 ## width = 50,   # Progress bar width. Defaults to getOption("width")
                                 char = "=")
        }
        
        dt.models[,lst:={
            simres.n <- NULL
            files.needed.n <- try(strsplit(files.needed,":")[[1]],silent=TRUE)
            if(inherits(files.needed.n,"try-error")) files.needed.n <- NULL
            
### this is an important assumption. Removing everyting in format
### run.extension. run_input.extension kept.
            ## files.unwanted <- list.files(
            if(file.exists(path.sim.lst)){
                ## message("Existing output control stream found. Removing.")
                
                dt.outtabs <- try(NMscanTables(path.sim.lst,meta.only=TRUE,as.fun="data.table",quiet=TRUE),silent=TRUE)
                if(!inherits(dt.outtabs,"try-error") && is.data.table(dt.outtabs) && nrow(dt.outtabs)>0){
                    
                    file.remove(
                        dt.outtabs[file.exists(file),file]
                    )
                }
                unlink(path.sim.lst)

            }

            
            NMexec(files=path.sim,sge=sge,nc=nc,wait=wait.exec,
                   args.psn.execute=args.psn.execute,nmquiet=nmquiet,quiet=TRUE,
                   method.execute=NMsimConf$method.execute,
                   path.nonmem=NMsimConf$path.nonmem,
                   dir.psn=NMsimConf$dir.psn,
                   system.type=NMsimConf$system.type,
                   files.needed=files.needed.n,
                   input.archive=input.archive,
                   dir.data="..",
                   clean=clean,
                   backup=FALSE)
            
            ## simres.n <- list(lst=path.sim.lst)
            ## simres.n
            if(do.pb){
                setTxtProgressBar(pb, .I)
            }
            
            path.sim.lst
        },by=.(ROWMODEL2)]
        if(do.pb){
            close(pb)
        }
    }

###  Section end: Execute

    
    dt.models.save <- split(dt.models,by="path.rds")
    addClass(dt.models,"NMsimModTab")
    files.rds <- lapply(1:length(dt.models.save),function(I){

####### notify user where to find rds files
        fn.this.rds <- unique(dt.models.save[[I]][,path.rds])
        addClass(dt.models.save[[I]],"NMsimModTab")
        saveRDS(dt.models.save[[I]],file=fn.this.rds)
        fn.this.rds
    })

#### Section start: Read results if requested ####
    
    if(execute && (wait.exec||wait)){

##### Messaging user
### we are controlling this messaging better from NMreadSim()
        ## if(!quiet) message("* Collecting Nonmem results")
        simres <- NMreadSim(unlist(files.rds),wait=wait,progress=progress,quiet=quiet)
    }
    
### Section end: Read results if requested

##### return results to user
    
    ## if(!wait) return(simres$lst)
    ## if(execute && (wait.exec||wait)){
    if(is.NMsimRes(simres) || (execute && (wait.exec||wait))){
        if(!quiet){
            message("\nSimulation results returned. Re-read them without re-simulating using:\n",paste(sprintf("  simres <- NMreadSim(\"%s\")",dt.models[,unique(path.rds)]),collapse="\n"))
        }
        return(returnSimres(simres))
    } else {
        if(!quiet & execute){
            message(sprintf("\nSimulation results not returned. Read them with:\n  simres <- NMreadSim(c(\"%s\"))\nThe first time the results are read, they will be efficiently stored in the simulation results folder. Until then, they only exist as Nonmem result files.",paste(dt.models[,unique(path.rds)],collapse="\",\n    \"")))
        }
        addClass(dt.models,"NMsimModTab")
        return(invisible(dt.models[,unique(path.rds)]))
    }

}
