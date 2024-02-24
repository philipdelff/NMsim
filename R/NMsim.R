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
##'     simulation on. The outpult control stream is for now assumed
##'     to be stored next to the input control stream and ending in
##'     .lst instead of .mod. The .ext file must also be present. If
##'     simulating known subjects, the .phi is necessary too.
##' @param data The simulation data as a data.frame.
##' @param dir.sims The directory in which NMsim will store all
##'     generated files. Default is to create a folder called `NMsim`
##'     next to `file.mod`.
##' @param name.sim Give all filenames related to the simulation a
##'     suffix. A short string describing the sim is recommended like
##'     "ph3_regimens".
##' @param order.columns reorder columns by calling
##'     NMdata::NMorderColumns before saving dataset and running
##'     simulations? Default is TRUE.
##' @param script The path to the script where this is run. For
##'     stamping of dataset so results can be traced back to code.
##' @param subproblems Number of subproblems to use as SUBPROBLEMS in
##'     $SIMULATION block in Nonmem. The default is subproblem=0 which
##'     means not to use SUBPROBLEMS.
##' @param reuse.results If simulation results found on file, should
##'     they be used? If TRUE and reading the results fail, the
##'     simulations will still be rerun.
##' @param transform A list defining transformations to be applied
##'     after the Nonmem simulations and before plotting. For each
##'     list element, its name refers to the name of the column to
##'     transform, the contents must be the function to apply.
##' @param seed Seed to pass to Nonmem. Default is to draw one betwen
##'     0 and 2147483647 (the values supported by Nonmem) for each
##'     simulation. You can pass a function that will be evaluated
##'     (say to choose a different pool of seeds to draw from). In
##'     case type.sim=known, seed is not used and will be set to 1.
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
##' @param execute Execute the simulation or only prepare it?
##'     `execute=FALSE` can be useful if you want to do additional
##'     tweaks or simulate using other parameter estimates.
##' @param nmquiet Silent messages from Nonmem.
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
##'     type.sim="NMsim_known". "psn" has the simple advantage that
##'     the path to nonmem does not have to be specified if "execute"
##'     is in the system search path. So as long as you know where
##'     your Nonmem executable is, "nmsim" is recommended. The default
##'     is "nmsim" if path.nonmem is specified, and "psn" if not.
##' @param nc Number of cores used in parallelization. This is so far
##'     only supported with \code{method.execute="psn"}.
##' @param method.update.inits The initial estimates must be updated
##'     from the estimated model before running the simulation. NMsim
##'     supports two ways of doing this: "psn" which uses PSN's
##'     "update_inits", and "nmsim" which uses a simple internal
##'     method. The advantage of "psn" is it keeps comments in the
##'     control stream and is a method known to many. The advantages
##'     of "nmsim" are it does not require PSN, and that it is very
##'     robust. "nmsim" fixes the whole OMEGA and SIGMA matrices as
##'     single blocks making the $OMEGA and $SIGMA sections of the
##'     control streams less easy to read. On the other hand, this
##'     method is robust because it avoids any interpretation of BLOCK
##'     structure or other code in the control streams.
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
##' @param list.sections Named list of additional control stream
##'     section edits. Note, these can be functions that define how to
##'     edit sections. This is an advanced feature which is not needed
##'     to run most simulations. It is however powerful for some types
##'     of analyses, like modifying parameter values. See vignettes
##'     for further information. Documentation still under
##'     development.
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
##'     the simulated models. This is useful for subsequently
##'     retrieving all the results using `NMreadSim()`. The default is
##'     to create a file called `NMsim_paths.rds` under the model
##'     simulation directory. However, if multiple models are
##'     simulated, this will result in multiple rds files. Specifying
##'     a path ensures that one rds file containing information about
##'     all simulated models will be created.
##' @param wait Wait for simulations to finish? Default is to do so if
##'     simulations are run locally but not to if they are sent to the
##'     cluster. Waiting for them means that the results will be read
##'     when simulations are done. If not waiting, path(s) to `rds`
##'     files to read will be returned. Pass them through
##'     `NMreadSim()` (which also supports waiting for the simulations
##'     to finish).
##' @param quiet If TRUE, messages from what is going on will be
##'     suppressed to the extend implemented.
##' @param check.mod Check the provided control streams for contents
##'     that may cause issues for simulation. Default is `TRUE`, and
##'     it is only recommended to disable this if you are fully aware
##'     of such a feature of your control stream, you know how it
##'     impacts simulation, and you want to get rid of warnings.
##' @param ... Additional arguments passed to \code{method.sim}.
##' @return A data.frame with simulation results (same number of rows
##'     as input data). If `sge=TRUE` a character vector with paths to
##'     simulation control streams.
##' @details Loosely speaking, the argument \code{method.sim} defines
##'     _what_ NMsim will do, \code{method.executes} define _how_ it
##'     does it. \code{method.sim} takes a function that converts an
##'     estimation control stream into whatever should be
##'     run. Features like replacing `$INPUT`, `$DATA`, `$TABLE`, and
##'     handling seeds are NMsim features that are done in addition to
##'     the \code{method.sim}. Also the \code{list.sections} argument
##'     is handled in addition to the \code{method.sim}. The
##'     \code{subproblems} and \code{seed} arguments are available to
##'     all methods creating a \code{$SIMULATION} section.
##'
##' Notice, the following functions are internally available to
##' `NMsim` so you can run them by say \code{method.sim=NMsim_known}
##' without quotes. To see the code of that method, type
##' \code{NMsim_known}.
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
##' \item \code{NMsim_typical} Like \code{NMsim_default} but with all
##' ETAs=0, giving a "typical subject" simulation. Do not confuse this
##' with a "reference subject" simulation which has to do with
##' covariate values. Technically all ETAs=0 is obtained by replacing
##' \code{$OMEGA} by a zero matrix.
##' 
##' \item \code{NMsim_known} Simulates _known_ subjects, meaning that
##' it reuses ETA values from estimation run. This is what is refered
##' to as emperical Bayes estimates. The .phi file from the estimation
##' run must be found next to the .lst file from the estimation.This
##' means that ID values in the (simulation) input data must be ID
##' values that were used in the estimation too. Runs an
##' \code{$ESTIMATION MAXEVAL=0} but pulls in ETAs for the ID's found
##' in data. No \code{$SIMULATION} step is run which may affect how
##' for instance residual variability is simulated, if at all.
##'
##' \item \code{NMsim_VarCov} Like \code{NMsim_default} but `$THETA`,
##' `$OMEGA`, and `SIGMA` are drawn from distribution estimated in
##' covariance step. This means that a successful covariance step must
##' be available from the estimation. In case the simulation leads to
##' negative diagonal elements in $OMEGA and $SIGMA, those values are
##' truncated at zero. For simulation with parameter variability based
##' on bootstrap results, use \code{NMsim_default}.
##'
##' }
##' @import NMdata
##' @import data.table
##' @importFrom stats runif
##' @importFrom xfun relative_path
##' @export



NMsim <- function(file.mod,data,dir.sims, name.sim,
                  order.columns=TRUE,script=NULL,subproblems=NULL,
                  reuse.results=FALSE,seed,args.psn.execute,
                  table.vars,
                  table.options,
                  text.sim="",
                  method.sim=NMsim_default,
                  execute=TRUE,sge=FALSE,
                  nc=1,transform=NULL,
                  method.execute,method.update.inits,
                  create.dirs=TRUE,dir.psn,
                  list.sections,sim.dir.from.scratch=TRUE,
                  col.row,
                  args.NMscanData,
                  path.nonmem=NULL,
                  nmquiet=FALSE,
                  as.fun
                 ,suffix.sim,text.table,
                  system.type=NULL
                 ,dir.res
                 ,file.res
                 ,wait
                 ,quiet=FALSE
                  ,check.mod = TRUE
                 ,...
                  ){
#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####
    
    . <- NULL
    est <- NULL
    DATAROW <- NULL
    data.name <- NULL
    default <- NULL
    direct <- NULL
    directory <- NULL
    dir.sim <- NULL
    f.exists <- NULL
    files.needed <- NULL
    files.res <- NULL
    fn.sim.tmp <- NULL
    fn <- NULL
    fn.mod <- NULL
    path.rds <- NULL
    fn.sim <- NULL
    i <- NULL
    par.type <- NULL
    rowtmp <- NULL
    ID <- NULL
    n <- NULL
    none <- NULL
    is.data <- NULL
    text <- NULL
    textmod <- NULL
    known <- NULL
    model <- NULL
    name.mod <- NULL
    psn <- NULL
    nmsim <- NULL
    pathSimsFromRes <- NULL
    run.mod <- NULL
    run.sim <- NULL
    typical <- NULL
    path.sim <- NULL
    path.digests <- NULL
    path.sim.lst <- NULL
    fn.data <- NULL
    path.data <- NULL
    sim <- NULL
    value <- NULL
    variable <- NULL
    ROW <- NULL
    ROWMODEL <- NULL
    ROWMODEL2 <- NULL
    ..dir.res <- NULL
    pathResFromists <- NULL
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
    
    ## dir.psn - should use NMdataConf setup
    if(missing(dir.psn)) dir.psn <- NULL
    dir.psn <- try(NMdata:::NMdataDecideOption("dir.psn",dir.psn),silent=TRUE)
    if(inherits(dir.psn,"try-error")){
        dir.psn <- NULL
        dir.psn <- simpleCharArg("dir.psn",dir.psn,"",accepted=NULL,lower=FALSE)
    }
    file.psn <- function(dir.psn,file.psn){
        if(dir.psn=="") return(file.psn)
        file.path(dir.psn,file.psn)
    }
    

    ## path.nonmem
    if(missing(path.nonmem)) path.nonmem <- NULL
    path.nonmem <- try(NMdata:::NMdataDecideOption("path.nonmem",path.nonmem),silent=TRUE)
    if(inherits(path.nonmem,"try-error")){
        path.nonmem <- NULL
        
        path.nonmem <- simpleCharArg("path.nonmem",path.nonmem,default=NULL,accepted=NULL,lower=FALSE)
    }

    if(missing(args.NMscanData)) args.NMscanData <- NULL
    if(!is.null(args.NMscanData)){
        if(!is.list(args.NMscanData)) stop("args.NMscanData must be a list.")
        if(any(names(args.NMscanData)=="")) stop("All elements in args.NMscanData must be named.")
    }
    args.NMscanData.default <- list(merge.by.row=FALSE)

    if(missing(system.type)) system.type <- NULL
    system.type <- getSystemType(system.type)

    
    ## after definition of wait and wait.exec, wait is used by
    ## NMreadSim(), wait.exec used by NMexec().
    if(missing(wait)) wait <- !sge
    wait.exec <- !sge && wait
    ## If we already wait on the simulation, no reason to wait for
    ## data. Especially, if NMTRAN fails in exec, NMreadSim() will
    ## wait indefinitely.
    if(wait.exec) wait <- FALSE


    if(nc>1){warning("nc>1 is likely not to work. Please notice there are more efficient methods to speed up simulations, and nc>1 is most likely not what you want anyway. See discussions on the NMsim website.")}
    
    ## method.execute
    if(missing(method.execute)) method.execute <- NULL
    ## if path.nonmem is provided, default method.execute is directory. If not, it is psn
    if(is.null(path.nonmem)) {
        method.execute.def <- "psn"
    } else {
        method.execute.def <- "nmsim"
    }
    method.execute <- simpleCharArg("method.execute",method.execute,method.execute.def,cc(psn,direct,nmsim))
    if(method.execute%in%cc(direct,nmsim) && is.null(path.nonmem)){
        stop("When method.execute is direct or nmsim, path.nonmem must be provided.")
    }

    if(system.type=="windows"){
        message('Windows support is new in NMsim and may be limited. You may need to avoid spaces and some special characters in directory and file names.')
    }
    
    ## args.psn.execute
    if(missing(args.psn.execute)) args.psn.execute <- NULL
    args.psn.execute <- simpleCharArg("args.psn.execute"
                                     ,args.psn.execute
                                     ,default="-clean=5 -model_dir_name -nm_output=xml,ext,cov,cor,coi,phi"
                                     ,accepted=NULL
                                     ,clean=FALSE
                                     ,lower=FALSE)
    
    ## method.update.inits
    if(missing(method.update.inits)) method.update.inits <- NULL
    ## if method.execute is psn, default is psn. If not, it is NMsim.
    if(is.null(method.update.inits)) {
        method.update.inits <- "psn"
        cmd.update.inits <- file.psn(dir.psn,"update_inits")

        if(system.type=="windows"){
            ## We have seen problems with PSN on windows. Until
            ## clarified, internal method prefered on win.
            method.update.inits <- "nmsim"
        }
        
        ## check if update_inits is avail
        ## if(suppressWarnings(system(paste(cmd.update.inits,"-h"),show.output.on.console=FALSE)!=0)){
        if(system.type=="linux"){
            which.found <- system(paste("which",cmd.update.inits),ignore.stdout=T)
            if(which.found!=0){
                method.update.inits <- "nmsim"
                rm(cmd.update.inits)
            }
        }
    }
    method.update.inits <- simpleCharArg("method.update.inits",method.update.inits,"nmsim",cc(psn,nmsim,none))
    ## if update.inits with psn, it needs to be available
    if(method.update.inits=="psn"){
        cmd.update.inits <- file.psn(dir.psn,"update_inits")        
        if(system.type=="linux" && suppressWarnings(system(paste(cmd.update.inits,"-h"),ignore.stdout = TRUE)!=0)){
            stop('Attempting to use PSN\'s update_inits but it was not found. Look at the dir.psn argument or use method.update.inits="nmsim"')
        }
    }
    
    if(missing(seed)) seed <- NULL
    
    ## name.sim
    if(!missing(suffix.sim)){
        if(!missing(name.sim)){
            stop("name.sim and suffix.sim supplied. Use name.sim and not the deprecated suffix.sim. ")
        }
        message("suffix.sim is deprecated. Use name.sim.")
        name.sim <- suffix.sim
    }
    if(missing(name.sim)) name.sim <- NULL
    name.sim <- simpleCharArg("name.sim",name.sim,"noname",accepted=NULL,lower=FALSE)

    ## modelname
    ## if(missing(modelname)){
    modelname <- NULL
    ## }
    file.mod.named <- FALSE
    if(!is.null(names(file.mod))){
        file.mod.named <- TRUE
    }
    
    if(missing(col.row)) col.row <- NULL
    col.row <- NMdata:::NMdataDecideOption("col.row",col.row)

    ## as.fun


    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdata:::NMdataDecideOption("as.fun",as.fun)
    
    input.archive <- inputArchiveDefault
    
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
            stop("Problems creating dir.res. Please check that the parent directory exists and is writable.")
        }
        
    }

    relpathResFromSims <- relative_path(dir.res,dir.sims)
    relpathSimsFromRes <- relative_path(dir.sims,dir.res)
    
    if(missing(text.table)) text.table <- NULL
    
    
    ## seed
    arg.seed <- seed
    if(is.null(seed)){
        seed <- function()round(runif(n=1)*2147483647)
    } 
    
    if(missing(subproblems)|| is.null(subproblems)) subproblems <- 0

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



    

### prepare data sets
    if(!is.null(data) && is.data.frame(data)) {
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
        
        dt.data <- data.table(DATAROW=1:length(data),data.name=names.data)
        if(dt.data[,.N]==1) dt.data[,data.name:=""]
        dt.models <- egdt(dt.models,dt.data,quiet=TRUE)
        
        dt.models[,ROWMODEL:=.I]
    }
    if(is.null(data)){
        dt.models[,data.name:=""]
    } else {
        if(order.columns) data <- lapply(data,NMorderColumns)
    }
    
    
### name.mod and name.sim are confusing. name.mod is the name
### specified for the mod, like
### file.mod=c(ref="run01.mod"). name.sim is the name that was
### given for the who sim.
    
    ## fn.sim is the file name of the simulation control stream created by NMsim
    ## fn.sim <- sub("^run","NMsim",basename(file.mod))
    dt.models[,fn.mod:=basename(file.mod)]
    dt.models[,fn.sim:=fnExtension(paste0("NMsim_",name.mod),".mod")]
    ## dt.models[,fn.sim:=paste0(fn.mod)]

    
    dt.models[,fn.sim.predata:=fnAppend(fn.sim,name.sim)]
    dt.models[,fn.sim:=fnAppend(fn.sim.predata,as.character(data.name)),by=.(ROWMODEL)]
    ## spaces not allowed in model names
    dt.models[,fn.sim:=gsub(" ","_",fn.sim)]
    dt.models[,run.sim:=modelname(fn.sim)]

    
### todo name by data set
    ## dir.sim is the model-individual directory in which the model will be run
    dt.models[,
              dir.sim:=file.path(dir.sims,paste(name.mod,name.sim,sep="_"))]
    
    ## path.sim.tmp is a temporary path to the sim control stream - it
    ## will be moved to path.sim once created.
    dt.models[,fn.sim.tmp:=fnAppend(fn.sim,"tmp")]
    ## path.sim: full path to simulation control stream
    dt.models[,path.sim:=NMdata:::filePathSimple(file.path(dir.sim,fn.sim))]
    

    ## where tosave input data to be read by simulation control stream
    ## fn.data is the data file name, no path
    
    dt.models[,fn.data:=paste0("NMsimData_",fnAppend(fnExtension(name.mod,".csv"),name.sim))]
    dt.models[,fn.data:=fnAppend(fn.data,data.name),by=.(ROWMODEL)]
    dt.models[,fn.data:=gsub(" ","_",fn.data)]
    
    dt.models[,path.data:=file.path(dir.sim,fn.data)]

    ## path.rds - Where to save table of runs
    if(missing(dir.res)) {
        dt.models[,dir.res:=dir.sim]
    } else {
        dt.models[,dir.res:=..dir.res]
    }
    
    if(missing(file.res)) file.res <- NULL
    
    
    if(is.null(file.res)){
        
        ## dt.models[,path.rds:=file.path(dir.res,"NMsim_paths.rds")]
        dt.models[,path.rds:=file.path(dir.res,fnAppend(fnExtension(fn.sim.predata,"rds"),"paths"))]
    } else {
        dt.models[,path.rds:=fnExtension(file.res,"rds")]
    }
    dt.models[,path.rds.exists:=file.exists(path.rds)]
### reading results from prior run
    if(reuse.results && all(dt.models[,path.rds.exists==TRUE])){
        if(!quiet) message("Reading from simulation results on file.")
        simres <- try(NMreadSim(dt.models[,path.rds],wait=wait))
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
                    dir.create(dir.sim)}
    },by=.(ROWMODEL)
    ]
    
    
    
### Generate the first version of file.sim.
    ## It would not need to, but beware PSN's update_inits needs to
    ## create a new file - don't try to overwrite an existing one.
    if(method.update.inits=="none"){
        dt.models[,file.copy(file.mod,path.sim),by=ROWMODEL]
    }

    if(method.update.inits=="psn"){
### this next line is done already. I think it should be removed but testing needed.
        cmd.update.inits <- file.psn(dir.psn,"update_inits")

        dt.models[,
        {
            cmd.update <- sprintf("%s --output_model=\"%s\" \"%s\"",normalizePath(cmd.update.inits,mustWork=FALSE),fn.sim.tmp,normalizePath(file.mod))
### would be better to write to another location than next to estimation model
            ## cmd.update <- sprintf("%s --output_model=%s %s",cmd.update.inits,file.path(".",fn.sim.tmp),file.mod)

            if(system.type=="linux"){
                sys.res <- system(cmd.update,wait=TRUE)
                
                if(sys.res!=0){
                    stop("update_inits failed. Please look into this. Is the output control stream available? Is it in a directory where you have write-access?")
                }
            }
            if(system.type=="windows"){
                cmd.update <- sprintf("\"%s\" --output_model=\"%s\" \"%s\"",normalizePath(cmd.update.inits),fn.sim.tmp,normalizePath(file.mod))
                script.update.inits <- file.path(dirname(file.mod),"script_update_inits.bat")
                writeTextFile(cmd.update,script.update.inits)
                sys.res <- shell(shQuote("tmp.bat",type="cmd") )
            }
            
            file.rename(file.path(dirname(file.mod),fn.sim.tmp),path.sim)
        },by=ROWMODEL]
    }
    if(method.update.inits=="nmsim"){
        
        dt.models[,NMupdateInits(file.mod=file.mod,newfile=path.sim,fix=TRUE),by=.(ROWMODEL)]
    }
    
    
    dt.models[,{
### note: insert test for whether run is needed here
        ## if data is NULL, we will re-use data used in file.mod
        rewrite.data.section <- TRUE
        if(is.null(data)){
            if(!packageVersion("NMdata")>"0.1.1") stop("data has to be supplied. Starting with NMdata 0.1.2 it will be possible not to supply data which is intented for simulations for VPCs.")
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
        } else {
            data.this <- data[[DATAROW]]

        }
        
        ## if(order.columns) data <- lapply(data,NMorderColumns)
        ##data <- NMorderColumns(data)
### save data and replace $input and $data
#### multiple todo: save only for each unique path.data
        
        
        
        nmtext <- NMwriteData(data.this,file=path.data,quiet=TRUE,args.NMgenText=list(dir.data="."),script=script)
        
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
            NMreplaceDataFile(files=path.sim,path.data=basename(path.data))
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
        ## fun.paste <- function(...) paste(...,sep="\n")
        ## l.bu <- lines.tables.new
        ## lines.tables.new <- do.call(fun.paste,lines.tables.new)
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


#### Section start: Additional control stream modifications specified by user - list.sections ####
    if( !missing(list.sections) && !is.null(list.sections) ){
### This requires NMdata >=0.1.0.905
        if(packageVersion("NMdata")<"0.1.1") warning("list.sections argument requires NMdata>=0.1.1. Please upgrade NMdata.") 
        dt.models[,{
            NMwriteSection(files=path.sim,list.sections=list.sections)
        },by=.(ROWMODEL)]
    }
    
### Section end: Additional control stream modifications specified by user - list.sections

    
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
        if(method.execute!="nmsim"){
            stop("Multiple simulation runs spawned, and they need additional files than the simulation input control streams. The only way this is supported is using method.execute=\"nmsim\".")
        }
    }
    
    ## if files.needed, psn execute cannot be used.
    if("files.needed"%in%colnames(dt.models.gen)){
        if(method.execute=="psn"){
            stop("method.execute=\"psn\" cannot be used with simulation methods that need additional files to run. Try method.execute=\"nmsim\".")
        }
    }
    ## if multiple models spawned, direct is not allowed
    if(nrow(dt.models.gen)>1){
        if(method.execute=="direct"){
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
    dt.models[,seed:={if(is.function(seed))  seed() else seed},by=.(ROWMODEL2)]
    
    
    
### seed and subproblems
    if(!is.null(seed) || subproblems>0){
        dt.models[,{
            
            lines.sim <- readLines(path.sim)
            all.sections.sim <- NMreadSection(lines=lines.sim)
            names.sections <- names(all.sections.sim)
            n.sim.sections <- sum(grepl("^(SIM|SIMULATION)$",names.sections))
            if(n.sim.sections == 0 && (!is.null(arg.seed) || subproblems>1) ){
                warning("No simulation section found. Subproblems and seed will not be applied.")
            }
            if(n.sim.sections > 1 ){
                warning("More than one simulation section found. Subproblems and seed will not be applied.")
            }
            if(n.sim.sections == 1 ){
                name.sim <- names.sections[grepl("^(SIM|SIMULATION)$",names.sections)]
                section.sim <- all.sections.sim[[name.sim]]
                
                if(!is.null(seed)){
                    section.sim <- gsub("\\([0-9]+\\)","",section.sim)
                    section.sim <- paste(section.sim,sprintf("(%s)",seed))
                }
                if(subproblems>0){
                    section.sim <- gsub("SUBPROBLEMS *= *[0-9]*"," ",section.sim)
                    section.sim <- paste(section.sim,sprintf("SUBPROBLEMS=%s",subproblems))
                }
                section.sim <- paste(section.sim,text.sim)
                lines.sim <- NMdata:::NMwriteSectionOne(lines=lines.sim,section="simulation",newlines=section.sim,quiet=TRUE)
                writeTextFile(lines.sim,path.sim)
            }
        },by=.(ROWMODEL2)]
    }




    
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

        dt.models[,unlink(path.rds)]
        file.res.data <- fnAppend(fnExtension(dt.models[,path.rds],"fst"),"res")
        if(any(file.exists(file.res.data))){
            unlink(file.res.data[file.exists(file.res.data)])
        }

        ## run sim
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
            
            NMexec(files=path.sim,sge=sge,nc=nc,wait=wait.exec,args.psn.execute=args.psn.execute,nmquiet=nmquiet,method.execute=method.execute,path.nonmem=path.nonmem,dir.psn=dir.psn,files.needed=files.needed.n,input.archive=input.archive,system.type=system.type)
            
            ## simres.n <- list(lst=path.sim.lst)
            ## simres.n
            path.sim.lst
        },by=.(ROWMODEL2)]
    }

###  Section end: Execute

    
    dt.models.save <- split(dt.models,by="path.rds")
    addClass(dt.models,"NMsimModTab")
    files.rds <- lapply(1:length(dt.models.save),function(I){

####### notify user where to find rds files
        fn.this.rds <- unique(dt.models.save[[I]][,path.rds])
        addClass(dt.models.save[[I]],"NMsimModTab")
        if(!quiet){
            message(sprintf("\nWriting simulation info to %s\n",fn.this.rds))
        }
        saveRDS(dt.models.save[[I]],file=fn.this.rds)
        fn.this.rds
    })

#### Section start: Read results if requested ####
    
    if(execute && (wait.exec||wait)){

### This runs NMreadSim in try. But since the user is
### requesting execute and wait, an error reading this should
### result in an NMsim error.
        ## simres <- try(NMreadSim(unlist(files.rds),wait=wait))
        ## if(inherits(simres,"try-error")){
        ##     message("Could not read simulation results. Returning path to rds file containing a table with info on all simulations (read with `readRDS()`).")
        ##     return(unlist(files.res))
        ## }
        simres <- NMreadSim(unlist(files.rds),wait=wait)
    }
    
### Section end: Read results if requested

##### return results to user
    
    ## if(!wait) return(simres$lst)
    ## if(execute && (wait.exec||wait)){
    if(is.NMsimRes(simres) || (execute && (wait.exec||wait))){
        return(returnSimres(simres))
    } else {
        addClass(dt.models,"NMsimModTab")
        return(invisible(dt.models[,unique(path.rds)]))
    }
}
