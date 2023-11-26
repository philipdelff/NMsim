##' Execute Nonmem and archive input data with model files
##'
##' Execute Nonmem from within R - optionally but by default in
##' parallel. Archiving the input data ensures that postprocessing can
##' still be reproduced if the input data files should be updated.
##' 
##' @param files File paths to the models (control streams) to run
##'     nonmem on. See file.pattern too.
##' @param file.pattern Alternatively to files, you can supply a
##'     regular expression which will be passed to list.files as the
##'     pattern argument. If this is used, use dir argument as
##'     well. Also see data.file to only process models that use a
##'     specific data file.
##' @param dir If file.pattern is used, dir is the directory to search
##'     for control streams in.
##' @param sge Use the sge queing system. Default is TRUE. Disable for
##'     quick models not to wait for the queue to run the job.
##' @param input.archive A function of the model file path to generate
##'     the path in which to archive the input data as RDS. Set to
##'     NULL not to archive the data.
##' @param nc Number of cores to use if sending to the cluster. This
##'     will only be used if \code{method.execute="psn"}, and
##'     \code{sge=TRUE}. Default is 64.
##' @param dir.data The directory in which the data file is
##'     stored. This is normally not needed as data will be found
##'     using the path in the control stream. This argument may be
##'     removed in the future since it should not be needed.
##' @param wait Wait for process to finish before making R console
##'     available again? This is useful if calling NMexec from a
##'     function that needs to wait for the output of the Nonmem run
##'     to be available for further processing.
##' @param args.psn.execute A character string with arguments passed
##'     to execute. Default is
##'     "-model_dir_name -nm_output=xml,ext,cov,cor,coi,phi,shk".
##' @param update.only Only run model(s) if control stream or data
##'     updated since last run?
##' @param nmquiet Suppress terminal output from `Nonmem`. This is
##'     likely to only work on linux/unix systems.
##' @param method.execute How to run Nonmem. Must be one of 'psn',
##'     'nmsim', or 'direct'. 
##' \itemize{
##'
##' \item psn PSN's execute is used. This supports parallel Nonmem
##' runs. Use the \code{nc} argument to control how many cores to use
##' for each job. For estimation runs, this is most likely the better
##' choice, if you have PSN installed. See \code{dir.psn} argument
##' too.
##'
##' \item nmsim Creates a temporary directory and runs Nonmem
##' inside that directory before copying relevant results files back
##' to the folder where the input control stream was. If
##' \code{sge=TRUE}, the job will be submitted to a cluster, but
##' parallel execution of the job itself is not supported. See
##' \code{path.nonmem} argument too.
##' 
##' \item direct Nonmem is called directly on the control stream. This
##' is the simplest method and is the least convenient in most
##' cases. It does not offer parallel runs and leaves all the Nonmem
##' output files next to the control streams.
##' 
##'
##' }
##'
##' See `sge` as well.
##' @param dir.psn The directory in which to find PSN
##'     executables. This is only needed if these are not searchable
##'     in the system path, or if the user should want to be explicit
##'     about where to find them (i.e. want to use a specific
##'     installed version of PSN).
##' @param path.nonmem The path to the nonmem executable. Only used if
##'     \code{method.execute="direct"} or
##'     \code{method.execute="nmsim"} (which is not default). If this
##'     argument is not supplied, NMexec will try to run nmfe75,
##'     i.e. this has to be available in the path of the underlying
##'     shell. The default value can be modified using
##'     \code{NMdata::NMdataConf}, like
##'     \code{NMdataConf(path.nonmem="/path/to/nonmem")}
##' @param files.needed In case method.execute="nmsim", this argument
##'     specifies files to be copied into the temporary directory
##'     before Nonmem is run. Input control stream and simulation
##'     input data does not need to be specified.
##' @param system.type A charachter string, either \"windows\" or
##'     \"linux\" - case insensitive. Windows is only experimentally
##'     supported. Default is to use \code{Sys.info()[["sysname"]]}.
##' @details Use this to read the archived input data when retrieving
##'     the nonmem results:
##'     \code{NMdataConf(file.data=inputArchiveDefault)}
##'
##' Since `NMexec` will typically not be used for simulations directly
##' (`NMsim` is the natural interface for that purpose), the default
##' method for `NMexec` is currently to use `method.execute="psn"`
##' which is at this point the only of the methods that allow for
##' multi-core execution of a single Nonmem job (NB:
##' `method.execute="NMsim"` can run multiple jobs in parallel which
##' is normally sufficient for simulations).
##' 
##' @return NULL (invisibly)
##' @import NMdata
##' @importFrom utils packageVersion
##' @importFrom R.utils getAbsolutePath
##' @examples
##' file.mod <- "run001.mod"
##' \dontrun{
##' ## run locally - not on cluster
##' NMexec(file.mod,sge=FALSE)
##' ## run on cluster with 16 cores. 64 cores is default
##' NMexec(file.mod,nc=16)
##' ## submit multiple models to cluster
##' multiple.models <- c("run001.mod","run002.mod")
##' NMexec(multiple.models,nc=16)
##' ## run all models called run001.mod - run099.mod if updated. 64 cores to each.
##' NMexec(file.pattern="run0..\\.mod",dir="models",nc=16,update.only=TRUE)
##' }
##' @export


NMexec <- function(files,file.pattern,dir,sge=TRUE,input.archive,
                   nc=64,dir.data=NULL,wait=FALSE, args.psn.execute,
                   update.only=FALSE,nmquiet=FALSE,
                   method.execute="psn",dir.psn,path.nonmem,system.type,
                   files.needed){
    
    
#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    input.archive <- NULL
    nid <- NULL
    input <- NULL
    result <- NULL
    
### Section end: Dummy variables, only not to get NOTE's in pacakge checks
    
    ## dir.psn
    if(missing(dir.psn)) dir.psn <- NULL
    dir.psn <- try(NMdata:::NMdataDecideOption("dir.psn",dir.psn))
    if(inherits(dir.psn,"try-error")){
        dir.psn <- NULL
        dir.psn <- simpleCharArg("dir.psn",dir.psn,"",accepted=NULL,lower=FALSE)
    }
    fun.file.psn <- function(dir.psn,file.psn){
        if(dir.psn=="") return(file.psn)
        file.path(dir.psn,file.psn)
    }
    cmd.execute <- fun.file.psn(dir.psn,"execute")

    method.execute <- tolower(gsub(" ","",method.execute))
    if(!method.execute %in% c("psn","direct","nmsim")){
        stop("method.execute must be one of psn, direct, and nmsim.")
    }
    
    ## path.nonmem
    ## if(missing(path.nonmem)||is.null(path.nonmem)) path.nonmem <- "nmfe75"
    if(missing(path.nonmem)) path.nonmem <- NULL
    path.nonmem <- try(NMdata:::NMdataDecideOption("path.nonmem",path.nonmem))
    if(inherits(path.nonmem,"try-error")){
        path.nonmem <- NULL
        path.nonmem <- simpleCharArg("path.nonmem",path.nonmem,NULL,accepted=NULL,lower=FALSE)
    }


    if(missing(input.archive)||is.null(input.archive)){
        input.archive <- inputArchiveDefault
    }    
    if(isFALSE(input.archive)){
        input.archive <- function(file) FALSE
    }

    if(missing(system.type)) system.type <- NULL
    system.type <- getSystemType(system.type)

    ## args.psn.execute
    if(missing(args.psn.execute)) args.psn.execute <- NULL
    args.psn.execute <- simpleCharArg("args.psn.execute"
                                     ,args.psn.execute
                                     ,default="-model_dir_name -nm_output=xml,ext,cov,cor,coi,phi,shk"
                                     ,accepted=NULL
                                     ,clean=FALSE
                                     ,lower=FALSE)

    if(missing(files)) files <- NULL
    if(missing(dir)) dir <- NULL
    dir <- simpleCharArg("dir"
                        ,dir
                        ,default=NULL
                        ,accepted=NULL
                        ,clean=FALSE
                        ,lower=FALSE)

    if(missing(file.pattern)) file.pattern <- NULL
    file.pattern <- simpleCharArg("file.pattern"
                                 ,file.pattern
                                 ,default=NULL
                                 ,accepted=NULL
                                 ,clean=FALSE
                                 ,lower=FALSE)

    
    
    if(is.null(files) && is.null(file.pattern)) file.pattern <- ".+\\.mod"
    files.all <- NMdata:::getFilePaths(files=files,file.pattern=file.pattern,dir=dir,quiet=TRUE)
    
    files.exec <- files.all
    if(update.only){
        ## files.exec <- findUpdated(fnExtension(files.all,"lst"))
        files.exec <- findUpdated(files.all)
    }

    ## message(paste(files.exec,collapse=", "))
    
    for(file.mod in files.exec){
        file.mod <- NMdata:::filePathSimple(file.mod)
        message(paste0("Executing ",file.mod))
        if(!file.exists(file.mod)){
            stop(paste("Could not find file:",file.mod))
        }
### cat(file.mod,"\n")

        ## replace extension of fn.input based on path.input - prefer rds
        rundir <- dirname(file.mod)

        if(!isFALSE(input.archive(file.mod))){
            fn.input <- input.archive(file.mod)

            ## copy input data
            if(packageVersion("NMdata")<"0.1.1"){
                dat.inp <- NMscanInput(file=file.mod,translate=FALSE,applyFilters = FALSE,file.data="extract",dir.data=dir.data,quiet=TRUE)
            } else {
                dat.inp <- NMscanInput(file=file.mod,translate=FALSE,apply.filters = FALSE,file.data="extract",dir.data=dir.data,quiet=TRUE)
            }
            saveRDS(dat.inp,file=file.path(rundir,basename(fn.input)))
        }


        if(method.execute=="psn"){
            ##if(system.tpe=="linux"){
            
            string.cmd <- sprintf('cd "%s"; "%s" %s',rundir,cmd.execute ,args.psn.execute)
            ##}
            ## if(system.type=="windows"){
            ##     pas
            ## }
            if(sge){
                string.cmd <- paste0(string.cmd," -run_on_sge")
                if(nc>1){
                    file.pnm <- file.path(rundir,"NMexec.pnm")
                    pnm <- NMgenPNM(nc=nc,file=file.pnm)
                    string.cmd <- paste0(string.cmd," -sge_prepend_flags=\"-pe orte ",nc," -V\" -parafile=",basename(pnm)," -nodes=",nc)
                }
            }
            string.cmd <- paste(string.cmd,basename(file.mod))
        }
        if(method.execute=="direct"){
            if(!file.exists(path.nonmem)){
                stop(paste("The supplied path to the Nonmem executable is invalid:",path.nonmem))
            }
            string.cmd <- callNonmemDirect(file.mod,path.nonmem)
        }
        if(method.execute=="nmsim"){
            if(!file.exists(path.nonmem)){
                stop(paste("The supplied path to the Nonmem executable is invalid:",path.nonmem))
            }
            string.cmd <- NMexecDirectory(file.mod,path.nonmem,files.needed=files.needed)
            if(sge) {
                
                ## string.cmd <- sprintf("cd %s; qsub -terse -wd \'%s\' %s",getwd(),dirname(string.cmd),string.cmd)
                ## I am not sure if absolute path is needed here.
                string.cmd <- sprintf('cd "%s"; qsub -terse -wd \'%s\' %s',
                                      getwd(),getAbsolutePath(dirname(string.cmd)),string.cmd)
                ## string.cmd <- paste0("CURWD=",getwd()," ",string.cmd)
                wait <- TRUE
            } else {
                string.cmd <- sprintf("cd %s; ./%s",dirname(string.cmd),basename(string.cmd))
            }
        }
        
        if(system.type=="windows"){
            
            ## contents.bat <- gsub(";","\n",string.cmd)
            ## cat(contents.bat,file=path.script)
            path.script <- file.path(dirname(file.mod),"NMsim_exec.bat")
            contents.bat <-
                strsplit(string.cmd,split=";")[[1]]
            writeTextFile(contents.bat,file=path.script)

            shell(shQuote(path.script,type="cmd") )
        }
        if(system.type=="linux"){
            if(nmquiet) string.cmd <- paste(string.cmd, ">/dev/null 2>&1")
            if(!wait) string.cmd <- paste(string.cmd,"&")
            
            system(string.cmd,ignore.stdout=nmquiet)
        }
    }

    return(invisible(NULL))
}
