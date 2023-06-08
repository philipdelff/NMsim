##' execute nonmem while also archiving input data

##' @param files File paths to the models (control stream) to
##'     edit. See file.pattern too.
##' @param file.pattern Alternatively to files, you can supply a
##'     regular expression which will be passed to list.files as the
##'     pattern argument. If this is used, use dir argument as
##'     well. Also see data.file to only process models that use a
##'     specific data file.
##' @param dir If file.pattern is used, dir is the directory to search
##'     for control streams in.
##' @param sge Use the sge queing system. Default is TRUE. Disable for
##'     quick models not to wait.
##' @param input.archive A function of the model file path to generate
##'     the path in which to archive the input data as RDS. Set to
##'     NULL not to archive the data.
##' @param nc Number of cores to use if sending to the
##'     cluster. Default is 64.
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
##'     "-model_dir_name -nm_output=xml,ext,cov,cor,coi,phi".
##' @param update.only Only run model(s) if control stream or data
##'     updated since last run?
##' @param nmquiet Suppress terminal output from `Nonmem`. This is
##'     likely to only work on linux/unix systems.
##' @param method.execute How to run nonmem. Must be one of 'psn',
##'     'direct', or 'directory'.
##' @param dir.psn The directory in which to find PSN
##'     executables. This is only needed if these are not searchable
##'     in the system path, or if the user should want to be explicit
##'     about where to find them (i.e. want to use a specific
##'     installed version of PSN).
##' @param path.nonmem The path to the nonmem executable. Only used if
##'     method.execute="direct" (which is not default). If this
##'     argument is not supplied, NMexec will try to run nmfe75,
##'     i.e. this has to be available in the path of the underlying
##'     shell.
##' @param files.needed In case method.execute="directory", this
##'     argument specifies files to be copied into the temporary
##'     directory before Nonmem is run. Input control stream and
##'     simulation input data does not need to be specified.
##' @details Use this to read the archived input data when retrieving
##'     the nonmem results
##'     NMdataConf(file.data=function(x)fnExtension(fnAppend(x,"input"),".rds"))
##' @import NMdata
##' @examples
##' \dontrun{
##' file.mod <- "run001.mod"
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


### -nm_version=nm74_gf

NMexec <- function(files,file.pattern,dir,sge=TRUE,input.archive,
                   nc=64,dir.data=NULL,wait=FALSE, args.psn.execute,
                   update.only=FALSE,nmquiet=FALSE,
                   method.execute="psn",dir.psn,path.nonmem,
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
    file.psn <- function(dir.psn,file.psn){
        if(dir.psn=="") return(file.psn)
        file.path(dir.psn,file.psn)
    }
    cmd.execute <- file.psn(dir.psn,"execute")


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

    ## args.psn.execute
    if(missing(args.psn.execute)) args.psn.execute <- NULL
    args.psn.execute <- simpleCharArg("args.psn.execute"
                                     ,args.psn.execute
                                     ,default="-model_dir_name -nm_output=xml,ext,cov,cor,coi,phi"
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
        files.exec <- findUpdated(files.all)
    }

    message(paste(files.exec,collapse=", "))
    
    for(file.mod in files.exec){
        file.mod <- NMdata:::filePathSimple(file.mod)
        message(file.mod)
        if(!file.exists(file.mod)){
            stop(paste("Could not find file:",file.mod))
        }
### cat(file.mod,"\n")

        ## replace extension of fn.input based on path.input - prefer rds
        rundir <- dirname(file.mod)

        if(!isFALSE(input.archive(file.mod))){
            fn.input <- input.archive(file.mod)

            ## copy input data
            dat.inp <- NMscanInput(file.mod,file.mod=file.mod,translate=FALSE,applyFilters = FALSE,file.data="extract",dir.data=dir.data,quiet=TRUE)
            saveRDS(dat.inp,file=file.path(rundir,basename(fn.input)))
        }

        if(method.execute=="psn"){
            ## string.cmd <- paste0("cd ",rundir,"; ",cmd.execute ,args.execute)
            string.cmd <- sprintf("cd %s; %s %s",rundir,cmd.execute ,args.psn.execute)
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
            string.cmd <- callNonmemDirect(file.mod,path.nonmem)
        }
        if(method.execute=="directory"){
            string.cmd <- NMexecDirectory(file.mod,path.nonmem,files.needed=files.needed)
        }
        
        if(nmquiet) string.cmd <- paste(string.cmd, ">/dev/null 2>&1")
        
        if(!wait) string.cmd <- paste(string.cmd,"&")
        
        system(string.cmd,ignore.stdout=nmquiet)
    }

    return(invisible(NULL))
}

