##' execute nonmem while also archiving input data

##' @param sge Use the sge queing system. Default is TRUE. Disable for
##'     quick models not to wait.
##' @param file.data.archive A function of the model file path to
##'     generate the path in which to archive the input data as
##'     RDS. Set to NULL not to archive the data.
##' @details Use this to read the archived input data when retrieving
##'     the nonmem results
##'     NMdataConf(file.data=function(x)fnExtension(fnAppend(x,"input"),".rds"))
##' @import NMdata
##' @export

## todo: add support for dir/pattern args
## lapply(list.files("models",pattern="\\.mod$",full.names=T),execsafe)

### -nm_version=nm74_gf

NMexec <- function(files,file.pattern,dir,sge=TRUE,file.data.archive,nc=64,dir.data=NULL,wait=FALSE,args.execute){
    
    
    if(missing(file.data.archive)){
        file.data.archive <- function(file){
            fn.input <- fnAppend(file,"input")
            fn.input <- fnExtension(fn.input,".rds")
            fn.input
        }
    }
    if(missing(args.execute) || is.null(args.execute)){
        args.execute <- "-model_dir_name -nm_output=xml,ext,cov,cor,coi,phi"
    }

    if(missing(files)) files <- NULL
    if(missing(dir)) dir <- NULL
    if(missing(file.pattern)) file.pattern <- NULL
    if(is.null(files) && is.null(file.pattern)) file.pattern <- ".+\\.mod"
    files.all <- NMdata:::getFilePaths(files=files,file.pattern=file.pattern,dir=dir,quiet=quiet)

    files.updated <- findUpdated(files.all)
    
    print(files.updated)
    
    for(file.mod in files.updated){    
        ## replace extension of fn.input based on path.input - prefer rds
        rundir <- dirname(file.mod)

        if(!is.null(file.data.archive)){
            fn.input <- file.data.archive(file.mod)

            ## copy input data
            dat.inp <- NMscanInput(file.mod,translate=FALSE,applyFilters = FALSE,file.data="extract",dir.data=dir.data)
            saveRDS(dat.inp,file=file.path(rundir,basename(fn.input)))
        }

        string.cmd <- paste0("cd ",rundir,"; execute ",args.execute)
        if(sge){
            file.pnm <- file.path(rundir,"NMexec.pnm")
            pnm <- NMgenPNM(nc=nc,file=file.pnm)
            string.cmd <- paste0(string.cmd," -run_on_sge -sge_prepend_flags=\"-pe orte ",nc," -V\" -parafile=",basename(pnm)," -nodes=",nc)
        }

        ## } else {
        ##     string.cmd <- paste0("cd ",rundir,"; execute ",basename(file.mod))
        ## }

        string.cmd <- paste(string.cmd,basename(file.mod))
        if(!wait) string.cmd <- paste(string.cmd,"&")
        system(string.cmd)
    }
    
    return(invisible(NULL))
}


## execute nonmem
## system(
##     paste0("cd ",rundir,"; execute -model_dir_name -run_on_sge -sge_prepend_flags=\"-pe orte ",nc," -V\" -parafile=",basename(pnm)," -nodes=",nc," ",basename(file.mod)," &")
## )
