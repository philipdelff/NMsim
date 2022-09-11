##' execute nonmem while also archiving input data

##' @param sge Use the sge queing system. Default is TRUE. Disable for
##'     quick models not to wait.
##' @param file.data.archive A function of the model file path to
##'     generate the path in which to archive the input data as
##'     RDS. Set to NULL not to archive the data.
##' @details Use this to read the archived input data when retrieving
##'     the nonmem results
##'     NMdataConf(file.data=function(x)fnExtension(fnAppend(x,"input"),".rds"))

## todo: add support for dir/pattern args
## lapply(list.files("models",pattern="\\.mod$",full.names=T),execsafe)


NMexec <- function(file.mod,sge=TRUE,file.data.archive,nc=64,dir.data=NULL,wait=FALSE){
    
    library(NMdata)

    if(missing(file.data.archive)){
        file.data.archive <- function(file){
            fn.input <- fnAppend(file,"input")
            fn.input <- fnExtension(fn.input,".rds")
            fn.input
        }
    }

    ## replace extension of fn.input based on path.input - prefer rds
    rundir <- dirname(file.mod)

    if(!is.null(file.data.archive)){
        fn.input <- file.data.archive(file.mod)

        ## copy input data
        dat.inp <- NMscanInput(file.mod,translate=FALSE,applyFilters = FALSE,file.data="extract",dir.data=dir.data)
        saveRDS(dat.inp,file=file.path(rundir,basename(fn.input)))
    }

    if(sge){
        file.pnm <- file.path(rundir,"execsafe.pnm")
        pnm <- NMgenPNM(nc=nc,file=file.pnm)
        ## execute nonmem
        system(
            paste0("cd ",rundir,"; execute -model_dir_name -run_on_sge -sge_prepend_flags=\"-pe orte ",nc," -V\" -parafile=",basename(pnm)," -nodes=",nc," ",basename(file.mod)," &")
        )
    } else {
        string.cmd <- paste0("cd ",rundir,"; execute ",basename(file.mod))
        if(!wait) string.cmd <- paste(string.cmd,"&")
        system(string.cmd)
    }
}


