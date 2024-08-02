##' Summarize and test NMsim configuration
##' @param path.nonmem See ?NMsim
##' @param dir.psn See ?NMsim
##' @param method.execute See ?NMsim
##' @param must.work Throw an error if the configuration does not seem
##'     to match system.
##' @param system.type See ?NMsim
##' @export

NMsimTestConf <- function(path.nonmem,dir.psn,method.execute,must.work=FALSE,system.type){

    psn <- NULL
    direct <- NULL
    nmsim <- NULL
    none <- NULL

    if(missing(system.type)) system.type <- NULL
    
    res <- list()

    res$version.NMsim <- packageVersion("NMsim")
    res$version.NMdata <- packageVersion("NMdata")

    res$sysname <- NA
    if(is.null(system.type)){
        res$sysname <- Sys.info()[['sysname']]
        res$system.type <- getSystemType(sysname=res$sysname)
    } else {
        res$system.type <- system.type
    }
    
    ## path.nonmem
    if(missing(path.nonmem)) path.nonmem <- NULL
    res$path.nonmem <- try(NMdata:::NMdataDecideOption("path.nonmem",path.nonmem),silent=TRUE)
    if(inherits(res$path.nonmem,"try-error")){
        res$path.nonmem <- NULL
        
        res$path.nonmem <- simpleCharArg("path.nonmem",res$path.nonmem,default=NULL,accepted=NULL,lower=FALSE)
    }
    if(is.null(res$path.nonmem)) res$path.nonmem <- "none"
    res$exists.path.nonmem <- ifelse(res$path.nonmem=="",NA,searchExecutable(res$path.nonmem))
    
    ## dir.psn
    if(missing(dir.psn)) dir.psn <- NULL
    res$dir.psn <- try(NMdata:::NMdataDecideOption("dir.psn",dir.psn),silent=TRUE)
    if(inherits(res$dir.psn,"try-error")){
        res$dir.psn <- NULL
        res$dir.psn <- simpleCharArg("dir.psn",res$dir.psn,"",accepted=NULL,lower=FALSE)
    }
    if(is.null(res$dir.psn)) res$dir.psn <- "none"
    res$exists.dir.psn <- ifelse(res$dir.psn=="none",NA,dir.exists(res$dir.psn))

    if(res$system.type=="linux"){
        res$exists.dir.psn <- any(grepl(pattern="^execute$",list.files(res$dir.psn)))
    }
    if(res$system.type=="windows"){
        res$exists.dir.psn <- any(grepl(pattern="^execute.*",list.files(res$dir.psn)))
    }
    res$exists.psn.execute <- searchExecutable("execute",dir.extra=res$dir.psn)
    
    ## method.execute
    if(missing(method.execute)) method.execute <- NULL
    ## if path.nonmem is provided, default method.execute is directory. If not, it is psn
    if(res$exists.path.nonmem) {
        method.execute.def <- "nmsim"
    } else if(res$exists.psn.execute) {
        method.execute.def <- "psn"
    } else {
        method.execute.def <- "none"
    }
    res$method.execute <- simpleCharArg("method.execute",method.execute,method.execute.def,cc(psn,direct,nmsim,none))
    if(res$method.execute%in%cc(direct,nmsim) && is.null(res$path.nonmem)){
        stop("When method.execute is direct or nmsim, path.nonmem must be provided.")
    }

######## TODO integrate metod.update.inits

    
    ## if(res$system.type=="windows"){
    ##     message('Windows support is new in NMsim and may be limited. You may need to avoid spaces and some special characters in directory and file names.')
    ## }

    if(must.work && res$method.execute=="none"){
        stop("No execution method found. Check path.nonmem and (if wanted) dir.psn.")
    }

### Test execution
    ## if(test.exec){
    ## }

    res

}

