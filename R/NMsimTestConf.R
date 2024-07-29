NMsimTestConf <- function(path.nonmem,dir.psn,method.execute,must.work=FALSE){
    
    res <- list()

    res$version.NMsim <- packageVersion("NMsim")
    res$version.NMdata <- packageVersion("NMdata")
    res$system <- Sys.info()['sysname']
    res$system.type <- getSystemType()
    
    ## path.nonmem
    if(missing(path.nonmem)) path.nonmem <- NULL
    res$path.nonmem <- try(NMdata:::NMdataDecideOption("path.nonmem",path.nonmem),silent=TRUE)
    if(inherits(res$path.nonmem,"try-error")){
        res$path.nonmem <- NULL
        
        res$path.nonmem <- simpleCharArg("path.nonmem",res$path.nonmem,default=NULL,accepted=NULL,lower=FALSE)
    }
    if(is.null(res$path.nonmem)) res$path.nonmem <- "none"
    res$exists.path.nonmem <- ifelse(res$path.nonmem=="",NA,file.exists(res$path.nonmem))


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
    
    ## method.execute
    if(missing(method.execute)) method.execute <- NULL
    ## if path.nonmem is provided, default method.execute is directory. If not, it is psn
    if(res$exists.path.nonmem) {
        method.execute.def <- "nmsim"
    } else if(res$exists.dir.psn) {
        method.execute.def <- "psn"
    } else {
        method.execute.def <- "none"
    }
    res$method.execute <- simpleCharArg("method.execute",method.execute,method.execute.def,cc(psn,direct,nmsim,none))
    if(res$method.execute%in%cc(direct,nmsim) && is.null(res$path.nonmem)){
        stop("When method.execute is direct or nmsim, path.nonmem must be provided.")
    }

    if(res$system.type=="windows"){
        message('Windows support is new in NMsim and may be limited. You may need to avoid spaces and some special characters in directory and file names.')
    }

    if(must.work && res$method.execute=="none"){
        stop("No execution method found. Check path.nonmem and (if wanted) dir.psn.")
    }

### Test execution
    ## if(test.exec){
    ## }

    res

}

searchInPath <- function(cmd,system.type){

    res.num <- switch(system.type,
           windows=suppressWarnings(shell(shQuote(sprintf("where /q %s || EXIT /B",cmd)))),
           ## linux=system(sprintf("which %s",cmd),ignore.stdout=T)
           linux=system(sprintf("command -v %s",cmd))
           )
    ## TRUE means found, FALSE means not found
    res.num==0

}

