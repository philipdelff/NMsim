##' Simulate with parameter values sampled from a covariance step
##'
##' @description Like \code{NMsim_default} but `$THETA`, `$OMEGA`, and `SIGMA` are
##' drawn from distribution estimated in covariance step. A successful
##' covariance step must be available from the estimation. In case the
##' simulation leads to negative diagonal elements in $OMEGA and
##' $SIGMA, those values are truncated at zero. For simulation with
##' parameter variability based on bootstrap results, use
##' \code{NMsim_default}.
##'
##' This function does not run any simulations. To simulate, using
##' this method, see `NMsim()`.
##'
##' @param file.sim The path to the control stream to be edited. This
##'     function overwrites the contents of the file pointed to by
##'     file.sim.
##' @param file.mod Path to the path to the original input control
##'     stream provided as `file.mod` to `NMsim()`.
##' @param data.sim Included for compatibility with `NMsim()`. Not
##'     used.
##' @param nsims Number of replications wanted. The default is 1. If
##'     greater, multiple control streams will be generated.
##' @param ext Parameter values in long format as created by
##'     `readParsWide` and `NMdata::NMreadExt`.
##' @param write.ext If supplied, a path to an rds file where the
##'     parameter values used for simulation will be saved.
##' @import NMdata
##' @import data.table
##' @importFrom MASS mvrnorm
##' @return Character vector of simulation control stream paths
##' @export

NMsim_VarCov <- function(file.sim,file.mod,data.sim,nsims,ext,write.ext=NULL){

#### Section start: Dummy variables, only not to get NOTE's in package checks ####

    . <- NULL
    est <- NULL
    i <- NULL
    j <- NULL
    FIX <- NULL
    fn.sim <- NULL
    NMREP <- NULL
    parameter <- NULL
    par.type <- NULL
    path.sim <- NULL
    ROW <- NULL
    run.sim <- NULL
    submodel <- NULL
    SUBMODEL <- NULL
    value <- NULL

### Section end: Dummy variables, only not to get NOTE's in package checks 
    

    files.needed.def <- NMsim_default(file.sim=file.sim,file.mod,data.sim)
    
    path.lst <- fnExtension(file.mod,"lst")
    path.cov <- fnExtension(path.lst,"cov")
    path.ext <- fnExtension(path.lst,"ext")

    ## define new files
    path.sim.0 <- file.sim
    run.sim.0 <- fnExtension(basename(path.sim.0),"")
    rm(file.sim)


    if(missing(ext)) ext <- NULL
    if(missing(nsims)) nsims <- NULL
    
    if(is.null(ext)){
        if(is.null(nsims)) nsims <- 1
        simulatePars <- TRUE
    } else {
        if(!is.null(nsims)) stop("nsims not supported in combination with ext")
        nsims <- ext[,.N]
        simulatePars <- FALSE
    }


#### Section start: sampling new parameters from COV matrix ####
    if(simulatePars){
        covmat <- NMdata::NMreadCov(path.cov)
        ests <- NMreadExt(path.ext,as.fun="data.table")[NMREP==1,.(parameter,par.type,i,j,est,FIX)]
        ests <- ests[par.type%in%c("THETA","SIGMA","OMEGA")]
        ests <- ests[match(ests$parameter,colnames(covmat))]
        newpars <- mvrnorm(n=nsims,Sigma=covmat,mu=ests$est)
        newpars <- round(newpars,8)
### as.list first is because without it, this will fail for
### nsims=1. This is because a single-column data.table would be
### created in that case, and then SUBMODEL and further steps
### become wrong and will fail.
        if(nsims==1){
            newpars <- as.data.table(as.list(newpars))
        } else {
            newpars <- as.data.table(newpars)
        }
        
        newpars[,SUBMODEL:=.I]

        newpars <- mergeCheck(
            melt(newpars,id.vars="SUBMODEL",variable.name="parameter")
           ,
            ests
           ,by="parameter",quiet=TRUE)

        ## newpars <- mergeCheck(newpars,dt.sims,by="SUBMODEL")
        ## if the parameter was fixed, reset it to the estimate
        newpars[FIX==1,value:=est]
        ## if OMEGA or SIGMA diagonal elements are <0 set to 0.
        newpars[i==j&value<0,value:=0]

###  Section end: sampling new parameters from COV matrix
    }
#### Section start: Parameter from provided table ####

    if(!simulatePars){

        newpars <- ext
        ## newpars[,SUBMODEL:=.GRP,by=.(model)]
        setnames(newpars,"model","SUBMODEL")
    }

### Section end: Parameter from provided table

    
    
    ## dt.sims <- data.table(SUBMODEL=1:nsims)
    ## length.num.char <- ceiling(log10(nsims+1))
    ## dt.sims[,submodel:=sprintf(fmt=paste0("%0",length.num.char,"d"),SUBMODEL)]
    ## dt.sims[,path.sim:=fnAppend(path.sim.0,submodel),by=.(SUBMODEL)]
    ## dt.sims[,fn.sim:=basename(path.sim),by=.(SUBMODEL)]
    ## dt.sims[,run.sim:=fnExtension(fn.sim,""),by=.(SUBMODEL)]
    
    ## dt.sims <- data.table(SUBMODEL=1:nsims)
    newpars[,ROW:=.I]
    length.num.char <- newpars[,ceiling(log10(uniqueN(SUBMODEL)+1))]
    newpars[,submodel:=sprintf(fmt=paste0("%0",length.num.char,"d"),SUBMODEL)]
    newpars[,path.sim:=fnAppend(path.sim.0,submodel),by=.(ROW)]
    newpars[,fn.sim:=basename(path.sim)]
    newpars[,run.sim:=fnExtension(fn.sim,"")]

    if(!is.null(write.ext)){
        saveRDS(newpars,file=write.ext)
    }
    
### create control streams one by one
    res <- newpars[,
                   NMreplaceInits(files=unique(path.sim.0)
                                 ,newfile=unique(path.sim)
                                 ,fix=TRUE
                                 ,inits=.SD
                                 ,quiet=TRUE)
                  ,by="SUBMODEL"]


### output tables.
    ## gsub the sim name string with a new subsetted simname string.
    sec.0 <- NMreadSection(file=path.sim.0,section="TABLE")
    newpars[,{
        sec.new <- gsub(run.sim.0,unique(run.sim),x=sec.0)
        NMwriteSection(files=path.sim,section="TABLE",newlines=sec.new,quiet=TRUE)
    },by=.(SUBMODEL)]


    invisible(unique(newpars$path.sim))
}
