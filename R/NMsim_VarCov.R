##' Simulate with parameter values sampled from a covariance step
##'
##' Like \code{NMsim_default} but `$THETA`, `$OMEGA`, and `SIGMA` are
##' drawn from distribution estimated in covariance step. This means
##' that a successful covariance step must be available from the
##' estimation. In case the simulation leads to negative diagonal
##' elements in $OMEGA and $SIGMA, those values are truncated at
##' zero. For simulation with parameter variability based on bootstrap
##' results, use \code{NMsim_default}.
##'
##' @param file.sim See \code{?NMsim}.
##' @param file.mod See \code{?NMsim}.
##' @param data.sim See \code{?NMsim}.
##' @param nsims Number of replications wanted. The default is 1. If
##'     greater, multiple control streams will be generated.
##' @import NMdata
##' @import data.table
##' @importFrom MASS mvrnorm
##' @return Character vector of simulation control stream paths
##' @export

NMsim_VarCov <- function(file.sim,file.mod,data.sim,nsims=1){

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    . <- NULL
    est <- NULL
    i <- NULL
    j <- NULL
    FIX <- NULL
    fn.sim <- NULL
    submodel <- NULL
    SUBMODEL <- NULL
    run.sim <- NULL
    NMREP <- NULL
    parameter <- NULL
    par.type <- NULL
    path.sim <- NULL
    value <- NULL

### Section end: Dummy variables, only not to get NOTE's in pacakge checks 
    

    files.needed.def <- NMsim_default(file.sim=file.sim,file.mod,data.sim)

    
    path.lst <- fnExtension(file.mod,"lst")
    path.cov <- fnExtension(path.lst,"cov")
    path.ext <- fnExtension(path.lst,"ext")
    ## Should not include NMREP. 
    ##    NMreadTabCov(path.cov,rm.name=F)

    ## define new files
    path.sim.0 <- file.sim
    run.sim.0 <- fnExtension(basename(path.sim.0),"")
    rm(file.sim)
    dt.sims <- data.table(SUBMODEL=1:nsims)
    length.num.char <- ceiling(log10(nsims+1))
    dt.sims[,submodel:=sprintf(fmt=paste0("%0",length.num.char,"d"),SUBMODEL)]
    dt.sims[,path.sim:=fnAppend(path.sim.0,submodel),by=.(SUBMODEL)]
    dt.sims[,fn.sim:=basename(path.sim),by=.(SUBMODEL)]
    dt.sims[,run.sim:=fnExtension(fn.sim,""),by=.(SUBMODEL)]
    
    
    ## nonmem2rx::nmcov(path.cov)
    covmat <- NMreadCov(path.cov)
    ests <- NMdata::NMreadExt(path.ext,as.fun="data.table")[NMREP==1,.(parameter,par.type,i,j,est,FIX)]
    ests <- ests[par.type%in%c("THETA","SIGMA","OMEGA")]
    ## ests <- NMreadExt(path.ext)$pars[NMREP==1,.(parameter,par.type,i,j,est,FIX)]
    ests <- ests[match(ests$parameter,colnames(covmat))]
    
    
    newpars <- mvrnorm(n=nsims,Sigma=covmat,mu=ests$est)
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

    newpars <- mergeCheck(newpars,dt.sims,by="SUBMODEL")
    ## if the parameter was fixed, reset it to the estimate
    newpars[FIX==1,value:=est]
    ## if OMEGA or SIGMA diagonal elements are <0 set to 0.
    newpars[i==j&value<0,value:=0]
    
    ## newpars[,est:=NULL]
    ## setnames(newpars,"value","est")
    
    
### create control streams one by one
    res <- newpars[,
                   NMreplaceInits(files=unique(path.sim.0)
                                 ,newfile=unique(path.sim)
                                 ,fix=TRUE
                                 ,inits=.SD)
                  ,by="SUBMODEL"]
    

### output tables.
    ## gsub the sim name string with a new subsetted simname string.
    sec.0 <- NMreadSection(file=path.sim.0,section="TABLE")
    dt.sims[,{
        sec.new <- gsub(run.sim.0,run.sim,x=sec.0)
        NMwriteSection(files=path.sim,section="TABLE",newlines=sec.new)
    },by=.(SUBMODEL)]

    
    invisible(unique(newpars$path.sim))
}
