NMsim_VarCov <- function(path.sim,path.mod,data.sim,nsims=1){

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    submodel <- NULL
    SUBMODEL <- NULL
    . <- NULL
    fn.sim <- NULL
    run.sim <- NULL
    NMREP <- NULL
    parameter <- NULL
    par.type <- NULL
    i <- NULL
    j <- NULL
    est <- NULL

### Section end: Dummy variables, only not to get NOTE's in pacakge checks 


    
    
    files.needed.def <- NMsim_default(path.sim=path.sim,path.mod,data.sim)

    
    path.lst <- fnExtension(path.mod,"lst")
    path.cov <- fnExtension(path.lst,"cov")
    path.ext <- fnExtension(path.lst,"ext")
    ## Should not include NMREP. 
    ##    NMreadTabCov(path.cov,rm.name=F)

    ## define new files
### we generate two sims
    path.sim.0 <- path.sim
    run.sim.0 <- fnExtension(basename(path.sim.0),"")
    rm(path.sim)
    dt.sims <- data.table(SUBMODEL=1:nsims)
    length.num.char <- ceiling(log10(nsims+1))
    dt.sims[,submodel:=sprintf(fmt=paste0("%0",length.num.char,"d"),SUBMODEL)]
    dt.sims[,path.sim:=fnAppend(path.sim.0,submodel),by=.(SUBMODEL)]
    dt.sims[,fn.sim:=basename(path.sim),by=.(SUBMODEL)]
    dt.sims[,run.sim:=fnExtension(fn.sim,""),by=.(SUBMODEL)]
    
    
    ## nonmem2rx::nmcov(path.cov)
    covmat <- NMreadCov(path.cov)
    ests <- NMreadExt(path.ext)$pars[NMREP==1,.(parameter,par.type,i,j,est)]
    ests <- ests[match(ests$parameter,colnames(covmat))]

    
    newpars <- mvrnorm(n=nsims,Sigma=covmat,mu=ests$est)
    newpars <- as.data.table(newpars)
    newpars[,SUBMODEL:=.I]
    

    newpars <- mergeCheck(melt(newpars,id.vars="SUBMODEL",variable.name="parameter")
                         ,ests
                         ,by="parameter",quiet=TRUE)

    newpars <- mergeCheck(newpars,dt.sims,by="SUBMODEL")
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
