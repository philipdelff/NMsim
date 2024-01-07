##' Generate a population based on a Nonmem model
##' @param file.mod Path to input control stream
##' @param N Number of subjects to generate
##' @param seed Optional seed. Will be passed to `set.seed`. Same
##'     thing as running `set.seed` just before calling
##'     `simPopEtas()`.
##' @param file.phi An optional phi file to write the generated subjects to.
##' @param as.fun The default is to return data as a data.frame. Pass
##'     a function (say tibble::as_tibble) in as.fun to convert to
##'     something else. If data.tables are wanted, use
##'     as.fun="data.table". The default can be configured using
##'     NMdataConf.
##' @import data.table
##' @import NMdata
##' @importFrom MASS mvrnorm


simPopEtas <- function(file.mod,N,seed,file.phi,as.fun){

    par.type <- NULL
    i <- NULL
    ID <- NULL
    dt.res <- NULL
    TABLENO <- NULL
    
    if(!missing(seed)) set.seed(seed)
    if(missing(file.phi)) file.phi <- NULL
    
    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdata:::NMdataDecideOption("as.fun",as.fun)
    
    pars <- NMdata::NMreadExt(file=fnExtension(file.mod,"ext"),return="pars",as.fun="data.table")
### NMdata::NMreadExt can do this starting from NMdata 0.1.4. For now,
### we make sure only one step is used this way.
    pars <- pars[TABLENO==max(TABLENO)]

    Netas <- pars[par.type=="OMEGA",max(i)]

    Sigma <- dt2mat(pars[par.type=="OMEGA"])
    dt.etas <- as.data.table(mvrnorm(n=N,mu=rep(0,Netas),Sigma=Sigma))
    colnames(dt.etas) <- paste0(substring("ETA",1,3-(1:Netas)%/%10),1:Netas)
    dt.etas[,ID:=1:N]
    setcolorder(dt.etas,"ID")
    
    if(!is.null(file.phi)){
        genPhiFile(data=dt.etas,file=file.phi)
        return(invisible(as.fun(dt.etas)))
    }
    return(as.fun(dt.etas))
    
}
