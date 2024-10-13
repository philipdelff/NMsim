##' Generate a population based on a Nonmem model
##' @param file Passed to `NMdata::NMreadExt()`. Path to ext file. By
##'     default, `NMreadExt()` uses a`auto.ext=TRUE` which means that
##'     the file name extension is replaced by `.ext`. If your ext
##'     file name extension is not `.ext`, add `auto.ext=FALSE` (see
##'     ...).
##' @param N Number of subjects to generate
##' @param seed Optional seed. Will be passed to `set.seed`. Same
##'     thing as running `set.seed` just before calling
##'     `simPopEtas()`.
##' @param pars A long-format parameter table containing par.type and
##'     i columns. If this is supplied, the parameter values will not
##'     be read from an ext file, and file has no effect. If an ext
##'     file is available, it is most likely better to use the file
##'     argument.
##' @param file.phi An optional phi file to write the generated
##'     subjects to.
##' @param as.fun The default is to return data as a data.frame. Pass
##'     a function (say `tibble::as_tibble`) in as.fun to convert to
##'     something else. If data.tables are wanted, use
##'     as.fun="data.table". The default can be configured using
##'     NMdataConf.
##' @param file.mod Deprecated. Use file instead.
##' @param ... Additional arguments passed to NMdata::NMreadExt(). Use
##'     `auto.ext=FALSE` if
##' @import data.table
##' @import NMdata
##' @importFrom MASS mvrnorm
##' @export

simPopEtas <- function(file,N,seed,pars,file.phi,as.fun,file.mod,...){

    par.type <- NULL
    i <- NULL
    ID <- NULL
    dt.res <- NULL
    TABLENO <- NULL
    
    if(!missing(seed)) set.seed(seed)
    if(missing(pars)) pars <- NULL
    if(missing(file)) file <- NULL
    if(missing(file.phi)) file.phi <- NULL

    ## name.sim
    if(!missing(file.mod)){
        if(!missing(file)){
            stop("file and file.mod supplied. Use file and not the deprecated file.mod. ")
        }
        message("file.mod is deprecated. Use file.")
        file <- file.mod
    }
    
    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdata:::NMdataDecideOption("as.fun",as.fun)

    if(is.null(pars)){
        pars <- NMreadExt(file=file,return="pars",tableno="max",as.fun="data.table",...)
    }
    
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
