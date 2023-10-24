##' Add residual variability based on parameter estimates
##' @param data A data set containing indiviudual predictions. Often a
##'     result of NMsim.
##' @param path.ext Path to the ext file to take the parameter
##'     estimates from.
##' @param prop Parameter number of parameter holding variance of the
##'     proportional error component. If ERR(1) is used for
##'     proportional error, use prop=1. Can also refer to a theta
##'     number.
##' @param add Parameter number of parameter holding variance of the
##'     additive error component. If ERR(1) is used for additive
##'     error, use add=1. Can also refer to a theta number.
##' @param log Should the error be added on log scale? This is used to
##'     obtain an exponential error distribution.
##' @param par.type Use "sigma" if variances are estimated with the
##'     SIGMA matrix. Use "theta" if THETA parameters are used. See
##'     `scale.par` too.
##' @param trunc0 If log=FALSE, truncate simulated values at 0? If
##'     trunc0, returned predictions can be negative.
##' @param scale.par Denotes if parmeter represents a variance or a
##'     standard deviation. Allowed values and default value depends
##'     on `par.type`.  \itemize{ \item if par.type="sigma" only "var"
##'     is allowed.  \item if par.type="theta" allowed values are "sd"
##'     and "var". Default is "sd".}
##' @param subset A character string with an expression denoting a
##'     subset in which to add the residual error. Example:
##'     subset="DVID=='A'"
##' @param seed A number to pass to set.seed() before
##'     simulating. Default is to generate a seed and report it in the
##'     console. Use seed=FALSE to avoid setting the seed (if you
##'     prefer doing it otherwise).
##' @param as.fun The default is to return data as a data.frame. Pass
##'     a function (say tibble::as_tibble) in as.fun to convert to
##'     something else. If data.tables are wanted, use
##'     as.fun="data.table". The default can be configured using
##'     NMdataConf.
##' @param col.ipred The name of the column containing individual
##'     predictions.
##' @param col.ipredvar The name of the column to be created by
##'     addResVar to contain the simulated observations (individual
##'     predictions plus residual error).
##' @import MASS
##' @import data.table
##' @import NMdata
##' @return An updated data.frame
##' @examples
##' \dontrun{
##' ## based on SIGMA
##' simres.var <- addResVar(data=simres,
##'                         path.ext = "path/to/model.ext"
##'                         prop = 1,
##'                         add = 2,
##'                         par.type = "SIGMA",
##'                         log = FALSE)
##'
##' ## If implemented using THETAs
##' simres.var <- addResVar(data=simres,
##'                         path.ext = "path/to/model.ext"
##'                         prop = 8, ## point to elements in THETA
##'                         add = 9,  ## point to elements in THETA
##'                         par.type = "THETA",
##'                         log = FALSE)
##'
##' }
##'
##' @export



addResVar <- function(data,path.ext,prop=NULL,add=NULL,log=FALSE,par.type="SIGMA",trunc0=TRUE,scale.par,subset,seed,col.ipred="IPRED",col.ipredvar="IPREDVAR",as.fun){

    . <- NULL
    ERRadd <- NULL
    ERRprop <- NULL
    IPRED <- NULL
    IPREDVAR <- NULL
    est <- NULL
    ext.par.type <- NULL
    file.mod <- NULL
    i <- NULL
    j <- NULL
    sd <- NULL
    sigma <- NULL
    theta <- NULL
    var <- NULL
    
#### Section start: Pre-process arguments ####


    ## data - make sure data is a data.table but no edit by ref. If we
    ## are carefull in not overwriting anything, we can skip the copy
    ## for faster processing.
    if(!is.data.frame(data)){stop("data must be a data.frame")}
    if(nrow(data)==0) {
        warning("data is empty. Nothing to do.")
        ## as.fun <- NMdataDecideOption("as.fun",as.fun)
        data <- as.fun(data)
        return(data)
    }

    data.was.data.table <- TRUE
    if(is.data.table(data)){
        data <- copy(data)
    } else {
        data.was.data.table <- FALSE
        data <- as.data.table(data)
    }
    if(missing(subset)) subset <- TRUE
    data.sub <- data[eval(parse(text=subset))]
    
    if(missing(prop)) prop <- NULL
    if(missing(add)) add <- NULL
    par.type <- simpleCharArg("par.type",par.type,default="sigma",accepted=cc(sigma,theta),lower=TRUE,clean=TRUE)

    ## scale.par
    if(missing(scale.par)) scale.par <- NULL
    scale.par <- switch(par.type,
                        theta=simpleCharArg("scale.par",scale.par,default="sd",accepted=cc(sd,var),lower=TRUE,clean=TRUE),
                        sigma=simpleCharArg("scale.par",scale.par,default="var",accepted=cc(var),lower=TRUE,clean=TRUE)
                        )

    if(missing(seed)) seed <- NULL
    if(is.null(seed)) {
        seed <- sample(1:1e8,size=1)
        message(paste("seed is",seed))
    }

    ## as.fun
    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdata:::NMdataDecideOption("as.fun",as.fun)


###  Section end: Pre-process arguments

    if(!isFALSE(seed)){
        set.seed(seed)
    }
    
    ## get parameter estimates
    ext <- NMreadExt(path.ext)
    nerrs <- length(c(prop,add))
    
    setnames(ext$pars,"par.type","ext.par.type")
    pars <- ext$pars[ext.par.type==toupper(par.type)&i%in%c(prop,add)]

    name.err <- c()
    if(!is.null(prop)) name.err <- c(name.err,"ERRprop")
    if(!is.null(add)) name.err <- c(name.err,"ERRadd")

    dt.err.names <- data.table(i=c(prop,add),name.err=name.err)
    
    ## in case of sigma, create matrix from triangle
    if(par.type=="sigma"){
        pars.mat <- rbind(pars,
                          pars[i!=j]
                          [,.(ext.par.type,i=j,j=i,est)]
                         ,fill=T)
        ## note, dcast returns a keyed data.table (keys are LHS vars) so it is always ordered by i.
        matrix.pars <- as.matrix(dcast(pars.mat,i~j,value.var="est")[,!("i")])
    } else {
        if(scale.par=="sd"){
            matrix.pars <- diag(pars$est^2)
        } else {
            matrix.pars <- diag(pars$est)
        }
    }

    
    nrows <- data.sub[,.N]
    errs <- mvrnorm(n=nrows,mu=rep(0,nerrs),Sigma=matrix.pars)
    errs <- as.data.table(errs)
    setnames(errs,dt.err.names[order(i),name.err])



    ## calc predictions
    if(log){
        data[eval(parse(text=subset)),(col.ipredvar):=exp(log(get(col.ipred))*(1+errs[,ERRprop])+errs[,ERRadd])]
    } else {
        data[eval(parse(text=subset)),(col.ipredvar):=get(col.ipred)*(1+errs[,ERRprop])+errs[,ERRadd]]
        if(trunc0) {
            data[eval(parse(text=subset))&get(col.ipredvar)<0,(col.ipredvar):=0]
        }
    }

    as.fun(data)
}


