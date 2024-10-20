##' Sample model parameters using the `simpar` package
##' @param file.mod Path to model control stream. Will be used for
##'     both `NMreadExt()` and `NMreadCov()`, and extension will
##'     automatically be replaced by `.ext` and `.cov`.
##' @param nsim Number of sets of parameter values to generate. Passed
##'     to `simpar`.
##' @param format "ext" (default) or "wide".
##' @import NMdata
##' @return A table with sampled model parameters
##' @author Sanaya Shroff, Philip Delff
##' @export

sampleParsSimpar <- function(file.mod,nsim,format="ext"){

    if(packageVersion("NMdata")<"0.1.7.905"){
        stop("NMsim_NWPRI requires NMdata 0.1.7 or later.")
    }


    DF2 <- NULL 
    iblock  <- NULL
    par.type <- NULL
    value <- NULL
    
    loadres <- requireNamespace("simpar",quietly=TRUE)
    if(!loadres) {
        message("simpar not available. Please install from github or MPN.")
        return(NULL)        
    }

    ## read param distributions from ext file
    pars <- NMreadExt(file=file.mod,as.fun="data.table")

    ## calculate degrees of freedom
    omega.sigma.dfs <- NWPRI_df(pars)

    ## variance-covariance for THETAs
    covar <- NMreadCov(file=file.mod)
    theta.covar <- covar[grep("^THETA",rownames(covar))
                        ,grep("^THETA",colnames(covar))]

    ## variance-covariance for OMEGAs
    omegas <- pars[par.type=="OMEGA" & !is.na(iblock),]
    omegas.list <- split(omegas,by="iblock")
    omega.mat.list <- 
        lapply(omegas.list,
               NMdata::dt2mat)

    ## variance-covariance for SIGMAs
    sigmas <- pars[par.type=="SIGMA" & !is.na(iblock),]
    sigmas.list <- split(sigmas,by="iblock")
    sigma.mat.list <- 
        lapply(sigmas.list,
               NMdata::dt2mat)

    ## use simpar to sample params
    pars <- simpar::simpar(
                        nsim = nsim,
                        theta = pars[par.type=="THETA",value],
                        covar = theta.covar,
                        omega = omega.mat.list,
                        odf = omega.sigma.dfs[par.type=="OMEGA",DF2],
                        sigma = sigma.mat.list,
                        sdf = omega.sigma.dfs[par.type=="SIGMA",DF2]
                    ) 

    if(format=="ext"){
        ## read in parameters simulated with simpar
        pars <- readParsWide(
            data=pars,as.fun="data.table"
        )
    }
    pars
}
