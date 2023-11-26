##' Replace initial values in Nonmem control stream
##' @param inits A data.frame with new initial estimates, same style
##'     as returned by NMreadExt. Column` par.type` can contain
##'     elements THETA, OMEGA, SIGMA.
##' @param fix Fix the initial values? Default is not to.
##' @param ... Passed to NMdata::NMwriteSection. This is important for
##'     NMreplaceInits to run at all.
##' @return The modified control stream
##' @keywords internal

NMreplaceInits <- function(inits,fix=FALSE,...){


#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    par.type <- NULL
    i <- NULL
    value <- NULL
    j <- NULL

### Section end: Dummy variables, only not to get NOTE's in pacakge checks



    if(!isTRUE(fix)){stop("fix must be TRUE. Not fixing the parameters is not supported for now.")}

    if(fix) {
        str.fix <- "FIX"
    } else {
        str.fix <- ""
    }
    
    ## create THETA section
    thetas <- inits[par.type=="THETA"]
    setorder(thetas,i)
    lines.theta <- c("$THETA",
                     paste(thetas[,value],str.fix)
                     )
    
    ## create OMEGA section
    omegas <- inits[par.type=="OMEGA"]

    lines.omega <- NMcreateMatLines(omegas,type="OMEGA")
    sigmas <- inits[par.type=="SIGMA"]    
    lines.sigma <- NMcreateMatLines(sigmas,type="SIGMA")

    ##     omegas[value==0,value:=1e-30]
    ##     Netas <- omegas[,max(i)]
    ##     setorder(omegas,i,j)
    ## ## this is the diag and the upper triangle only
    ##     dt.diag <- dcast(omegas,j~i,value.var="value")
    ##     list.lines <- lapply(dt.diag[,!("j")],function(x)paste(x[!is.na(x)],collapse=" "))
    ##     lines.omega <- c(sprintf("$OMEGA BLOCK(%d) %s",Netas,str.fix)
    ##                     ,unlist(list.lines,use.names=FALSE)
    ##                      )
    
    ## create SIGMA section
    ## sigmas <- inits[par.type=="SIGMA"]
    ## sigmas[value==0,value:=1e-30]
    ## Netas <- sigmas[,max(i)]
    ## setorder(sigmas,i,j)
    ## dt.diag <- dcast(sigmas,j~i,value.var="value")
    ## list.lines <- lapply(dt.diag[,!("j")],function(x)paste(x[!is.na(x)],collapse=" "))
    ## lines.sigma <- c(sprintf("$SIGMA BLOCK(%d) %s",Netas,str.fix)
    ##                 ,unlist(list.lines,use.names=FALSE)
    ##                  )

    list.sections <- list(THETA=lines.theta
                         ,OMEGA=lines.omega
                         ,SIGMA=lines.sigma)

    
    res <- NMwriteSection(list.sections=list.sections
                         ,...
                          )

    invisible(res)
}
