##' @keywords internal

NMupdateInitsFix <- function(file.mod,new.mod){
    
    par.type <- NULL
    i <- NULL
    est <- NULL
    j <- NULL
    
    ext <- NMreadExt(fnExtension(file.mod,"ext"))
    
    ## create THETA section
    thetas <- ext$pars[par.type=="THETA"]
    setorder(thetas,i)
    lines.theta <- c("$THETA",
                     paste(thetas[,est],"FIX")
                     )
    ## create OMEGA section
    
    omegas <- ext$pars[par.type=="OMEGA"]
    Netas <- omegas[,max(i)]
    setorder(omegas,i,j)
    dt.diag <- dcast(omegas,j~i,value.var="est")
    list.lines <- lapply(dt.diag[,!("j")],function(x)paste(x[!is.na(x)],collapse=" "))
    lines.omega <- c(sprintf("$OMEGA BLOCK(%d) FIX",Netas)
                    ,unlist(list.lines,use.names=FALSE)
                     )
    
    ## create SIGMA section
    sigmas <- ext$pars[par.type=="SIGMA"]
    Netas <- sigmas[,max(i)]
    setorder(sigmas,i,j)
    dt.diag <- dcast(sigmas,j~i,value.var="est")
    list.lines <- lapply(dt.diag[,!("j")],function(x)paste(x[!is.na(x)],collapse=" "))
    lines.sigma <- c(sprintf("$SIGMA BLOCK(%d) FIX",Netas)
                    ,unlist(list.lines,use.names=FALSE)
                     )

    list.sections <- list(THETA=lines.theta
                         ,OMEGA=lines.omega
                         ,SIGMA=lines.sigma)
    
    NMwriteSection(file.mod
                  ,newfile=new.mod
                  ,list.sections=list.sections
                  ,backup=FALSE
                  ,quiet=TRUE)

    invisible(list.sections)
}
