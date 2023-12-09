##' Generate a .phi file for further simulation with Nonmem
##'
##' This will typically be used in a couple of different
##' situations. One is if a number of new subjects have been simulated
##' and their ETAs should be reused in subsequent simulations. Another
##' is internally by NMsim when simulating new subjects from models
##' estimated with SAEM.
##' 
##' @param data A dataset that contains "ID" and all ETAs. This can be
##'     obtained by `NMdata::NMscanData`.
##' @param file Path to the .phi file to be written.
##' @import data.table
##' @export



genPhiFile <- function(data,file){
##     if(missing(file)) file <- NULL

    
    name.eta.phi <- NULL
    name.tab <- NULL
    SUBJECT_NO <- NULL
    
    dt.covs <- findCovs(data,by="ID",as.fun="data.table")
    ### it should be checked against the ext file that we found all the etas.
    
    cnames.covs <- colnames(dt.covs)
    dt.names <- data.table(
        name.tab = cnames.covs[grepl(pattern="ET[A]*[1-9][0-9]*",cnames.covs)]
    )
    dt.names[,I:=as.numeric(sub("ET[A]*([1-9][0-9]*)","\\1",name.tab))]
    dt.names[,name.eta.phi:=sprintf("ETA(%s)",I)]
    setorder(dt.names,I)
    
    
    dt.etas <- dt.covs[,c("ID",dt.names$name.tab),with=FALSE]
    setnames(dt.etas,dt.names$name.tab,dt.names$name.eta.phi)
    dt.etas[,SUBJECT_NO:=.I]
    setcolorder(dt.etas,"SUBJECT_NO")
    
    fwrite(dt.etas,file=file,sep=" ")
    lines.phi <- readLines(file)
    lines.phi <- paste(" ",lines.phi)


    lines.phi <- c("TABLE NO.     1: Derived from output tables by NMsim",lines.phi)

    writeTextFile(lines.phi,file)
    return(invisible(lines.phi))

}
