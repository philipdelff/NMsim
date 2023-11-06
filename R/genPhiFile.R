##' @import data.table

genPhiFile <- function(data,file){
##     if(missing(file)) file <- NULL

    name.tab <- NULL
    SUBJECT_NO <- NULL
    
    dt.covs <- findCovs(data,by="ID",as.fun="data.table")
    ### it should be checked against the ext file that we found all the etas.
    
    cnames.covs <- colnames(dt.covs)
    dt.names <- data.table(
        name.tab = cnames.covs[grepl(pattern="ET[A]*[1-9][0-9]*",cnames.covs)]
    )
    dt.names[,I:=sub("ET[A]([1-9][0-9]*)","\\1",name.tab) |> as.numeric()]
    setorder(dt.names,I)

    
    dt.etas <- dt.covs[,c("ID",dt.names$name.tab),with=FALSE]
    dt.etas[,SUBJECT_NO:=.I]
    setcolorder(dt.etas,"SUBJECT_NO")
    
    fwrite(dt.etas,file=file,sep=" ")
    lines.phi <- readLines(file)
    lines.phi <- paste(" ",lines.phi)

#### Could this say "Derived from output tables by NMsim"?
    ## lines.phi <- c("TABLE NO.     1: First Order Conditional Estimation with Interaction: Problem=1 Subproblem=0 Superproblem1=0 Iteration1=0 Superproblem2=0 Iteration2=0",lines.phi)
    lines.phi <- c("TABLE NO.     1: Derived from output tables by NMsim",lines.phi)

    writeTextFile(lines.phi,file)
    return(invisible(lines.phi))

}
