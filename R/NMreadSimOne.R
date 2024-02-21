##' Read simulation results from data.frames or fst files
##' @param x a data set or a fst file
##' @import data.table
##' @keywords internal
## don't export


NMreadSimRes <- function(x){

    
    res.list <- lapply(x,NMreadSimResOne)
    res <- rbindlist(res.list,fill=TRUE)

    addClass(res,"NMsimRes")
    res
}

##' read one sim element. This will be run in lapply in NMreadSim.
##' @param x A path to an fst file or a data set
##' @return A data.table
##' @importFrom fst read_fst
##' @keywords internal
NMreadSimResOne <- function(x){

    if(is.character(x)){
        x <- read_fst(x,as.data.table=TRUE)
    }

    x
    
}
