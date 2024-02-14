#### a function that reads one sim element. This will be run in lapply in NMreadSim.

## don't export

##' @param x a data set or a fst file
NMreadSimRes <- function(x){

    
    res.list <- lapply(x,NMreadSimResOne)
    res <- rbindlist(res.list,fill=TRUE)

    addClass(res,"NMsimRes")
    res
}


##' @importFrom fst read_fst
NMreadSimResOne <- function(x){

    if(is.character(x)){
        x <- read_fst(x)
    }

    x
    
}
