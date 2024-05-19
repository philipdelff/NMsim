##' Add seed string to simulation model data.table
##'
##' This is an internal NMsim function.
##' 
##' @param models A data.frame containing model paths etc as created
##'     by \code{NMsim()}.
##' @param nseeds Number of seeds in each simulation control
##'     stream. Default is to match length of dist.
##' @param dist Distribution of random sources. These character
##'     strings will be pasted directly into the Nonem control streams
##'     after the seed values. Default is "" which means one normal
##'     distribution. \code{dist=c("","UNIFORM")} will give two seeds
##'     with random sources following a normal and a uniform
##'     distribution.
##' @param values Optionally, seed values. This can be a data.frame
##'     with as many columns as random sources.
##' @import data.table
##' @return An updated data.table with simulation model information
##'     including seed strings.
##' @keywords internal

## Do not export - this currently only makes sense from within NMsim

#### must be able to

## return values in a data.table
## return full strings as a vector

## identify number of existing seeds and add as necessary


NMseed <- function(models,nseeds,dist,values,fun.seed=seedFunDefault){

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    . <- NULL
    sims <- NULL
    nsim <- NULL
    seed <- NULL
    string <- NULL

### Section end: Dummy variables, only not to get NOTE's in pacakge checks


    nsims <- nrow(models)
    if(missing(dist)||is.null(dist)) dist <- ""
    if(missing(nseeds) || is.null(nseeds)) nseeds <- length(dist)

    ## check number of seeds matches models

    ## check number of seeds matches length(dist) 

    if(!missing(values) && !is.null(values)){
        ## check nrows matches
        if(!is.data.frame(values)){
            values <- data.table(values)
        }
        values <- as.data.table(values)
        if(!missing(nseeds) && !is.null(nseeds)){
            if(nseeds>ncol(values)) stop("more seeds requested than columns provided")
        }
        nseeds <- ncol(values)

    }
    

    if(length(dist) == 1 && nseeds>1 ) dist <- rep(dist,nseeds)

    
    if(missing(values)||is.null(values)) {
        values.cols <- lapply(1:nseeds,function(col) data.table(fun.seed(n=nsims)))
        ## values <- do.call(data.table:::cbind.data.table,values.cols)
        values <- do.call(cbind,values.cols)
    } 

    setnames(values,new=paste0("seed",1:ncol(values)))
    values[,nsim:=.I]
    

    values <- values[,string:=paste(paste0("(",unlist(.SD)," ",dist,")"),collapse=" "),by=.(nsim)]
    values[,string:=gsub(" \\)",")",string)]
    models[,seed:=values[,string]]
    
    return(models[])
}
