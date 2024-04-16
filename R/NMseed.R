##' import data.table

#### must be able to

## return values in a data.table
## return full strings as a vector

NMseed <- function(models,nseeds,dist,values,return.strings=TRUE){

    val.max <- 2147483647
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
        values.cols <- lapply(1:nseeds,function(col) data.table(round(runif(n=nsims)*val.max)))
        values <- do.call(data.table:::cbind.data.table,values.cols)

    } 

    setnames(values,new=paste0("seed",1:ncol(values)))        
    values[,nsim:=.I]
    
    if(return.strings){
        values <- values[,string:=paste(paste0("(",unlist(.SD)," ",dist,")"),collapse=" "),by=.(nsim)]
        values[,string:=gsub(" \\)",")",string)]
    }
    models[,seed:=values[,string]]
    
    return(models[])
}
