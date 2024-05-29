## most commonly, we want to create a simulation section for a model that doesn't have one.

## if a uniform or some other dist is needed, maybe it will always be for the second source/seed because the first one must be normal for ETA's.

## we may need to aff more than one


## Internal function to extract seed information from a control stream

NMseedExtract <- function(file.mod,lines){

    if(!is.null(file.mod)){
        list.sec.sim <- NMreadSection(file.mod,as.one=FALSE,simplify=FALSE)
        ## currently, only one sim section is supported
        stopifnot(length(list.sec.sim)<=1)
        lines <- list.sec.sim[[1]]
    }
    
}
