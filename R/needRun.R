##' get arguments passed to a function
##'
##' @param which The number of environment levels to move. Default is
##'     1 because that means the result is concerns the
##'     function/environment in which callArgs() is executed.
##' @return List of arguments

callArgs <- function(which=-1){

### args in call.
    ## no args are needed for this
    args.call <- as.list( match.call(
        definition = sys.function(which=which),
        call = sys.call(which=which)
    ) )[-1]
    

    argsconts <- lapply(args.call, eval, envir = parent.frame())
    ##    digest(argsconts)

    argsconts
}


##' Get digest checksum of object elements
##' 
##' @param obj An object with elements to run digest on.
##' @param funs Named list of functions to be applied to elements (matched on names) in `obj`. Optional.
##' @return A data.table with hashes/checksums.
##' @importFrom digest digest
## Made for needRun. Don't export.
digestElements <- function(obj,funs){

    if(missing(funs)) funs <- NULL
    nms.funs <- names(funs)
    ## modify the elements in obj that have associated functions in
    ## funs
    for(na in nms.funs){
        if(is.null(obj[[na]])){
            obj[[na]] <- NULL
        } else {
            obj[[na]] <- funs[[na]](obj[[na]])
        }
    }
    obj
    dtapply(obj,digest)
}

### need a digests of argument values, including their contents.

### More than that, additionally digest of a list of contents derived
### from arguments, like contents of a file which path is an arg.

##' Determine whether re-run is necessary
##'
##' Compares arguments against stored checksums
##'
##' @param path.res Path to function results output file
##' 
needRun <- function(path.res,path.digest,funs){

    run.fun <- TRUE
    digest.old <- NULL
    if(file.exists(path.digest)){
        digest.old <- readRDS(path.digest)
    }
    digest.new <- NULL
    if(file.exists(path.res)){
        obj.fun <- callArgs(which = -2)
        digest.new <- digestElements(obj.fun,funs=funs)
    }
    digest.all <- NULL
    if(!is.null(digest.old)&&!is.null(digest.new)){
        
        digest.all <- try(merge(digest.new,digest.old,by="name",suffixes=c(".new",".old"),all.x=T,all.y=T))
        if(inherits(digest.all,"try-error")){
            run.fun <- TRUE
            digest.all <- NULL
        } else if(
                   all(
                       digest.all[,isTRUE(res.new==res.old),by=.(name)][,V1]
                   )
               ){
            run.fun <- FALSE
        }
    }
    list(needRun=run.fun,digest.new=digest.new,digest.all=digest.all)
}
