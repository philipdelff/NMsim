##' get arguments passed to a function
##'
##' Within a function, run callArgs to get a list of all arguments
##' passed to the function.
##' @param which The number of environment levels to move. Default is
##'     1 because that means the result is concerns the
##'     function/environment in which callArgs() is executed.
##' @return List of arguments
##' @examples
##' funfoo <- function(a,b){
##'   NMsim:::callArgs()
##' }
##' funfoo(a=1)

callArgs <- function(which=-1){
    
### args in call.
    ## no args are needed for this
    args.call <- as.list(
        match.call(
            definition = sys.function(which=which),
            call = sys.call(which=which)
        )
    )[-1]
    
    ### should this be envir=parent.frame(-which) ?
    argsconts <- lapply(args.call, eval, envir = parent.frame(n=-which))
    ##    digest(argsconts)

    argsconts
}


##' Derive digests of argument values or their contents
##' 
##' Get digests of argument values. Optionally, functions can be run
##' on some arguments before calculating digests. An example would be
##' reading contents of a file where the file path is an argument.
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


##' Determine whether re-run of a function is necessary
##'
##' Compares arguments against stored checksums or output of functions
##' of them. If changes found, the function should be re-run.
##'
##' @param path.res Path to function results output file
##' @param path.digest Path to the file containing digests to compare
##'     to.
##' @param funs Named list of functions to apply to arguments
##' @param which Number of environment levels to jump to evaluate the
##'     arguments
##' 
needRun <- function(path.res,path.digest,funs,which=-2){
    
#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    res.new <- NULL
    res.old <- NULL
    . <- NULL
    name <- NULL
    V1 <- NULL
    
### Section end: Dummy variables, only not to get NOTE's in pacakge checks

    
    run.fun <- TRUE
    digest.old <- NULL
    if(file.exists(path.digest)){
        digest.old <- readRDS(path.digest)
    }
    digest.new <- NULL
    if(file.exists(path.res)){
        
        obj.fun <- callArgs(which = which)
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
