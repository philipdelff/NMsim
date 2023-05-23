### need a digests of argument values, including their contents.

### More than that, additionally digest of a list of contents derived
### from arguments, like contents of a file which path is an arg.


callArgs <- function(which=-1){

### args in call.
    ## no args are needed for this
    args.call <- as.list( match.call(
        def = sys.function( which ),
        call = sys.call(which)
    ) )[-1]
    

    argsconts <- lapply(args.call, eval, envir = parent.frame())
    ##    digest(argsconts)

    argsconts
}


## generally applicable but still too early to export
dtapply <- function(...){
    res.list <- lapply(...)
    dt1 <- data.table(name=names(res.list),res=unlist(res.list))
    dt1
}


## NMsim: get digest of path.mod

##' @param args
##' @importFrom digest digest
## args is a list. Element names correspond to arg names. Content
## is a function that returns digest.

addCallRes <- function(obj,args){

    if(missing(args)) args <- NULL
    nms.args <- names(args)
    ## modify the elements in obj that have associated functions in
    ## args
    for(na in nms.args){
        if(is.null(obj[[na]])){
            obj[[na]] <- NULL
        } else {
            obj[[na]] <- args[[na]](obj[[na]])
        }
    }
    obj
    dtapply(obj,digest)
}


##' @export
##' 
needRun <- function(path.res,path.digest){

    run.fun <- TRUE
    digest.old <- NULL
    if(file.exists(path.digest)){
        digest.old <- readRDS(path.digest)
    }
    digest.new <- NULL
    if(file.exists(path.res)){
        obj.fun <- callArgs(which = -2)
        digest.new <- addCallRes(obj.fun,args=list(file=readLines))
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
