##' Remove NMsimRes class and discard NMsimRes meta data
##' @param x An 'NMsimRes' object.
##' @return x stripped from the 'NMsimRes' class
##' @export
##' @rdname NMsimResOperations
unNMsimRes <- function(x){
    if(!is.null(x)){
        ## setattr(x,"NMsimRes",NULL)
        setattr(x,"NMsimModTab",NULL)
        setattr(x,"class",setdiff(class(x),"NMsimRes"))
    }
}


##' Check if an object is 'NMsimRes'
##' @param x Any object
##' @return logical if x is an 'NMsimRes' object
##' @export
##' @rdname NMsimResOperations
is.NMsimRes <- function(x){
    inherits(x,"NMsimRes")
}


##' Basic arithmetic on NMsimRes objects
##'
##' @param x an NMsimRes object
##' @param ... arguments passed to other methods.
##' @details When 'dimnames', 'merge', 'cbind', 'rbind', or 't' is
##'     called on an 'NMsimRes' object, the 'NMsimRes' class is dropped,
##'     and then the operation is performed. So if and 'NMsimRes' object
##'     inherits from 'data.frame' and no other classes (which is
##'     default), these operations will be performed using the
##'     'data.frame' methods. But for example, if you use 'as.fun' to
##'     get a 'data.table' or 'tbl', their respective methods are used
##'     instead.
##' @return An object that is not of class 'NMsimRes'.
##' @name NMsimResOperations
NULL

##' @rdname NMsimResOperations
##' @export
merge.NMsimRes <- function(x,...){
    unNMsimRes(x)
    merge(x,...)
}

##' @rdname NMsimResOperations
##' @export
t.NMsimRes <- function(x,...){
    unNMsimRes(x)
    t(x,...)
}

##' @rdname NMsimResOperations
##' @export
dimnames.NMsimRes <- function(x,...){
    unNMsimRes(x)
    dimnames(x,...)
}

##' @rdname NMsimResOperations
##' @export
rbind.NMsimRes <- function(x,...){
    
    list.ModTab <- lapply(c(list(x),list(...)),function(y)attributes(y)$NMsimModTab)
    ## list.ModTab <- list.ModTab[!sapply(list.ModTab,is.null)]
    ModTab <- rbindlist(list.ModTab,fill=TRUE)

### solution 1
    unNMsimRes(x)
    ## lapply(c(x,list(...)),unNMsimRes)
    res <- rbind(x,...)
    
    ## args.res.list <- c(x,list(...))
    ## args.res.list <- args.res.list[!sapply(args.res.list,is.null)]
    ## res <- do.call(rbind,args.res.list)
### solution 2
    ## res <- data.table:::rbind.data.table(x,...)

    addClass(res,"NMsimRes")
    setattr(res,"NMsimModTab",ModTab)
    res
}

##' @rdname NMsimResOperations
##' @export
cbind.NMsimRes <- function(x,...){
    unNMsimRes(x)
    cbind(x,...)
}

