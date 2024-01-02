##' Remove NMsimTab class and discard NMsimTab meta data
##' @param x An 'NMsimTab' object.
##' @return x stripped from the 'NMsimTab' class
##' @export
##' @rdname NMsimTabOperations
unNMsimTab <- function(x){
    setattr(x,"NMsimTab",NULL)
    setattr(x,"class",setdiff(class(x),"NMsimTab"))
}


##' Check if an object is 'NMsimTab'
##' @param x Any object
##' @return logical if x is an 'NMsimTab' object
##' @export
##' @rdname NMsimTabOperations
is.NMsimTab <- function(x){
    inherits(x,"NMsimTab")
}


##' Basic arithmetic on NMsimTab objects
##'
##' @param x an NMsimTab object
##' @param ... arguments passed to other methods.
##' @details When 'dimnames', 'merge', 'cbind', 'rbind', or 't' is
##'     called on an 'NMsimTab' object, the 'NMsimTab' class is dropped,
##'     and then the operation is performed. So if and 'NMsimTab' object
##'     inherits from 'data.frame' and no other classes (which is
##'     default), these operations will be performed using the
##'     'data.frame' methods. But for example, if you use 'as.fun' to
##'     get a 'data.table' or 'tbl', their respective methods are used
##'     instead.
##' @return An object that is not of class 'NMsimTab'.
##' @name NMsimTabOperations
NULL

##' @rdname NMsimTabOperations
##' @export
merge.NMsimTab <- function(x,...){
    unNMsimTab(x)
    merge(x,...)
}

##' @rdname NMsimTabOperations
##' @export
t.NMsimTab <- function(x,...){
    unNMsimTab(x)
    t(x,...)
}

##' @rdname NMsimTabOperations
##' @export
dimnames.NMsimTab <- function(x,...){
    unNMsimTab(x)
    dimnames(x,...)
}

##' @rdname NMsimTabOperations
##' @export
rbind.NMsimTab <- function(x,...){
    
    unNMsimTab(x)
    rbind(x,...)
}

##' @rdname NMsimTabOperations
##' @export
cbind.NMsimTab <- function(x,...){
    unNMsimTab(x)
    cbind(x,...)
}

