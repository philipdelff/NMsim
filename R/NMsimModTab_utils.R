##' Remove NMsimModTab class and discard NMsimModTab meta data
##' @param x An 'NMsimModTab' object.
##' @return x stripped from the 'NMsimModTab' class
##' @export
##' @rdname NMsimModTabOperations
unNMsimModTab <- function(x){
    setattr(x,"NMsimModTab",NULL)
    setattr(x,"class",setdiff(class(x),"NMsimModTab"))
}


##' Check if an object is 'NMsimModTab'
##' @param x Any object
##' @return logical if x is an 'NMsimModTab' object
##' @export
##' @rdname NMsimModTabOperations
is.NMsimModTab <- function(x){
    inherits(x,"NMsimModTab")
}


##' Basic arithmetic on NMsimModTab objects
##'
##' @param x an NMsimModTab object
##' @param ... arguments passed to other methods.
##' @details When 'dimnames', 'merge', 'cbind', 'rbind', or 't' is
##'     called on an 'NMsimModTab' object, the 'NMsimModTab' class is dropped,
##'     and then the operation is performed. So if and 'NMsimModTab' object
##'     inherits from 'data.frame' and no other classes (which is
##'     default), these operations will be performed using the
##'     'data.frame' methods. But for example, if you use 'as.fun' to
##'     get a 'data.table' or 'tbl', their respective methods are used
##'     instead.
##' @return An object that is not of class 'NMsimModTab'.
##' @name NMsimModTabOperations
NULL

##' @rdname NMsimModTabOperations
##' @export
merge.NMsimModTab <- function(x,...){
    unNMsimModTab(x)
    merge(x,...)
}

##' @rdname NMsimModTabOperations
##' @export
t.NMsimModTab <- function(x,...){
    unNMsimModTab(x)
    t(x,...)
}

##' @rdname NMsimModTabOperations
##' @export
dimnames.NMsimModTab <- function(x,...){
    unNMsimModTab(x)
    dimnames(x,...)
}

##' @rdname NMsimModTabOperations
##' @export
rbind.NMsimModTab <- function(x,...){
    
    unNMsimModTab(x)
    rbind(x,...)
}

##' @rdname NMsimModTabOperations
##' @export
cbind.NMsimModTab <- function(x,...){
    unNMsimModTab(x)
    cbind(x,...)
}

