##' Remove NMsimModels class and discard NMsimModels meta data
##' @param x An 'NMsimModels' object.
##' @return x stripped from the 'NMsimModels' class
##' @export
##' @rdname NMsimModelsOperations
unNMsimModels <- function(x){
    setattr(x,"NMsimModels",NULL)
    setattr(x,"class",setdiff(class(x),"NMsimModels"))
}


##' Check if an object is 'NMsimModels'
##' @param x Any object
##' @return logical if x is an 'NMsimModels' object
##' @export
##' @rdname NMsimModelsOperations
is.NMsimModels <- function(x){
    inherits(x,"NMsimModels")
}


##' Basic arithmetic on NMsimModels objects
##'
##' @param x an NMsimModels object
##' @param ... arguments passed to other methods.
##' @details When 'dimnames', 'merge', 'cbind', 'rbind', or 't' is
##'     called on an 'NMsimModels' object, the 'NMsimModels' class is dropped,
##'     and then the operation is performed. So if and 'NMsimModels' object
##'     inherits from 'data.frame' and no other classes (which is
##'     default), these operations will be performed using the
##'     'data.frame' methods. But for example, if you use 'as.fun' to
##'     get a 'data.table' or 'tbl', their respective methods are used
##'     instead.
##' @return An object that is not of class 'NMsimModels'.
##' @name NMsimModelsOperations
NULL

##' @rdname NMsimModelsOperations
##' @export
merge.NMsimModels <- function(x,...){
    unNMsimModels(x)
    merge(x,...)
}

##' @rdname NMsimModelsOperations
##' @export
t.NMsimModels <- function(x,...){
    unNMsimModels(x)
    t(x,...)
}

##' @rdname NMsimModelsOperations
##' @export
dimnames.NMsimModels <- function(x,...){
    unNMsimModels(x)
    dimnames(x,...)
}

##' @rdname NMsimModelsOperations
##' @export
rbind.NMsimModels <- function(x,...){
    
    unNMsimModels(x)
    rbind(x,...)
}

##' @rdname NMsimModelsOperations
##' @export
cbind.NMsimModels <- function(x,...){
    unNMsimModels(x)
    cbind(x,...)
}

