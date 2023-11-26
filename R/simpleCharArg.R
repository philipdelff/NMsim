
##' Check that a variable is a single character string meeting specified requirements
##'
##'
##' @param name.arg Name of the argument
##' @param val.arg argument value
##' @param default If val.arg is NULL, what should be returned?
##' @param accepted What values are allowed
##' @param lower run tolower?
##' @param clean clean white spaces?
##' @return The resulting parameter value
##' @details Better options may be available in packages like
##'     checkmate. This function doesn't only check the parameter
##'     value, it also sets it to the default value if missing.
##' @keywords internal
simpleCharArg <- function(name.arg,val.arg,default,accepted,lower=TRUE,clean=TRUE) {
    
    ## set default if missing
    if(is.null(val.arg)) {
        val.arg <- default
    }
    
    ## check for compliant format
    if( !(is.null(default) && is.null(val.arg))){
        if(length(val.arg)!=1 ||
           (!is.character(val.arg)&&!is.factor(val.arg))){
            stop(paste(name.arg,"must be a single character string."),call.=FALSE)
        }
    }
    ## clean
    if(clean) val.arg <- gsub(" ","",as.character(val.arg))
    if(lower) val.arg <- tolower(val.arg)
    ## check against allowed
    if(!is.null(accepted) && !val.arg %in% accepted){
        stop(sprintf("%s must be one of %s.",name.arg,paste(accepted,collapse=", ")),call.=FALSE)
    }
    val.arg
}
