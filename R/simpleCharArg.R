
##' @keywords internal
simpleCharArg <- function(name.arg,val.arg,default,accepted,lower=TRUE) {
        
        ## set default if missing
        if(is.null(val.arg)) {
            val.arg <- default
        }
        ## check for compliant format
        if(length(val.arg)!=1||(!is.character(val.arg)&&!is.factor(val.arg))){
            stop(paste(name.arg,"must be a single character string."))
        }
        ## simplify
        val.arg <- gsub(" ","",as.character(val.arg))
        if(lower) val.arg <- tolower(val.arg)
        ## check against allowed
        if(!is.null(accepted) && !val.arg %in% accepted){
            stop(sprintf("%s must be one of %s.",name.arg,paste(types.sim.avail,collapse=", ")))
        }
        val.arg
    }
