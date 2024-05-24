##' pad zeros on integers
##' @param x integers to pad. They can be coded as characters already.
##' @param nchars Optional specification of length of character
##'     strings to return. If not supplied, characters will be padded
##'     to match length of max value in x.
##' @keywords internal
##' @return A character vector
##' 
padZeros <- function(x,nchars){

    if(missing(nchars)||is.null(nchars)){
        xmax <- max(as.numeric(as.character(x)))
        nchars <- ceiling(log10(xmax+1))
    }
    sprintf(fmt=paste0("%0",nchars,"d"),x)
    
}

