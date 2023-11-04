##' @keywords internal

getSystemType <- function(system.type){

    if(missing(system.type) || is.null(system.type)){
        system.type <- Sys.info()['sysname']
    }
    system.type <- tolower(system.type)
    if(system.type=="darwin") system.type <- "linux"
    if(!system.type%in%c("linux","windows")){
        stop("system.type must be either linux or windows")
    }
    system.type
}
