##' @keywords internal

getSystemType <- function(sysname){

    if(missing(sysname) || is.null(sysname)){
        sysname <- Sys.info()[['sysname']]
    }
    system.type <- tolower(sysname)
    if(system.type=="darwin") system.type <- "linux"
    if(!system.type%in%c("linux","windows")){
        stop("system.type must be either linux or windows")
    }
    system.type
}
