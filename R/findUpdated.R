##' Filter control streams to only those updated since last run
##' @param lstsd list of (input or output) control streams to consider
##' @keywords internal


findUpdated <- function(mods){
    times.res <- sapply(mods,function(mod){
        file.lst <- fnExtension(mod,".lst")
        try(checkTimes(file.lst,use.input=TRUE,file.mod=mod)$time.ok)
    })
    
    times.res
    
    mods.updated <- mods[times.res!="All OK"]
    mods.updated
}
