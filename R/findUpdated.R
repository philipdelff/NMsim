##' Filter control streams to only those updated since last run
##' @param lstsd list of (input or output) control streams to consider
##' @keywords internal


findUpdated <- function(lsts){
    times.res <- sapply(lsts,function(lst){
        file.mod <- fnExtension(lst,".mod")
        try(checkTimes(lst,use.input=TRUE,file.mod=file.mod)$time.ok)
    })
    
    times.res
    
    lsts.updated <- lsts[times.res!="All OK"]
    lsts.updated
}
