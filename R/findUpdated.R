##' @export
##' 
findUpdated <- function(lsts){
    times.res <- sapply(lsts,function(lst){
        file.mod <- fnExtension(lst,".mod")
        checkTimes(lst,use.input=TRUE,file.mod=file.mod)$time.ok
    })
    
    times.res
    
    lsts.updated <- lsts[times.res!="All OK"]
    lsts.updated
}
