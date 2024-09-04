## on win
searchExecutable <- function(cmd,dir.extra=NULL) {
    
    ## append .ext if missing - this will not work if cmd="my.script"
    ## if(.Platform$OS.type=="windows" && fnExtension(cmd)=="" ){
    ##     cmd <- fnExtension(cmd,".exe")
    ## }
    
    ## Check if the cmd is an absolute file path
    if (file.exists(cmd)) {
        return(TRUE)
    }

    ## Check if the cmd is available in dir.extra
    if(!is.null(dir.extra)){
        if (any(file.exists(file.path(dir.extra,cmd)))) {
            return(TRUE)
        }
    }
    
    path_sep <- ifelse(.Platform$OS.type=="windows",";",":")
    
    ## Get the PATH environment variable
    paths <- strsplit(Sys.getenv("PATH"), split=path_sep)[[1]]
    
    ## Check if the cmd is in any of the directories listed in PATH
    for (path in paths) {
        full_path <- file.path(path, cmd)
        if (file.exists(full_path)) {
            return(TRUE)
        }
    }
    
    return(FALSE)
}
