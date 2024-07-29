## on win
check_executable <- function(cmd) {

    ## append .ext if missing - this will not work if cmd="my.script"
    cmd <- fnExtension(cmd,".exe")
    
    ## Check if the cmd is an absolute file path
    if (file.exists(cmd)) {
        return(TRUE)
    }
    
    ## Get the PATH environment variable
    paths <- strsplit(Sys.getenv("PATH"), ";")[[1]]
    
    ## Check if the cmd is in any of the directories listed in PATH
    for (path in paths) {
        full_path <- file.path(path, cmd)
        if (file.exists(full_path)) {
            return(TRUE)
        }
    }
    
    return(FALSE)
}
