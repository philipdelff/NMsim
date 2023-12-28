NMreadSim <- function(path){
    ## when to look for combined and saved results?


    ## if path is a dir, search for rds
    
    ## if an rds, just read it
    if(!is.list(path) && is.character(path)) {
        tab.paths <- readRDS(path)
    }
    if(!inherits(tab.paths,"NMsimTab")) {
        stop("The provided rds file does not contain a NMsimRes object")
    }
    simres <- NMscanMultiple(tab.paths$path.sim.lst)

    ## if an lst, read it

    ## the rds table must keep NMscanData arguments


    return(simres)
}
