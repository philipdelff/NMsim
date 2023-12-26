NMreadSim <- function(path){
    ## when to look for combined and saved results?


    ## if path is a dir, search for rds
    
    ## if an rds, just read it
    tab.paths <- readRDS(path)
    simres <- NMscanMultiple(tab.paths$path.sim.lst)

    ## if an lst, read it

    ## the rds table must keep NMscanData arguments


    return(simres)
}
