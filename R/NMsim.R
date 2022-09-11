##' @param dir.data directory to store data in. Default is to save it
## in dir.sim. See nmdir.data too.

## import NMdata


### it would be useful not to have to do update_inits but just use
### copy. update_inits may go wrong.

### need to be able to just read results if sim was run already

## ** NMsim
## *** save input to datafile in simulations folder
## *** NMexec archives


### input data to be named NMsimData001.csv
### run to be called NMsim001.mod


NMsim <- function(path.mod,data,dir.sim,dir.data=dir.sim,nmdir.data=".",suffix.sim,order.columns=TRUE,script=NULL){
    
    if(order.columns) data <- NMorderColumns(data)
### save input data to be read by simulation control stream
    ## fn.data is the data file name, no path
    fn.data <- paste0("NMsimData_",fnExtension(fnAppend(basename(path.mod),suffix.sim),".csv"))
    path.data <- file.path(dir.data,fn.data)
##    nmtext <- NMwriteData(data,file=path.data,nmdir.data=nmdir.data,script=script)
    nmtext <- NMwriteData(data,file=path.data,nmdir.data=nmdir.data)
    
    ## path.mod <- paste0("models/Final_Auto/run",run,".mod")
    fn.sim <- sub("^run","NMsim",basename(path.mod))
    fn.sim <- fnAppend(fn.sim,suffix.sim)

    ## we should be able to move the simulation mod into another dir.
    path.sim.0 <- file.path(dirname(path.mod),fn.sim)
    path.sim <- file.path(dir.sim,fn.sim)

    run.mod <- sub("\\.mod","",basename(path.mod))
    run.sim <- sub("\\.mod","",fn.sim)

    cmd.update <- sprintf("update_inits --output_model=%s --seed=52256 %s",fn.sim,path.mod)
    if(file.exists(path.sim)) unlink(path.sim)
    system(cmd.update)

    file.rename(path.sim.0,path.sim)

    ## checked that $OMEGA looks OK
    NMreadSection(path.sim,section="OMEGA")
### replace $ESTIMATION with $SIMULATION SIMONLY
    NMwriteSection(files=path.sim,section="$ESTIMATION",newlines="$SIMULATION ONLYSIM (52256)")
    NMwriteSection(files=path.sim,section="$COVARIANCE",newlines="")

### replace data file
    NMwriteSection(files=path.sim,list.sections = nmtext)

    
    
    ## replace output table name
    lines <- readLines(path.sim)
    lines <- sub(paste0("FILE=",run.mod,".tab"),replacement=paste0("FILE=",run.sim,".tab"),lines)
    con.newfile <- file(path.sim,"wb")
    writeLines(lines,con=con.newfile)
    close(con.newfile)

    ## run sim
    NMexec(file.mod=path.sim,sge=FALSE,wait=TRUE)

    
    
    simres <- NMscanData(fnExtension(path.sim,".lst"))
    simres
}
