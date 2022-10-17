##' @param dir.data directory to store data in. Default is to save it
##'     in dir.sim. See nmdir.data too.
##' @export

## import NMdata


### it would be useful not to have to do update_inits but just use
### copy. update_inits may go wrong.

### need to be able to just read results if sim was run already

## ** NMsim
## *** save input to datafile in simulations folder
## *** NMexec archives


### input data to be named NMsimData001.csv
### run to be called NMsim001.mod


NMsim <- function(path.mod,data,dir.sim,dir.data=dir.sim,nmdir.data=".",suffix.sim,order.columns=TRUE,script=NULL,reuse.results=FALSE,seed,subproblems){

    
#### Section start: Defining additional paths based on arguments ####

    if(missing(subproblems)|| is.null(subproblems)) subproblems <- 0

    ## path.mod <- paste0("models/Final_Auto/run",run,".mod")
    fn.sim <- sub("^run","NMsim",basename(path.mod))
    fn.sim <- fnAppend(fn.sim,suffix.sim)

    ## we should be able to move the simulation mod into another dir.
    path.sim.0 <- file.path(dirname(path.mod),fn.sim)
    path.sim <- file.path(dir.sim,fn.sim)

    path.sim.lst <- fnExtension(path.sim,".lst")

###  Section end: Defining additional paths based on arguments
    
    ## if(missing(col.row) || is.null(col.row) ) {
    ##     col.row <- tmpcol(data,base="ROW")
    ##     reuse.results <- FALSE
    ## }
    
    if(reuse.results && file.exists(path.sim.lst)){
        ## simres <- try(NMscanData(path.sim.lst,col.row=col.row))
        simres <- try(NMscanData(path.sim.lst,merge.by.row=FALSE))
        if(!inherits(simres,"try-error")){
            return(simres)
        } else {
            message("Tried to reuse results but failed to find/read any. Going to do the simulation.")
        }
    }

    data <- copy(as.data.table(data))
    
    ## if(!col.row%in%colnames(data)) data[,(col.row):=.I]
    
    if(order.columns) data <- NMorderColumns(data)
### save input data to be read by simulation control stream
    ## fn.data is the data file name, no path
    fn.data <- paste0("NMsimData_",fnExtension(fnAppend(basename(path.mod),suffix.sim),".csv"))
    path.data <- file.path(dir.data,fn.data)
    ##    nmtext <- NMwriteData(data,file=path.data,nmdir.data=nmdir.data,script=script)
    nmtext <- NMwriteData(data,file=path.data,nmdir.data=nmdir.data,quiet=TRUE)
    
    run.mod <- sub("\\.mod","",basename(path.mod))
    run.sim <- sub("\\.mod","",fn.sim)

    cmd.update <- sprintf("update_inits --output_model=%s --seed=%s %s",fn.sim,seed,path.mod)
    if(file.exists(path.sim)) unlink(path.sim)
    system(cmd.update)

    file.rename(path.sim.0,path.sim)

    ## checked that $OMEGA looks OK
    NMreadSection(path.sim,section="OMEGA")
### replace $ESTIMATION with $SIMULATION SIMONLY
    line.sim <- sprintf("$SIMULATION ONLYSIM (%s)",seed)
    if(subproblems>0){
        line.sim <- paste(line.sim,sprintf("SUBPROBLEMS=%s",subproblems))
    }
    NMwriteSection(files=path.sim,section="$ESTIMATION",newlines=line.sim,backup=FALSE,quiet=TRUE)
    NMwriteSection(files=path.sim,section="$COVARIANCE",newlines="",backup=FALSE,quiet=TRUE)

### replace data file
    NMwriteSection(files=path.sim,list.sections = nmtext,backup=FALSE,quiet=TRUE)

    
    
    ## replace output table name
    lines <- readLines(path.sim)
### This should be done on the TABLE sections only
    lines <- sub(paste0("FILE *= *[^ ]+"),replacement=paste0("FILE=",run.sim,".tab"),lines)
    con.newfile <- file(path.sim,"wb")
    writeLines(lines,con=con.newfile)
    close(con.newfile)

    ## run sim
    NMexec(files=path.sim,sge=FALSE,wait=TRUE,args.execute="-clean=5")
    
    
    ## simres <- NMscanData(path.sim.lst,col.row=col.row)
    simres <- NMscanData(path.sim.lst,merge.by.row=FALSE)
    simres
}
