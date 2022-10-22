##' Run simulations from an estimated Nonmem model
##'
##' Supply a data set and an input control stream, and NMsim will
##' create neccesary files, run the simulation and read the results.
##'
##' @param path.mod Path to the input control stream to run the
##'     simulation on. The outpult control stream is for now assumed
##'     to be stored next to the input control stream and ending in
##'     .lst instead of .modl
##' @param data The simulation data as a data.frame.
##' @param dir.sim The directory in which NMsim will store all
##'     generated files.
##' @param suffix.sim Give all filenames related to the simulation a
##'     suffix. A short string describing the sim is recommended like
##'     "ph3_regimens".
##' @param order.columns reorder columns by calling
##'     NMdata::NMorderColumns before saving dataset and running
##'     simulations? Default is TRUE.
##' @param script The path to the script where this is run.For
##'     stamping of dataset so results can be traced back to code.
##' @param subproblems Number of subproblems to use as SUBPROBLEMS in
##'     $SIMULATION block in Nonmem. The default is subproblem=0 which
##'     means not to use SUBPROBLEMS.
##' @param reuse.results If simulation results found on file, should
##'     they be used? If TRUE and reading the results fail, the
##'     simulations will still be rerun.
##' @param seed Seed to pass to Nonmem

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


NMsim <- function(path.mod,data,dir.sim,
                  suffix.sim,order.columns=TRUE,script=NULL,subproblems,reuse.results=FALSE,seed){

    
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
    path.data <- file.path(dir.sim,fn.data)
    
    ##    nmtext <- NMwriteData(data,file=path.data,nmdir.data=nmdir.data,script=script)
    if(is.null(script)){
        nmtext <- NMwriteData(data,file=path.data,quiet=TRUE,args.NMgenText=list(dir.data="."))
    } else {
        nmtext <- NMwriteData(data,file=path.data,quiet=TRUE,args.NMgenText=list(dir.data="."),script=script)
    }
    
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
    lines.tables <- NMreadSection(path.sim,section="TABLE",asOne=FALSE)
    if(length(lines.tables)==0){
        stop("No TABLE statements found in control stream.")
    } else if(length(lines.tables)==1){
        lines.tables <- sub(paste0("FILE *= *[^ ]+"),replacement=paste0("FILE=",run.sim,".tab"),lines.tables)
    } else {
        lines.tables <- lapply(seq_along(lines.tables),function(n){
            sub(paste0("FILE *= *[^ ]+"),replacement=fnAppend(paste0("FILE=",run.sim,".tab"),n),lines.tables[[n]])

        }
        )}

    fun.paste <- function(...)paste(...,sep="\n")
    lines.tables <- do.call(fun.paste,lines.tables)
    NMwriteSection(newlines=lines.tables,section="TABLE",files=path.sim)


    ## run sim
    NMexec(files=path.sim,sge=FALSE,wait=TRUE,args.execute="-clean=5")
    
    
    ## simres <- NMscanData(path.sim.lst,col.row=col.row)
    simres <- NMscanData(path.sim.lst,merge.by.row=FALSE)
    simres
}
