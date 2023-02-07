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
##' @param transform A list defining transformations to be applied
##'     after the Nonmem simulations and before plotting. For each
##'     list element, its name refers to the name of the column to
##'     transform, the contents must be the function to apply. See
##'     examples.
##' @param seed Seed to pass to Nonmem
##' @import NMdata

##' @export




### it would be useful not to have to do update_inits but just use
### copy. update_inits may go wrong.

### need to be able to just read results if sim was run already

### read results if previously sent to cluster?

## ** NMsim
## *** save input to datafile in simulations folder
## *** NMexec archives


### input data to be named NMsimData001.csv
### run to be called NMsim001.mod


NMsim <- function(path.mod,data,dir.sim,
                  suffix.sim,order.columns=TRUE,script=NULL,subproblems,
                  reuse.results=FALSE,seed,args.execute="-clean=5",nmquiet=FALSE,text.table,
                  type.input="est",execute=TRUE,sge=FALSE,transform=NULL){


    warn.notransform <- function(transform){
        if(is.null(transform)) return(invisible(NULL))
        warning("`tranform` (argument) ignored since NMsim is not reading the simulation results.")
    }

    if(length(path.mod)>1){
        allres.l <- lapply(path.mod,NMsim,data=data
                          ,dir.sim=dir.sim,
                           suffix.sim=suffix.sim,
                           order.columns=order.columns,script=script,
                           subproblems=subproblems,
                           reuse.results=reuse.results,seed=seed,
                           args.execute=args.execute,nmquiet=nmquiet,
                           text.table=text.table,
                           type.input=type.input,execute=execute,
                           sge=sge)
        return(rbindlist(allres.l))
    }
    
#### Section start: Defining additional paths based on arguments ####

    if(missing(subproblems)|| is.null(subproblems)) subproblems <- 0
    if(missing(text.table)) text.table <- NULL
    if(!file.exists(path.mod)) stop("path.mod must be a path to an existing file.")
    if(is.function(seed)) seed <- seed()
    
    if(!type.input%in%cc(sim,est)){
        stop("type.input denotes whether supplied control stream is an estimation run or it is already a simulation run to be applied.")
    }

    
    ## fn.sim is the file name of the simulation control stream created by NMsim
    ## fn.sim <- sub("^run","NMsim",basename(path.mod))
    fn.sim <- paste0("NMsim_",basename(path.mod))
    fn.sim <- fnAppend(fn.sim,suffix.sim)

    ## path.sim.0 is a temporary path to the sim control stream - it
    ## will be moved to path.sim once created.
    path.sim.0 <- file.path(dirname(path.mod),fn.sim)
    ## path.sim: full path to simulation control stream
    path.sim <- file.path(dir.sim,fn.sim)

    ## path.sim.lst is full path to final output control stream to be read by NMscanData.
    path.sim.lst <- fnExtension(path.sim,".lst")

###  Section end: Defining additional paths based on arguments
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
    
    nmtext <- NMwriteData(data,file=path.data,quiet=TRUE,args.NMgenText=list(dir.data="."),script=script)
    
    
    run.mod <- sub("\\.mod","",basename(path.mod))
    run.sim <- sub("\\.mod","",fn.sim)

    if(file.exists(path.sim)) unlink(path.sim)
    
    sections.mod <- NMreadSection(file=path.mod)
    names.sections <- names(sections.mod)
    line.sim <- sprintf("$SIMULATION ONLYSIM (%s)",seed)
    
    if(type.input=="est"){
        cmd.update <- sprintf("update_inits --output_model=%s --seed=%s %s",fn.sim,seed,path.mod)
        system(cmd.update,wait=TRUE)

        file.rename(path.sim.0,path.sim)

        ## checked that $OMEGA looks OK
        ## NMreadSection(path.sim,section="OMEGA")
### replace $ESTIMATION with $SIMULATION SIMONLY

### If no $SIM section is found, replace $EST section with one
        
        
        if(subproblems>0){
            line.sim <- paste(line.sim,sprintf("SUBPROBLEMS=%s",subproblems))
        }
        
        sim.section.exists <- any(grepl("^(SIM|SIMULATION)$",names.sections))
        
        if(sim.section.exists){
            NMwriteSection(files=path.sim,section="$ESTIMATION",newlines="",backup=FALSE,quiet=TRUE)
            str.sim <- names.sections[min(grepl("^(SIM|SIMULATION)$",names.sections))]
            NMwriteSection(files=path.sim,section=str.sim,newlines=line.sim,backup=FALSE,quiet=TRUE)
        } else {
            ## inserting $SIM instead of $EST. But rather, it should be
            ## inserted before table, and $EST should be removed.
            NMwriteSection(files=path.sim,section="$ESTIMATION",newlines=line.sim,backup=FALSE,quiet=TRUE)
        }
        
        try(NMwriteSection(files=path.sim,section="$COVARIANCE",newlines="",backup=FALSE,quiet=TRUE))
    } else if(type.input=="sim"){
        file.copy(path.mod,path.sim,overwrite=TRUE)
        str.sim <- names.sections[min(grep("^(SIM|SIMULATION)$",names.sections))]
        NMwriteSection(files=path.sim,section=str.sim,newlines=line.sim,backup=FALSE,quiet=TRUE)
    }

    
### replace data file
    NMwriteSection(files=path.sim,list.sections = nmtext,backup=FALSE,quiet=TRUE)

    fn.tab.base <- paste0("FILE=",run.sim,".tab")
    if(is.null(text.table)){
        
        ## replace output table name
        lines.tables <- NMreadSection(path.sim,section="TABLE",asOne=FALSE,simplify=FALSE)
        if(length(lines.tables)==0){
            stop("No TABLE statements found in control stream.")
        } else if(length(lines.tables)==1){
            lines.tables <- list(sub(paste0("FILE *= *[^ ]+"),replacement=fn.tab.base,lines.tables[[1]]))
        } else {
            lines.tables <- lapply(seq_along(lines.tables),function(n){
                sub(paste0("FILE *= *[^ ]+"),replacement=fnAppend(fn,n),lines.tables[[n]])
            }
            )}
    } else {
        lines.tables <- list(paste("$TABLE",text.table,fn.tab.base))
    }

    fun.paste <- function(...) paste(...,sep="\n")
    lines.tables <- do.call(fun.paste,lines.tables)
    ## if no $TABLE found already, just put it last
    
    if(is.null(NMreadSection(file=path.sim,section="TABLE"))){
        ## this will work with NMdata 0.0.16
        
        NMwriteSection(newlines=lines.tables,section="TABLE",files=path.sim,backup=FALSE,location="last")
    } else {
        NMwriteSection(newlines=lines.tables,section="TABLE",files=path.sim,backup=FALSE)
    }
    
    if(execute){
        ## run sim
        wait <- !sge
        NMexec(files=path.sim,sge=sge,nc=1,wait=wait,args.execute=args.execute,nmquiet=nmquiet)
        
        if(wait){
            simres <- NMscanData(path.sim.lst,merge.by.row=FALSE)
            ## optionally transform results like DV, IPRED, PRED
            if(!is.null(transform)){
                for(name in names(transform)){
                    simres[,(name):=transform[[name]](get(name))]
                }
            }
            warn.notransform(transform)
            return(simres)
        } else {
            warn.notransform(transform)
            return(invisible(NULL))
        }
    } else {
        invisible(NULL)
    }
}

