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
##' @param name.sim Give all filenames related to the simulation a
##'     suffix. A short string describing the sim is recommended like
##'     "ph3_regimens".
##' @param order.columns reorder columns by calling
##'     NMdata::NMorderColumns before saving dataset and running
##'     simulations? Default is TRUE.
##' @param script The path to the script where this is run. For
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
##'     transform, the contents must be the function to apply.
##' @param seed Seed to pass to Nonmem. Default is to draw one like
##'     `sample(1:1e8,size=1)` for each simulation. In case
##'     type.sim=known, seed is not used and will be set to 1.
##' @param args.psn.execute A charachter string that will be passed as
##'     arguments PSN's `execute`.
##' @param text.table A character string including the variables to
##'     export from Nonmem. The default is to export the same tables
##'     as listed in the input control stream. But if many variables
##'     are exported, and much fewer are used, it can speed up NMsim
##'     significantly to only export what is needed (sometimes this is
##'     as little as "PRED IPRED"). Nonmem writes data slowly so
##'     reducing output data from say 100 columns to a handful makes a
##'     big difference.
##' @param type.mod The control stream "type". Default is "est"
##'     meaning that an $ESTIMATION block will be replaced by a
##'     "$SIMULATION" block, and parameter estimates should be taken
##'     from the estimation results. If the control stream has already
##'     been turned into a simulation control stream, and only $INPUT,
##'     $DATA, and $TABLE sections should be edited. This implies that
##'     in case type.mod="sim", `subproblems` is ignored. `type.mod`
##'     may be automated in the future.
##' @param type.sim One of "default" (new simulation), "typical"
##'     (typical subject, ETAs = 0), or "known" (ID's observed in
##'     model estimation).
##' @param execute Execute the simulation or only prepare it?
##'     `execute=FALSE` can be useful if you want to do additional
##'     tweaks or simulate using other parameter estimates.
##' @param nmquiet Silent messages from Nonmem.
##' @param sge Submit to cluster? Default is not to, but this is very
##'     useful if creating a large number of simulations,
##'     e.g. simulate with all parameter estimates from a bootstrap
##'     result.
##' @param method.execute Specify how to call Nonmem. Options are
##'     "psn" (PSN's execute), "directory" (an internal method similar
##'     to PSN's execute), and "direct" (just run Nonmem directly and
##'     dump all the temporary files). "directory" has advantages over
##'     "psn" that makes it the only supported method when
##'     type.sim="known". "psn" has the simple advantage that the path
##'     to nonmem does not have to be specified if "execute" is in the
##'     system search path. So as long as you know where your Nonmem
##'     executable is, "directory" is recommended. The default is
##'     "directory" if path.nonmem is specified, and "psn" if not.
##' @param method.update.inits The initial estimates must be updated
##'     from the estimated model before running the simulation. NMsim
##'     supports two ways of doing this: "psn" which uses PSN's
##'     "update_inits", and "nmsim" which uses a simple internal
##'     method. The advantage of "psn" is it keeps comments in the
##'     control stream and is a method known to many. The advantages
##'     of "nmsim" are it does not require PSN, and that it is very
##'     robust. "nmsim" fixes the whole OMEGA and SIGMA matrices as
##'     single blocks making the $OMEGA and $SIGMA sections of the
##'     control streams less easy to read. On the other hand, this
##'     method is robust because it avoids any interpretation of BLOCK
##'     structure or other code in the control streams.
##' @param dir.psn The directory in which to find PSN's executables
##'     ('execute' and 'update_inits'). The default is to rely on the
##'     system's search path. So if you can run 'execute' and
##'     'update_inits' by just typing that in a terminal, you don't
##'     need to specify this unless you want to explicitly use a
##'     specific installation of PSN on your system.
##' @param path.nonmem The path to the Nonmem executable to use. The
##'     could be something like "/usr/local/NONMEM/run/nmfe75" (which
##'     is a made up example). No default is available. You should be
##'     able to figure this out through how you normally execute
##'     Nonmem, or ask a colleague.
##' @param create.dir If the directory specified in dir.sim does not
##'     exists, should it be created? Default is TRUE.
##' @param as.fun The default is to return data as a data.frame. Pass
##'     a function (say tibble::as_tibble) in as.fun to convert to
##'     something else. If data.tables are wanted, use
##'     as.fun="data.table". The default can be configured using
##'     NMdataConf.
##' @param suffix.sim Deprecated. Use name.sim instead.
##' @param type.input Deprecated. Use type.mod instead.
##' @import NMdata

##' @export



NMsim <- function(path.mod,data,dir.sim, name.sim,
                  order.columns=TRUE,script=NULL,subproblems,
                  reuse.results=FALSE,seed,args.psn.execute,
                  nmquiet=FALSE,text.table, type.mod,method.sim=NMsim_default,
                  execute=TRUE,sge=FALSE,transform=NULL ,type.input,
                  method.execute,method.update.inits,create.dir=TRUE,dir.psn,
                  path.nonmem=NULL,as.fun
                 ,suffix.sim
                  ){
    
#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    sim <- NULL
    est <- NULL
    fn <- NULL
    par.type <- NULL
    i <- NULL
    rowtmp <- NULL
    . <- NULL
    ID <- NULL
    n <- NULL
    is.data <- NULL
    text <- NULL
    textmod <- NULL
    default <- NULL
    known <- NULL
    typical <- NULL
    psn <- NULL
    direct <- NULL
    directory <- NULL
    nmsim <- NULL
    
### Section end: Dummy variables, only not to get NOTE's in pacakge checks

#### Section start: Checking aguments ####

    ## type.sim
    ## if(missing(type.sim)) type.sim <- NULL
    ## type.sim <- simpleCharArg("type.sim",type.sim,"default",cc(default,known,typical))

    ## dir.psn - should use NMdataConf setup
    if(missing(dir.psn)) dir.psn <- NULL
    dir.psn <- try(NMdata:::NMdataDecideOption("dir.psn",dir.psn),silent=TRUE)
    if(inherits(dir.psn,"try-error")){
        dir.psn <- NULL
        dir.psn <- simpleCharArg("dir.psn",dir.psn,"",accepted=NULL,lower=FALSE)
    }
    file.psn <- function(dir.psn,file.psn){
        if(dir.psn=="") return(file.psn)
        file.path(dir.psn,file.psn)
    }
    
    cmd.update.inits <- file.psn(dir.psn,"update_inits")

    ## path.nonmem - should use NMdataConf setup
    
    if(missing(path.nonmem)) path.nonmem <- NULL
    path.nonmem <- try(NMdata:::NMdataDecideOption("path.nonmem",path.nonmem),silent=TRUE)
    if(inherits(path.nonmem,"try-error")){
        path.nonmem <- NULL
        
        path.nonmem <- simpleCharArg("path.nonmem",path.nonmem,default=NULL,accepted=NULL,lower=FALSE)
    }
    
    ## method.execute
    if(missing(method.execute)) method.execute <- NULL
    ## if path.nonmem is provided, default method.execute is directory. If not, it is psn
    if(is.null(path.nonmem)) {
        method.execute.def <- "psn"
    } else {
        method.execute.def <- "directory"
    }
    method.execute <- simpleCharArg("method.execute",method.execute,method.execute.def,cc(psn,direct,directory))
    ## if(type.sim=="known"&&method.execute=="psn"){
    ##     stop("when type.sim==known, method.execute=psn is not supported.")
    ## }
    if(method.execute%in%cc(direct,directory) && is.null(path.nonmem)){
        stop("When method.execute is direct or directory, path.nonmem must be provided.")
    }

    ## args.psn.execute
    if(missing(args.psn.execute)) args.psn.execute <- NULL
    args.psn.execute <- simpleCharArg("args.psn.execute"
                                     ,args.psn.execute
                                     ,default="-clean=5 -model_dir_name -nm_output=xml,ext,cov,cor,coi,phi"
                                     ,accepted=NULL
                                     ,clean=FALSE
                                     ,lower=FALSE)
    
    ## method.update.inits
    if(missing(method.update.inits)) method.update.inits <- NULL
    ## if method.execute is psn, default is psn. If not, it is NMsim.
    if(is.null(method.update.inits) && method.execute=="psn") method.update.inits <- "psn"
    method.update.inits <- simpleCharArg("method.update.inits",method.update.inits,"nmsim",cc(psn,nmsim,none))
    
    ## type.mod
    ## if(!missing(type.input)){
    ##     if(!missing(type.mod)){
    ##         stop("type.mod and type.input supplied. Use type.mod and not the deprecated type.input. ")
    ##     }
    ##     message("type.input is deprecated. Use type.mod.")
    ##     type.mod <- type.input
    ## }

    
    ## if(missing(type.mod)) type.mod <- NULL
    ## type.mod <- simpleCharArg("type.mod",type.mod,"est",accepted=cc(est,sim))

    ## if(type.sim=="known"&&type.mod!="est"){
    ##     stop("Currently, type.sim=known can only be used with type.mod=est.")
    ## }
    ## seed - will handle again after handling length(path.mod)>1
    if(missing(seed)) seed <- NULL

    ## name.sim
    if(!missing(suffix.sim)){
        if(!missing(name.sim)){
            stop("name.sim and suffix.sim supplied. Use name.sim and not the deprecated suffix.sim. ")
        }
        message("suffix.sim is deprecated. Use name.sim.")
        name.sim <- suffix.sim
    }
    if(missing(name.sim)) name.sim <- NULL
    name.sim <- simpleCharArg("name.sim",name.sim,"noname",accepted=NULL,lower=FALSE)

    
    ## as.fun
    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdata:::NMdataDecideOption("as.fun",as.fun)

    input.archive <- inputArchiveDefault
### this should be redundant with modules
    files.needed <- NULL
    
###  Section end: Checking aguments

    
    warn.notransform <- function(transform){
        if(is.null(transform)) return(invisible(NULL))
        warning("`transform` (argument) ignored since NMsim is not reading the simulation results.")
    }


    
    if(length(path.mod)>1){
        allres.l <- lapply(path.mod,NMsim,data=data
                          ,dir.sim=dir.sim,
                           name.sim=name.sim,
                           order.columns=order.columns,script=script,
                           subproblems=subproblems,
                           reuse.results=reuse.results,seed=seed,
                           args.psn.execute=args.psn.execute
                          ,nmquiet=nmquiet,
                           text.table=text.table,
                           type.mod=type.mod,execute=execute,
                           sge=sge
                          ,transform=transform
                           ##,type.sim=type.sim
                          ,method.sim=method.sim
                          ,path.nonmem=path.nonmem
                          ,dir.psn=dir.psn
                           )
        return(rbindlist(allres.l))
    }



    
#### Section start: Defining additional paths based on arguments ####

    ## dir.sim
    
    if(missing(dir.sim)) dir.sim <- NULL
    dir.sim <- simpleCharArg("dir.sim",dir.sim,file.path(dirname(file.mod),"NMsim"),accepted=NULL,lower=FALSE)
    
    if(!dir.exists(dir.sim)){
        if(!create.dir){
            stop(paste("dir.sim does not point to an existing directory. dir.sim is\n",NMdata:::filePathSimple(dir.sim)))
        }
        dir.create(dir.sim)
    }

    
    
    ## seed
    ## if( type.sim=="known" && !is.null(seed) && seed!=1 ){
    ##     ### how to handle this with modules?
    ##     warning("seed is specified even though type.sim=known. seed will not be used.")
    ## }
    if(is.null(seed)){
        seed <- function()sample(1:1e8,size=1)
    } 
    ## if(type.sim=="known"){
    ##     seed <- 1
    ## }
    if(is.function(seed)) seed <- seed()

    
### redundant with modules?
    if(missing(subproblems)|| is.null(subproblems)) subproblems <- 0

    if(missing(text.table)) text.table <- NULL
    if(!file.exists(path.mod)) stop("path.mod must be a path to an existing file.")


    ## fn.sim is the file name of the simulation control stream created by NMsim
    ## fn.sim <- sub("^run","NMsim",basename(path.mod))
    fn.sim <- paste0("NMsim_",basename(path.mod))
    fn.sim <- fnAppend(fn.sim,name.sim)

    ## path.sim.0 is a temporary path to the sim control stream - it
    ## will be moved to path.sim once created.
    path.sim.0 <- file.path(dirname(path.mod),fn.sim)
    ## path.sim: full path to simulation control stream
    path.sim <- NMdata:::filePathSimple(file.path(dir.sim,fn.sim))

    ## path.sim.lst is full path to final output control stream to be read by NMscanData.
    path.sim.lst <- fnExtension(path.sim,".lst")
    ## where to store checksums 
    path.digests <- fnExtension(fnAppend(path.sim.lst,"digests"),"rds")
###  Section end: Defining additional paths based on arguments

    ## if(missing(obj.checksums)){

    ## run.fun <- needRun(path.sim.lst, path.digests, funs=list(path.mod=readLines))

    
    run.fun <- try(
        needRun(path.sim.lst, path.digests, funs=list(path.mod=readLines,reuse.results=function(x)NULL),which=-2)
       ,silent=TRUE)
    
    if(inherits(run.fun,"try-error")){
        run.fun <- list(needRun=TRUE
                       ,digest.new=paste(Sys.time(),"unsuccesful")
                        )
    }
    if(reuse.results && !run.fun$needRun){
        simres <- try(NMscanData(path.sim.lst,merge.by.row=FALSE,file.data=input.archive))
        if(!inherits(simres,"try-error")){
            message("Found results from identical previous run (and reuse.results is TRUE). Not re-running simulation.")
            if(!is.null(transform)){
                
                for(name in names(transform)){
                    simres[,(name):=transform[[name]](get(name))]
                }
            }
            return(simres)
        } else {
            message("Tried to reuse results but failed to find/read any. Going to do the simulation.")
        }
    }
    ##}

    data <- copy(as.data.table(data))

    ## if(!col.row%in%colnames(data)) data[,(col.row):=.I]

    if(order.columns) data <- NMorderColumns(data)
### save input data to be read by simulation control stream
    ## fn.data is the data file name, no path
    fn.data <- paste0("NMsimData_",fnExtension(fnAppend(basename(path.mod),name.sim),".csv"))
    path.data <- file.path(dir.sim,fn.data)

    nmtext <- NMwriteData(data,file=path.data,quiet=TRUE,args.NMgenText=list(dir.data="."),script=script)


    ## run.mod <- sub("\\.mod","",basename(path.mod))
    run.mod <- fnExtension(basename(path.mod),"")
    ## run.sim <- sub("\\.mod","",fn.sim)
    run.sim <- fnExtension(basename(fn.sim),"")

    if(file.exists(path.sim)) unlink(path.sim)

    


########## this should be used to generate the first version of file.sim
    if(method.update.inits=="none"){
        file.copy(path.mod,path.sim)
    }
    if(method.update.inits=="psn"){
        cmd.update <- sprintf("%s --output_model=%s --seed=%s %s",cmd.update.inits,fn.sim,seed,path.mod)
        system(cmd.update,wait=TRUE)

        file.rename(path.sim.0,path.sim)
    }
    if(method.update.inits=="nmsim"){
        NMupdateInitsFix(file.mod=path.mod,new.mod=path.sim)
    }
    ## checked that $OMEGA looks OK
    

#### run the specified method to modify control stream
    ## lines.mod <- readLines(con=path.mod)
    lines.sim <- readLines(con=path.sim)
    lines.sim <- method.sim$fun.mod(lines.sim,seed=seed)
    
    sections.mod <- NMreadSection(file=path.mod)
    names.sections <- names(sections.mod)

    
    ## if(type.sim != "known"){
    ##         line.sim <- sprintf("$SIMULATION ONLYSIM (%s)",seed)
    ##     }

    ##    if(type.mod=="est"){
    

    ## NMreadSection(path.sim,section="OMEGA")
### replace $ESTIMATION with $SIMULATION SIMONLY

### If no $SIM section is found, replace $EST section with one
    
    
    ## if(subproblems>0){
    ##     line.sim <- paste(line.sim,sprintf("SUBPROBLEMS=%s",subproblems))
    ## }
    
    ## sim.section.exists <- any(grepl("^(SIM|SIMULATION)$",names.sections))
    
    ## if(sim.section.exists){
    ##     NMwriteSection(files=path.sim,section="$ESTIMATION",newlines="",backup=FALSE,quiet=TRUE)
    ##     str.sim <- names.sections[min(grepl("^(SIM|SIMULATION)$",names.sections))]
    ##     NMwriteSection(files=path.sim,section=str.sim,newlines=line.sim,backup=FALSE,quiet=TRUE)
    ## } else if(type.sim!="known") {
    ##     ## inserting $SIM instead of $EST. But rather, it should be
    ##     ## inserted before table, and $EST should be removed.
    ##     NMwriteSection(files=path.sim,section="$ESTIMATION",newlines=line.sim,backup=FALSE,quiet=TRUE)
    ## }
    
    ## try(NMwriteSection(files=path.sim,section="$COVARIANCE",newlines="",backup=FALSE,quiet=TRUE))
    ## } else if(type.mod=="sim"){
    ##     file.copy(path.mod,path.sim,overwrite=TRUE)
    ##     str.sim <- names.sections[min(grep("^(SIM|SIMULATION)$",names.sections))]
    ##     NMwriteSection(files=path.sim,section=str.sim,newlines=line.sim,backup=FALSE,quiet=TRUE)
    ## }
    ## if(type.sim=="typical"){

    ##         extres <- NMreadExt(fnExtension(path.mod,"ext"))
    ##         Netas <- extres$pars[par.type=="OMEGA",max(i)]

    ## ### creates a full block of zeros. Works but unnecessarily large.
    ##         ## lines.omega <- sprintf("$OMEGA BLOCK(%d)\n 0 FIX %s",Nomegas,paste(rep(0,(Nomegas**2-Nomegas)/2+Nomegas-1),collapse=" "))
    ##         lines.omega <- paste(c("$OMEGA",rep("0 FIX",Netas,"")),collapse="\n")
    ##         NMwriteSection(files=path.sim,section="omega",newlines=lines.omega,backup=FALSE,quiet=TRUE)
    ##     }


    
### replace data file
    lines.sim <- NMwriteSectionOne(lines=lines.sim,list.sections = nmtext,backup=FALSE,quiet=TRUE)

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

    lines.sim <- NMwriteSectionOne(lines=lines.sim,newlines=lines.tables,section="TABLE",backup=FALSE,location="last",quiet=TRUE)


### save file.sim
    con.newfile <- file(path.sim,"wb")
    writeLines(lines.sim,con=con.newfile)
    close(con.newfile)
    

    
    if(execute){
        
        ## run sim
        wait <- !sge
        
        NMexec(files=path.sim,sge=sge,nc=1,wait=wait,args.psn.execute=args.psn.execute,nmquiet=nmquiet,method.execute=method.execute,path.nonmem=path.nonmem,files.needed=files.needed,input.archive=input.archive)
        
        if(wait){
            simres <- NMscanData(path.sim.lst,merge.by.row=FALSE,as.fun="data.table",file.data=input.archive)
            ## optionally transform results like DV, IPRED, PRED
            if(!is.null(transform)){
                
                for(name in names(transform)){
                    simres[,(name):=transform[[name]](get(name))]
                }
            }
            ## warn.notransform(transform)
            simres <- as.fun(simres)
        } else {
            warn.notransform(transform)
            simres <- NULL
        }
    } else {
        simres <- NULL
    }

    saveRDS(run.fun$digest.new,file=path.digests)

    simres

}

