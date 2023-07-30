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
##' @param dir.sims The directory in which NMsim will store all
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
##' @param method.sim A function that creates the simulation control
##'     stream and other necessary files for a simulation based on the
##'     estimation control stream, the data, etc. Four methods are
##'     included: NMsim_default, NMsim_typical, NMsim_known, and
##'     NMsim_VarCov. See examples and vignettes on how to use these.
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
##' @param create.dir If the directory specified in dir.sims does not
##'     exists, should it be created? Default is TRUE.
##' @param as.fun The default is to return data as a data.frame. Pass
##'     a function (say tibble::as_tibble) in as.fun to convert to
##'     something else. If data.tables are wanted, use
##'     as.fun="data.table". The default can be configured using
##'     NMdataConf.
##' @param suffix.sim Deprecated. Use name.sim instead.
##' @param type.input Deprecated. Use type.mod instead.
##' @param ... Additional arguments passed to \code{method.sim}.
##' @import NMdata

##' @export



NMsim <- function(path.mod,data,dir.sims, name.sim,
                  order.columns=TRUE,script=NULL,subproblems,
                  reuse.results=FALSE,seed,args.psn.execute,
                  nmquiet=FALSE,text.table, type.mod,method.sim=NMsim_default,
                  execute=TRUE,sge=FALSE,transform=NULL ,type.input,
                  method.execute,method.update.inits,create.dir=TRUE,dir.psn,
                  path.nonmem=NULL,as.fun
                 ,suffix.sim
                 ,...
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
    none <- NULL
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
    file.mod <- NULL
    ROWMODEL <- NULL
    fn.mod <- NULL
    fn.sim <- NULL
    run.mod <- NULL
    run.sim <- NULL
    dir.sim <- NULL
    fn.sim.tmp <- NULL
    path.sim <- NULL
    path.digests <- NULL
    path.sim.lst <- NULL
    fn.data <- NULL
    path.data <- NULL
    variable <- NULL
    ROW <- NULL
    files.needed <- NULL
    ROWMODEL2 <- NULL
    
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
    if(is.null(method.update.inits)) method.update.inits <- "psn"
    method.update.inits <- simpleCharArg("method.update.inits",method.update.inits,"nmsim",cc(psn,nmsim,none))
    
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
    
###  Section end: Checking aguments

    
    warn.notransform <- function(transform){
        if(is.null(transform)) return(invisible(NULL))
        warning("`transform` (argument) ignored since NMsim is not reading the simulation results.")
    }


    ## if(F){
    if(length(path.mod)>1){
        allres.l <- lapply(path.mod,NMsim
                          ,data=data
                          ,dir.sims=dir.sims,
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
                          ,...
                           )
        return(rbindlist(allres.l))
    }
    ## }


    
#### Section start: Defining additional paths based on arguments ####

    ## dir.sim
    

    if(missing(dir.sims)) dir.sims <- NULL
    dir.sims <- simpleCharArg("dir.sims",dir.sims,file.path(dirname(path.mod),"NMsim"),accepted=NULL,lower=FALSE)
    
    if(!dir.exists(dir.sims)){
        if(!create.dir){
            stop(paste("dir.sims does not point to an existing directory. dir.sims is\n",NMdata:::filePathSimple(dir.sims)))
        }
        dir.create(dir.sims)
    }


    
    
    ## seed
    if(is.null(seed)){
        seed <- function()sample(1:1e8,size=1)
    } 

    if(missing(subproblems)|| is.null(subproblems)) subproblems <- 0

    dt.models <- data.table(path.mod=path.mod)
    dt.models[,ROWMODEL:=.I]

    if(missing(text.table)) text.table <- NULL
    if(any(!file.exists(dt.models[,path.mod]))) {
        stop("path.mod must be a path to an existing file.")
    }


    ## fn.sim is the file name of the simulation control stream created by NMsim
    ## fn.sim <- sub("^run","NMsim",basename(path.mod))
    dt.models[,fn.mod:=basename(path.mod)]
    dt.models[,fn.sim:=paste0("NMsim_",fn.mod)]
    ## dt.models[,fn.sim:=paste0(fn.mod)]
    dt.models[,fn.sim:=fnAppend(fn.sim,name.sim)]


    ## run.mod <- sub("\\.mod","",basename(path.mod))
    dt.models[,run.mod:=fnExtension(basename(path.mod),"")]
    ## run.sim <- sub("\\.mod","",fn.sim)
    dt.models[,run.sim:=fnExtension(basename(fn.sim),"")]


    ## dir.sim is the model-individual directory in which the model will be run
    dt.models[,
              dir.sim:=file.path(dir.sims,paste(run.mod,name.sim,sep="_"))]

    ## path.sim.0 is a temporary path to the sim control stream - it
    ## will be moved to path.sim once created.
    dt.models[,fn.sim.tmp:=fnAppend(fn.sim,"tmp")]
    ## path.sim: full path to simulation control stream
    dt.models[,path.sim:=NMdata:::filePathSimple(file.path(dir.sim,fn.sim))]

    if(F){
        ## where to store checksums 
        dt.models[,path.digests:=fnExtension(fnAppend(path.sim.lst,"digests"),"rds")]
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
    }

    data <- copy(as.data.table(data))

    ## if(!col.row%in%colnames(data)) data[,(col.row):=.I]

    if(order.columns) data <- NMorderColumns(data)
### save input data to be read by simulation control stream
    ## fn.data is the data file name, no path
    dt.models[,fn.data:=paste0("NMsimData_",fnExtension(fnAppend(basename(path.mod),name.sim),".csv"))]
    dt.models[,path.data:=file.path(dir.sim,fn.data)]

    dt.models[,if(file.exists(path.sim)) unlink(path.sim),by=.(ROWMODEL)]

    dt.models[,{if(!dir.exists(dir.sim)){
                    dir.create(dir.sim)}
    },by=.(ROWMODEL)
    ]

    

########## this is used to generate the first version of file.sim. It would not need to, but beware PSN's update_inits needs to create a new file - don't try to overwrite an existing one.
    if(method.update.inits=="none"){
        dt.models[,file.copy(path.mod,path.sim),by=ROWMODEL]
    }

    if(method.update.inits=="psn"){
        dt.models[,{
            ## cmd.update <- sprintf("%s --output_model=%s --seed=%s %s",cmd.update.inits,fn.sim.tmp,seed,path.mod)
            cmd.update <- sprintf("%s --output_model=%s %s",cmd.update.inits,fn.sim.tmp,path.mod)
### would be better to write to another location than next to estimation model
            ## cmd.update <- sprintf("%s --output_model=%s %s",cmd.update.inits,file.path(".",fn.sim.tmp),path.mod)

            sys.res <- system(cmd.update,wait=TRUE)

            if(sys.res!=0){
                stop("update_inits failed. Please look into this. Is the output control stream available? Is it in a directory where you have write-access?")
            }            

            file.rename(file.path(dirname(path.mod),fn.sim.tmp),path.sim)
        },by=ROWMODEL]
    }
    if(method.update.inits=="nmsim"){
        
        dt.models[,NMupdateInits(file.mod=path.mod,newfile=path.sim,fix=TRUE),by=.(ROWMODEL)]
    }


### save data and replace $input and $data
#### multiple todo: save for each path.data
    dt.models[,{
        nmtext <- NMwriteData(data,file=path.data,quiet=TRUE,args.NMgenText=list(dir.data="."),script=script)
        NMdata:::NMwriteSectionOne(file0=path.sim,list.sections = nmtext,backup=FALSE,quiet=TRUE)
    }]


#### Section start: Output tables ####

    dt.models[,{
        
        fn.tab.base <- paste0("FILE=",run.sim,".tab")
        lines.sim <- readLines(path.sim)
        lines.tables <- NMreadSection(lines=lines.sim,section="TABLE",as.one=FALSE,simplify=FALSE)

        if(is.null(text.table)){
            ## replace output table name
            if(length(lines.tables)==0){
                stop("No TABLE statements found in control stream.")
            } else if(length(lines.tables)==1){
                lines.tables <- list(gsub(paste0("FILE *= *[^ ]+"),replacement=fn.tab.base,lines.tables[[1]]))
            } else {
                
                lines.tables <- lapply(seq_along(lines.tables),function(n){
                    fn.tab <- fnAppend(fn.tab.base,n)
                    gsub(paste0("FILE *= *[^ ]+"),replacement=fn.tab,lines.tables[[n]])
                })
            }
        } else {
            lines.tables <- list(paste("$TABLE",text.table,fn.tab.base))
        }

        fun.paste <- function(...) paste(...,sep="\n")
        lines.tables.new <- do.call(fun.paste,lines.tables)
        ## if no $TABLE found already, just put it last
        if(length(lines.tables)){
            location <- "replace"
        } else {
            location <- "last"
        }
        lines.sim <- NMdata:::NMwriteSectionOne(lines=lines.sim,newlines=lines.tables.new,section="TABLE",backup=FALSE,location=location,quiet=TRUE)

### save file.sim
        writeTextFile(lines=lines.sim,file=path.sim)
    },by=.(ROWMODEL)]


###  Section end: Output tables


    ## fun simulation method
    ## dt.models[,
    ##           files.needed:=paste(method.sim(path.sim=path.sim,path.mod=path.mod,data.sim=data),collapse=":")
    ##          ,by=.(ROWMODEL)]

    
    dt.models.gen <- dt.models[,
                               method.sim(path.sim=path.sim,path.mod=path.mod,data.sim=data,...)
                              ,by=.(ROWMODEL)]
    
    if(ncol(dt.models.gen)==2 && all(colnames(dt.models.gen)%in%c("ROWMODEL","V1"))){
        setnames(dt.models.gen,"V1","path.sim")
    }

    ## we need the new path.sim and files.needed
    cnames.gen <- colnames(dt.models.gen)
    if(!"path.sim"%in%cnames.gen) stop("path.sim must be in returned data.table")

    ## TODO: check that all path.sim have been generated

    ## TODO: if multiple models have been spawned, and files.needed has been generated, the only allowed method.execute is "directory"
    if(nrow(dt.models.gen)>1 && !is.null(dt.models.gen[,files.needed])){
        if(method.execute!="directory"){
            stop("Multiple simulation runs spawned, and they need additional files than the simulation input control streams. The only way this is supported is using method.execute=\"directory\".")
        }
    }
    
    ## if files.needed, psn execute cannot be used.
    if(!is.null(dt.models.gen[,files.needed])){
        if(method.execute=="psn"){
            stop("method.execute=\"psn\" cannot be used with simulation methods that need additional files to run. Try method.execute=\"directory\".")
        }
    }
    ## if multiple models spawned, direct is not allowed
    if(nrow(dt.models.gen)>1){
        if(method.execute=="direct"){
            stop("method.execute=\"direct\" cannot be used with simulation methods that spawn multiple simulation runs. Try method.execute=\"directory\" or method.execute=\"psn\".")
        }
    }
    
    
    setnames(dt.models,"path.sim","path.sim.main")
    cols.fneed <- cnames.gen[grepl("^files.needed",cnames.gen)]

    ## check that all path.sim and files.needed have been generated
    dt.files <- melt(dt.models.gen,measure.vars=c("path.sim",cols.fneed),value.name="file")
    dt.files[,missing:=!file.exists(file)]
    if(dt.files[,sum(missing)]){
        print(dt.files[,.("No. of files missing"=sum(missing)),by=.(column=variable)])
        stop("All needed files must be available after running simulation method.")
    }

    if(length(cols.fneed)){
        dt.models.gen[,ROW:=.I]
        ## by ROW, paste contents of columns named as described in cols.fneed
        ## dt.models.gen[,files.needed:=do.call(paste,as.list(c(get(cols.fneed),sep=":"))),by=.(ROW)]
        pastetmp <- function(...)paste(...,sep=":")
        dt.models.gen[,files.needed:=do.call(pastetmp,.SD),by=.(ROW),.SDcols=cols.fneed]

    }
    dt.models <- mergeCheck(
        dt.models.gen[,intersect(c("ROWMODEL","path.sim","files.needed"),cnames.gen),with=FALSE],
        dt.models,
        by="ROWMODEL"
       ,quiet=TRUE)

    ## path.sim.lst is full path to final output control stream to be
    ## read by NMscanData. This must be derived after method.sim may
    ## have spawned more runs.
    dt.models[,path.sim.lst:=fnExtension(path.sim,".lst")]
    
    dt.models[,ROWMODEL2:=.I]
    dt.models [,seed:={if(is.function(seed))  seed() else seed},by=.(ROWMODEL2)]
    
    
    
### seed and subproblems
    if(!is.null(seed) || subproblems>0){
        dt.models[,{
            
            lines.sim <- readLines(path.sim)
            all.sections.sim <- NMreadSection(lines=lines.sim)
            names.sections <- names(all.sections.sim)
            n.sim.sections <- sum(grepl("^(SIM|SIMULATION)$",names.sections))
            if(n.sim.sections == 0){
                warning("No simulation section found. Subproblems and seed will not be applied.")
            }
            if(n.sim.sections > 1 ){
                warning("More than one simulation section found. Subproblems and seed will not be applied.")
            }
            if(n.sim.sections == 1 ){
                name.sim <- names.sections[grepl("^(SIM|SIMULATION)$",names.sections)]
                section.sim <- all.sections.sim[[name.sim]]
                if(subproblems>0){
                    section.sim <- gsub("SUBPROBLEMS *= *[0-9]*"," ",section.sim)
                    section.sim <- paste(section.sim,sprintf("SUBPROBLEMS=%s",subproblems))
                }
                if(!is.null(seed)){
                    section.sim <- gsub("\\([0-9]+\\)","",section.sim)
                    section.sim <- paste(section.sim,sprintf("(%s)",seed))
                }
                lines.sim <- NMdata:::NMwriteSectionOne(lines=lines.sim,section="simulation",newlines=section.sim,quiet=TRUE)
                writeTextFile(lines.sim,path.sim)
            }
        },by=.(ROWMODEL2)]
    }


    
#### Section start: Execute ####
    
### files needed can vary, so NMexec must be run for one model at a time
    simres <- NULL
    if(execute){
        
        ## run sim
        wait <- !sge

        simres <- dt.models[,{
            simres.n <- NULL
            files.needed.n <- try(strsplit(files.needed,":")[[1]],silent=TRUE)
            if(inherits(files.needed.n,"try-error")) files.needed.n <- NULL
            ## if the output control stream exists, delete it. If not, we risk reading old results in case Nonmem fails
### this is an important assumption. Removing everyting in format run.extension. run_input.extension kept. Impo
            ## files.unwanted <- list.files(
            if(file.exists(path.sim.lst)){
                message("Existing output control stream found. Removing.")
                dt.outtabs <- NMscanTables(path.sim.lst,meta.only=TRUE,as.fun="data.table")
                if(nrow(dt.outtabs)){
                    file.remove(dt.outtabs[,file])    
                }
                unlink(path.sim.lst)

            }
            NMexec(files=path.sim,sge=sge,nc=1,wait=wait,args.psn.execute=args.psn.execute,nmquiet=nmquiet,method.execute=method.execute,path.nonmem=path.nonmem,files.needed=files.needed.n,input.archive=input.archive)
            
            if(wait){
                simres.n <- try(NMscanData(path.sim.lst,merge.by.row=FALSE,as.fun="data.table",file.data=input.archive))
                if(inherits(simres.n,"try-error")){
                    message("Results could not be read.")
                    simres.n <- NULL
                } else if(!is.null(transform)){
                    ## optionally transform results like DV, IPRED, PRED
                    for(name in names(transform)){
                        simres.n[,(name):=transform[[name]](get(name))]
                    }
                }
                ## warn.notransform(transform)
                simres.n <- as.fun(simres.n)
            } else {
                warn.notransform(transform)
                simres.n <- NULL
            }
            simres.n
        },by=.(ROWMODEL2)]
    }

###  Section end: Execute

    ## saveRDS(run.fun$digest.new,file=path.digests)

    simres

}
