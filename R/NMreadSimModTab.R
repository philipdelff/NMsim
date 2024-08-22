##' Read simulation results from rds objects and/or NMsimModTab objects
##' @inheritParams NMreadSim
##' @param progress Track progress? Default is `TRUE` if `quiet` is
##'     FALSE and more than one model is being simulated. The progress
##'     tracking is based on the number of models completed, not the
##'     status of the individual models.
##' @keywords internal

NMreadSimModTab <- function(x,check.time=FALSE,dir.sims,wait=FALSE,skip.missing=FALSE,quiet=FALSE,progress,as.fun){
    
    
    ROWTMP <- NULL
    path.lst.read <- NULL
    path.rds.read <- NULL
    pathSimsFromRes <- NULL
    path.sim.lst <- NULL
    . <- NULL

    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdata:::NMdataDecideOption("as.fun",as.fun)
    if(missing(progress)) progress <- NULL
    

    ## read all rds files to get everything into just one table.
    unwrapRDS <- function(x){
        path.rds.read <- NULL
        file.res.data <- NULL
        path.results.read <- NULL
        path.results <- NULL

        if(is.character(x)) {
            ##  an rds, read it, make sure its NMSimModels, check for fst,  and proceed with NMSimModels
            
            tab.paths.list <- lapply(x,function(file){
                tab.paths <- readRDS(file)
                if(!inherits(tab.paths,"NMsimModTab")) {
                    if(!is.data.frame(tab.paths)){
                        stop("The provided rds file does not contain a NMsimModTab object")
                    }
                    message("x is not a NMsimModTab object. This can be OK if it was generated using earlier versions of NMsim. However, you may need to provide `dir.sims` for this to work.")
                }
                
                tab.paths[,path.rds.read:=file]
#### path.results does not exist for all old versions. Move to ..One and make dependent on version?
                ## tab.paths[,path.results.read:=file.path(dirname(file),basename(path.results))]
            })

            tab.paths <- rbindlist(tab.paths.list,fill=TRUE)
            ## tab.paths[,file.res.data:=fnAppend(fnExtension(x,"fst"),"res")]

            
        } else if(is.NMsimModTab(x)){
            ## a NMSimModels already, go to procecssing that
            tab.paths <- x
        } else {
            stop("x is not recognized as an NMsimModTab object")
        }
        tab.paths
    }
    
    res.list <- lapply(x,unwrapRDS)
    modtab <- rbindlist(res.list,fill=TRUE)
    
    ## add in usable path to sim results
    modtab[,ROWTMP:=.I]
    modtab[,path.lst.read:={
        if(is.null(dir.sims)){
            dirSims <- file.path(dirname(path.rds.read),pathSimsFromRes)
        } else {
            dirSims <- dir.sims           
        }
        file.path(dirSims,relative_path(path.sim.lst,dirSims))
    },
    ## by=.(ROWMODEL2)
    by=.(ROWTMP)
    ]
    
    
### rather than reading one rds at a time, we should read all the
### rds's, stack them, and then read the object as
### one. NMreadSimModTabOne checks that there is only one rds but I'm
### sure that requirement is needed anymore. Could try to combine
### these and run at once. Would require more testing.
    res.list <- lapply(split(modtab,by="path.rds.read"),NMreadSimModTabOne,check.time=check.time,dir.sims=dir.sims,wait=wait,skip.missing=skip.missing,quiet=quiet,as.fun=as.fun,progress=progress)
    
    
    res <- rbindlist(res.list,fill=TRUE)

    list.ModTab <- lapply(res.list,function(y)attributes(y)$NMsimModTab)
    ModTab <- rbindlist(list.ModTab,fill=TRUE)

    addClass(res,"NMsimRes")
    setattr(res,"NMsimModTab",ModTab)

    res

}

##' Read simulation results from an rds or a NMsimModTab object
##' @inheritParams NMreadSim
##' @keywords internal
##' @import utils
NMreadSimModTabOne <- function(modtab,check.time=FALSE,dir.sims,wait=FALSE,quiet=FALSE,skip.missing=FALSE,progress,as.fun){
    
    . <- NULL
    ROWMODEL2 <- NULL
    args.NMscanData <- NULL
    file.res.data <- NULL
    funs.transform <- NULL
    NMsimVersion <- NULL
    path.rds.read <- NULL
    path.results <- NULL
    path.results.read <- NULL
    path.lst.read <- NULL
    ROWTMP <- NULL
    
    if(missing(progress)) progress <- NULL
    if(is.null(progress)) progress <- TRUE
    ## Previous versions did not save path.results, so 

    
    if(!"path.results"%in%colnames(modtab)){
        if(! "NMsimVersion"%in%colnames(modtab) || !"file.res.data" %in% colnames(modtab)){
            modtab[,path.results:=fnExtension(fnAppend(path.rds.read,"res"),"fst")]
        } else {
            modtab[NMsimVersion<="0.1.0.941",path.results:=file.res.data]
        }
    }
    
### Assuming that rds and fst are next to each other
    modtab[,path.results.read:=file.path(dirname(path.rds.read),basename(path.results))]
    
    rdstab <- unique(modtab[,.(path.results.read
                              ,path.rds.read)])
    if(nrow(rdstab)>1) stop("modtab must be related to only one rds file.")
    
####### Now we have a NMSimModels object to process.
    
### will need a function to apply transformations if applicable
    wrap.trans <- function(dt,...){
        funs <- list(...)
        for(name.fun in names(funs)){
            dt[,(name.fun):=funs[[name.fun]](get(name.fun))]
        }
        dt
    }

### read all sim results

####  must read each model into list elements. Then rbind(fill=T)
### this is to make sure results from different models with
### incompatible columns can be combined.
    
    
    
### if we have an fst, read it and return results
    if(check.time){
        from.fst <- rdstab[,!is.null(path.results.read) &&
                            file.exists(path.results.read) &&
                            file.mtime(path.results.read)>file.mtime(path.rds.read)
                           ]
    } else {
        from.fst <- rdstab[,!is.null(path.results.read) &&
                            file.exists(path.results.read)]
    }


    
    ## fsts
    if(from.fst){
### reads unique fsts
        res.list <- lapply(modtab[,unique(path.results.read)],read_fst,as.data.table=TRUE)
        res <- rbindlist(res.list,fill=TRUE)

        setattr(res,"NMsimModTab",modtab)
        addClass(res,"NMsimRes")
        return(res)
        
    }

    lsts.found <- modtab[,file.exists(path.lst.read)]
    done <- all(lsts.found)

    if(!done){
        if(wait){
            turns <- 0
            if(!done) message("* Waiting for Nonmem to finish")
            
            ## progress tracker
            n.lsts <- modtab[,.N]
            do.pb <- !quiet && progress && n.lsts>1
            if(do.pb){
                ## set up progress bar
                n.done <- sum(file.exists(modtab[,path.lst.read]))
                pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                                     max = n.lsts, # Maximum value of the progress bar
                                     initial = n.done,
                                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                                     ## width = 50,   # Progress bar width. Defaults to getOption("width")
                                     char = "=")
            }

            
            while(!done){
                Sys.sleep(5)
                n.done <- sum(file.exists(modtab[,path.lst.read]))
                done <- n.done==n.lsts
                turns <- turns+1

                if(do.pb){
                    setTxtProgressBar(pb, n.done)
                }
                
            }
            ## if(turns>0) message("Nonmem finished.")
            if(do.pb){
                close(pb)
                ## message("")
            }
        } else {
            if(skip.missing){
                message(sprintf("%d/%d model runs found",sum(lsts.found),length(lsts.found)))
            } else {
                lapply(modtab[lsts.found==FALSE,path.lst.read],function(x) message(sprintf("Not found: %s",x)))
                stop("Not all model runs completed. Look in messages for which ones. If you want to go ahead and read the ones that are found, use skip.missing=`TRUE`.")
            }
        }
    }


    ## if(!quiet) message("Reading Nonmem results")
    

    tab.split <- split(modtab,by="ROWMODEL2")
    nsplits <- length(tab.split)

    if(!quiet) message("* Collecting Nonmem results")
    do.pb <- do.pb <- !quiet && progress && nsplits>1
    if(do.pb){
        ## progress tracker
        pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                             max = nsplits, # Maximum value of the progress bar
                             ##initial = n.done,
                             style = 3,    # Progress bar style (also available style = 1 and style = 2)
                             ## width = 50,   # Progress bar width. Defaults to getOption("width")
                             char = "=")
    }

### this is needed for nc>1
    ## Sys.sleep(5)
    res.list <- lapply(1:nsplits,function(count){
        dat <- tab.split[[count]]
        res <- dat[,{
            ## the rds table must keep NMscanData arguments
            args.NM <- args.NMscanData[[1]]
            if( "file.mod" %in% names(args.NM)){
                
                stop("Do not use file.mod in args.NMscanData. NMsim created the simulation control streams so as a user you do not need to help NMsim find them.")
            }
            args.NM$file.mod <- function(file) fnExtension(file,".mod")
            if(! "quiet" %in% names(args.NM)){
                args.NM$quiet <- TRUE
            }
            
            ## put this in try and report better info if broken
            this.res <- try(do.call(NMscanData,
                                    c(list(file=path.lst.read),args.NM)
                                    ),silent=TRUE)
            if(inherits(this.res,"try-error")){
                if(!quiet) {
### todo: look for lines.lst and recursively for nmtran_error.txt
                    ## path.err.nmtran <- list.files()
                    lines.lst <- readLines(path.lst.read)
                    nlines <- length(lines.lst)
                    message(sprintf("Results could not be read from %s\nPasting the bottom of output control stream:\n----------------------------------------------\n%s\n----------------------------------------------",path.lst.read,paste(lines.lst[(nlines-25):nlines],collapse="\n")))
                }
                this.res <- NULL
            }

            if(!is.null(.SD$funs.transform)){
                this.funs <- .SD[1,funs.transform][[1]]
                this.res <- do.call(wrap.trans,c(list(dt=this.res),this.funs))
                this.res
            }


            this.res
        },by=.(ROWMODEL2)]

        if(do.pb){
            setTxtProgressBar(pb, count)
        }

        
        res
        
    })

    if(do.pb){
        close(pb)
    }
    
    res <- rbindlist(res.list,fill=TRUE)
    res[,ROWMODEL2:=NULL]


    res <- as.fun(res)
    setattr(res,"NMsimModTab",modtab)
    addClass(res,"NMsimRes")
    
    if(!is.null(rdstab$path.results.read)){
        NMwriteData(res,
                    file=rdstab$path.results.read,
                    formats.write="fst",
                    genText=F,
                    quiet=TRUE)
        ## this message may confuse because the user may think this has not happened if they don't see the message. And the message will only appear the first time data is being read.
        ## message("Results have been efficiently stored in results folder.")
    }


    res

}
