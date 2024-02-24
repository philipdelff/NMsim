##' Read simulation results from rds objects and/or NMsimModTab objects
##' @inheritParams NMreadSim
##' @keywords internal

NMreadSimModTab <- function(x,check.time=FALSE,dir.sims,wait=FALSE,skip.missing=FALSE,quiet=FALSE,as.fun){


    ROWTMP <- NULL
    path.lst.read <- NULL
    path.rds.read <- NULL
    pathSimsFromRes <- NULL
    path.sim.lst <- NULL
    . <- NULL

    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdata:::NMdataDecideOption("as.fun",as.fun)

    

    ## read all rds files to get everything into just one table.
    unwrapRDS <- function(x){
        path.rds.read <- NULL
        file.res.data <- NULL
        
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
            })

            tab.paths <- rbindlist(tab.paths.list,fill=TRUE)
            tab.paths[,file.res.data:=fnAppend(fnExtension(x,"fst"),"res")]

            
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
    
    
    ## res <- NMreadSimModTabOne(modtab=modtab,check.time=check.time,dir.sims=dir.sims,wait=wait,quiet=quiet,as.fun=as.fun)
    res.list <- lapply(split(modtab,by="path.rds.read"),NMreadSimModTabOne,check.time=check.time,dir.sims=dir.sims,wait=wait,skip.missing=skip.missing,quiet=quiet,as.fun=as.fun)

    
    
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
NMreadSimModTabOne <- function(modtab,check.time=FALSE,dir.sims,wait=FALSE,quiet=FALSE,skip.missing=FALSE,as.fun){

    . <- NULL
    ROWMODEL2 <- NULL
    args.NMscanData <- NULL
    file.res.data <- NULL
    funs.transform <- NULL
    path.rds.read <- NULL
    path.lst.read <- NULL
    ROWTMP <- NULL

    rdstab <- unique(modtab[,.(file.res.data,path.rds.read)])
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
        from.fst <- rdstab[,!is.null(file.res.data) &&
                            file.exists(file.res.data) &&
                            file.mtime(file.res.data)>file.mtime(path.rds.read)
                           ]
    } else {
        from.fst <- rdstab[,!is.null(file.res.data) &&
                            file.exists(file.res.data)]
    }



    ## fsts
    if(from.fst){
### reads unique fsts
        res.list <- lapply(modtab[,unique(file.res.data)],read_fst,as.data.table=TRUE)
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
            if(!done) message("Waiting for Nonmem to finish simulating...")
            while(!done){
                Sys.sleep(5)
                done <- all(file.exists(modtab[,path.lst.read]))
                turns <- turns+1
            }
            if(turns>0) message("Nonmem finished.")
        } else {
            if(skip.missing){
                message(sprintf("%d/%d model runs found",sum(lsts.found),length(lsts.found)))
            } else {
                lapply(modtab[lsts.found==FALSE,path.lst.read],function(x) message(sprintf("Not found: %s",x)))
                stop("Not all model runs completed. Look in messages for which ones. If you want to go ahead and read the ones that are found, use skip.missing=`TRUE`.")
            }
        }
    }
    

        res.list <- lapply(split(modtab,by="ROWMODEL2"),function(dat){
        res <- dat[,{
            ## the rds table must keep NMscanData arguments
            args.NM <- args.NMscanData[[1]]
            if(! "quiet" %in% names(args.NM)){
                args.NM$quiet <- TRUE
            }
            
            ## put this in try and report better info if broken
            this.res <- try(do.call(NMscanData,
                                    c(list(file=path.lst.read),args.NM)
                                    ))
            if(inherits(this.res,"try-error")){
                if(!quiet) {
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
        res
        
    })
    res <- rbindlist(res.list,fill=TRUE)
    res[,ROWMODEL2:=NULL]


    res <- as.fun(res)
    setattr(res,"NMsimModTab",modtab)
    addClass(res,"NMsimRes")

    if(!is.null(rdstab$file.res.data)){
        NMwriteData(res,
                    file=rdstab$file.res.data,
                    formats.write="fst",
                    genText=F,
                    quiet=TRUE)
    }


    res
}
