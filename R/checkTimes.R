##' Test if file modification times indicate that Nonmem models should
##' be re-run
##' @param file Path to Nonmem-created file. Typically an output
##'     control stream.
##' @param use.input Scan input data for updates too? Default is TRUE.
##' @param nminfo.input If you do want to take into account input data
##'     but avoid re-reading the information, you can pass the NMdata
##'     meta data object.
##' @param file.mod The input control stream
##' @param tz.lst If files are moved around on or between file
##'     systems, the file modification time may not be reflective of
##'     the Nonmem runtime. In that case, you can choose to extract
##'     the time stamp from the output control stream. The issue is
##'     that Nonmem does not write the time zone, so you have to pass
##'     that to checkTimes if this is wanted.
##' @import NMdata
##' @keywords internal

checkTimes <- function(file,use.input=TRUE,nminfo.input=NULL,file.mod,tz.lst=NULL,use.tmp=TRUE){


#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    lstExtractTime <- NULL
    
### Section end: Dummy variables, only not to get NOTE's in pacakge checks

    if(missing(file.mod)) file.mod <- NULL
    file.mod <- NMdata:::NMdataDecideOption("file.mod",file.mod)
    file.mod <- file.mod(file)

### it would be better to put all the variables in a data.table
### and keep track of all elements in `file`
    ## dt.models <- data.table(file.mod=file.mod)
    ## dt.models[,mtime.mod:=file.mtime(file.mod),by=file.mod]

    file.info.mod <- NULL
    if(file.exists(file.mod)) file.info.mod <- file.info(file.mod)

    if(is.null(tz.lst)){
        logtime.lst <- NA
    } else {
        logtime.lst <- lstExtractTime(file,tz.lst)
    }
    mtime.lst <- file.mtime(file)
    time.method.lst <- NA
    time.method.inp <- NA
    time.ok <- "Not checked"
    
    
    if(use.input){
        if(is.null(nminfo.input)){
            
            dt.input <- try(NMscanInput(file.mod,file.mod=file.mod,quiet=TRUE))
            ## if("try-error" %in% class(dt.input)){
            ##     browser()
            ## }
            
            nminfo.input <- NMinfo(dt.input)
        }
        logtime.inp <- max(nminfo.input$tables$file.logtime)
        mtime.inp <- max(nminfo.input$tables$file.mtime)
    }


    time.method.lst <- "log"
    testtime.lst <- logtime.lst
    ## time.method.lst <- "mtime"
    ## testtime.lst <- mtime.lst
    if(is.na(logtime.lst)){
        testtime.lst <- mtime.lst
        if(is.na(mtime.lst)){
            testtime.lst <- -Inf
        }
        time.method.lst <- "mtime"
    }
    
    time.ok <- c()
    if(!is.null(file.mod) &&
       file.exists(file.mod) ##&&
       ## filePathSimple(file.mod)!=filePathSimple(file)
       ) {
        mtime.mod <- file.info.mod$mtime
        
        if(mtime.mod>testtime.lst){
            time.ok <- c(time.ok,"mod > lst")
        }
    }
    
    if(use.input) {
        
        time.method.inp <- "log"
        testtime.inp <- logtime.inp
        if(is.na(logtime.inp)){
            testtime.inp <- mtime.inp
            time.method.inp <- "mtime"
        }


        
        if(testtime.inp > testtime.lst){
            ## messageWrap(paste0("input data (",nminfo.input$tables$file,") is newer than output control stream (",file,") Seems like model has been edited since last run. This is likely to corrupt results. Please consider either not using input data or re-running model."),
            ## fun.msg=warning)
            time.ok <- c(time.ok,"input > lst")
        }
        ## if(testtime.inp > min(meta.output[,file.mtime])){
        ##     messageWrap(paste0("input data file (",nminfo.input$tables$file,") is newer than output tables. Seems like model has been edited since last run. This is likely to corrupt results. Please consider either not using input data or re-running model."),
        ##                 fun.msg=warning)
        ##     time.ok <- c(time.ok,"input > output")
        ## }
    }

    running <- NA
    if(use.tmp){
        ## find all temp dirs from psn or NMsim and derive max mtime
        tempdirs <- list.files(dirname(file.mod),pattern=paste0(fnExtension(basename(file.mod),""),"[\\._]dir[0-9]*"),full.names=TRUE)
        running <- FALSE
        if(length(tempdirs)){
            ## mtime.tmpdirs <- max(file.mtime(tempdirs))
            ## if(mtime.tmpdirs>testtime.lst){
            ##     running <- TRUE
            ## }
            running <- sapply(tempdirs,function(d){
                lsts.tmp <- list.files(d,pattern=".*\\.lst",full.names=TRUE)
                any(sapply(lsts.tmp,function(f) !grepl("Stop Time\\:",readLines(f)) ))
            })
        }
    }
    
    time.ok <-
        if(length(time.ok)>0) {
            paste("Warning(s):", paste(time.ok,collapse=", "))
        } else {
            "All OK"
        }
    
    if(use.tmp){
        if(!is.na(running) && running==TRUE ){
            time.ok <- "All OK"
        }
    }

    list(mtime.mod=mtime.mod,mtime.lst=mtime.lst,mtime.inp=mtime.inp,time.ok=time.ok)
}
