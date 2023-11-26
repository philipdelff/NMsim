##' Test if file modification times indicate that Nonmem models should
##' be re-run
##' @param file.lst The output control stream.
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

checkTimes <- function(file.lst,use.input=TRUE,nminfo.input=NULL,file.mod,tz.lst=NULL){


#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    lstExtractTime <- NULL
    
### Section end: Dummy variables, only not to get NOTE's in pacakge checks


    file <- file.lst

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
            
            dt.input <- NMscanInput(file.lst,file.mod=file.mod,quiet=TRUE)
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
        time.method.lst <- "mtime"
    }
    
    time.ok <- c()
    if(!is.null(file.mod) &&
       file.exists(file.mod) ##&&
       ## filePathSimple(file.mod)!=filePathSimple(file)
       ) {
        mtime.mod <- file.info.mod$mtime
        
        if(mtime.mod>testtime.lst){
            ## messageWrap(paste0("input control stream (",file.mod,") is newer than output control stream (",file,"). Seems like model has been edited since last run. If data sections have been edited, this can corrupt results."),
            ## fun.msg=warning)
            time.ok <- c(time.ok,"mod > lst")
        }
        ## if(mtime.mod>min(meta.output[,file.mtime])){
        ##     messageWrap(paste0("input control stream (",file.mod,") is newer than output tables. Seems like model has been edited since last run. If data sections have been edited, this can corrupt results."),
        ##                 fun.msg=warning)
        ##     time.ok <- c(time.ok,"mod > output")
        ## }
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
    
    time.ok <-
        if(length(time.ok)>0) {
            paste("Warning(s):", paste(time.ok,collapse=", "))
        } else {
            "All OK"
        }
    
    

    list(mtime.mod=mtime.mod,mtime.lst=mtime.lst,mtime.inp=mtime.inp,time.ok=time.ok)
}
