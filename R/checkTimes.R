##' @internal

checkTimes <- function(file.lst,use.input,nminfo.input){

    file <- file.lst

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
        logtime.inp <- max(nminfo.input$tables$file.logtime)
        mtime.inp <- max(nminfo.input$tables$file.mtime)
    }

    if(check.time){
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
           file.exists(file.mod) &&
           filePathSimple(file.mod)!=filePathSimple(file)) {
            mtime.mod <- file.info.mod$mtime
            
            if(mtime.mod>testtime.lst){
                messageWrap(paste0("input control stream (",file.mod,") is newer than output control stream (",file,"). Seems like model has been edited since last run. If data sections have been edited, this can corrupt results."),
                            fun.msg=warning)
                time.ok <- c(time.ok,"mod > lst")
            }
            if(mtime.mod>min(meta.output[,file.mtime])){
                messageWrap(paste0("input control stream (",file.mod,") is newer than output tables. Seems like model has been edited since last run. If data sections have been edited, this can corrupt results."),
                            fun.msg=warning)
                time.ok <- c(time.ok,"mod > output")
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
                messageWrap(paste0("input data (",nminfo.input$tables$file,") is newer than output control stream (",file,") Seems like model has been edited since last run. This is likely to corrupt results. Please consider either not using input data or re-running model."),
                            fun.msg=warning)
                time.ok <- c(time.ok,"input > lst")
            }
            if(testtime.inp > min(meta.output[,file.mtime])){
                messageWrap(paste0("input data file (",nminfo.input$tables$file,") is newer than output tables. Seems like model has been edited since last run. This is likely to corrupt results. Please consider either not using input data or re-running model."),
                            fun.msg=warning)
                time.ok <- c(time.ok,"input > output")
            }
        }
        
        time.ok <-
            if(length(time.ok)>0) {
                paste("Warning(s):", paste(time.ok,collapse=", "))
            } else {
                "All OK"
            }
        
    }

    list(mtime.mod=mtime.mod,mtime.lst=mtime.lst,time.ok=time.ok,mtime.imp)
}
