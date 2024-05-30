##' Create new Nonmem control stream with updated initial parameter values
##' @param file.mod The control stream to update. Will not be edited.
##' @param file.ext 
##' @param newfile New file to generate
##' @param fix Fix the values? Probably only TRUE is supported. Passed to \code{NMreplaceInits()}.
##' @return The resulting control stream path(s)
##' @import NMdata
##' @keywords internal

NMupdateInits <- function(file.mod,file.ext,newfile,fix){
    

    i <- NULL
    est <- NULL
    j <- NULL
    model <- NULL
    par.type <- NULL

    
    if(missing(file.ext)) file.ext <- NULL
### ext is supposed to be a table with parameter sets. But for now, defer to methods like NMsim_Varcov.
    ## if(missing(tab.ext)) tab.ext <- NULL
    ext <- NULL
    
    ## need file.mod
    if(!file.exists(file.mod)){stop("file.mod does not exist.")}

    ## for safety, require newfile
    if(missing(newfile)){stop("newfile must be provided.")}

    
    ## at max one of file.ext and tab.ext allowed
    if(! (is.null(file.ext) || is.null(ext) )){
        stop("Provide at maximum one of file.ext or tab.ext, not both.")
    }

    if(is.null(file.ext) && is.null(ext)){
        file.ext <- fnExtension(file.mod,"ext")
    }
    
    if(!is.null(file.ext)){
        dt.pars <- NMreadExt(file=file.ext,return="pars",as.fun="data.table",tableno="max",auto.ext=FALSE)
        setnames(dt.pars,"est","value")
    }    

    ###  handle tab.ext
    if(!is.null(ext)){
        if(!is.data.table(ext)){
            stop("ext must be a data.table")
        }
        dt.pars <- as.data.table(ext)
        if(! "model" %in% colnames(dt.pars)){
            dt.pars[,model:=.I]
        }
    }

### check whether there is more than one set of parameters. If so, we
### paste numbering to newfile
    dt.pars.split <- split(dt.pars,by="model")
    names.dat <- names(dt.pars.split)
### if length>1, create strings if not ""
    l.split <- length(dt.pars.split)
    suffix.names <- ""
    if(l.split>1){
        stop("NMupdateInits does not support updating multiple models")
        suffix.names <- padZeros(seq(1:l.split))
    }
    
    ##    newfiles <- paste(newfile,suffix.names,sep="_")
    if(is.null(newfile)) {
        newfiles <- NULL
    } else {
        newfiles <- sapply(suffix.names,fnAppend,fn=newfile)
    }
    silent <- lapply(1:length(dt.pars.split),function(ndat){
        xdat <- dt.pars.split[[ndat]]
        ### because we use newfile, this will be printed to newfile. If not, it would just return a list of control stream lines.
        res <- NMreplaceInits(files=file.mod,newfile=newfiles[[ndat]],inits=xdat,fix=fix,quiet=TRUE)
    })

    silent
}
