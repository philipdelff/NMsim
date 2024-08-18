##' Generate system command to call Nonmem directly
##' @keywords internal

callNonmemDirect <- function(file.mod,path.nonmem){
    bfile.mod <- basename(file.mod)
    sprintf("cd %s; %s %s %s; cd -",dirname(file.mod),path.nonmem,bfile.mod,fnExtension(bfile.mod,".lst"))
}

##' Execute Nonmem inside a dedicated directory
##'
##' Like PSN's execute with less features. But easier to control from
##' NMexec. NMexecDirectory is not intended to be run by the user. Use
##' \code{NMexec} or \code{NMsim} instead.
##'
##' @param file.mod Path to a Nonmem input control stream.
##' @param path.nonmem Path to Nonmem executable. You may want to
##'     control this with \code{NMdata::NMdataConf}.
##' @param files.needed Files needed to run the control stream. This
##'     cold be a .phi file from which etas will be read. Notice,
##'     input data set will be handled automatically, you do not need
##'     to specify that.
##' @param dir.data If NULL, data will be copied into the temporary
##'     directory, and Nonmem will read it from there. If not NULL,
##'     dir.data must be the relative path from where Nonmem is run to
##'     where the input data file is stored. This would be ".." if the
##'     run directory is created in a directory where the data is
##'     stored.
##' @param clean The degree of cleaning (file removal) to do after
##'     Nonmem execution. If `method.execute=="psn"`, this is passed
##'     to PSN's `execute`. If `method.execute=="nmsim"` a similar
##'     behavior is applied, even though not as granular. NMsim's
##'     internal method only distinguishes between 0 (no cleaning),
##'     any integer 1-4 (default, quite a bit of cleaning) and 5
##'     (remove temporary dir completely).
##' @import NMdata
##' @importFrom R.utils getAbsolutePath
##' @return A bash shell script for execution of Nonmem
##' @keywords internal

## do not export. NMexec will call this.

NMexecDirectory <- function(file.mod,path.nonmem,files.needed,dir.data="..",system.type,clean){
    
    ## if(missing(method)||is.null(method)) method <- "directory"
    ## if(!(is.characther(method) && length(method)==1)||!method%in%cc(directory,direct)){
    ##     stop("method must be one of 'directory' and 'direct' - and only a single string.")
    ## }

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####
    
    name <- NULL

###  Section end: Dummy variables, only not to get NOTE's in pacakge checks
    copy.data <- FALSE
    if(is.null(dir.data)){
        copy.data <- TRUE
    }     
    if(missing(files.needed)) files.needed <- NULL
    extr.data <- NMextractDataFile(file.mod,file.mod=file.mod,file.data="extract")
    
    if(is.null(extr.data$path.csv) && !is.null(extr.data$path)){
        extr.data$path.csv <- extr.data$path
    }
    if(is.null(extr.data$exists.file.csv) && !is.null(extr.data$exists.file)){
        extr.data$exists.file.csv <- extr.data$exists.file
    }


### checks
    if(!file.exists(file.mod)) stop("file.mod must point to an existing file")
    if(!extr.data$exists.file.csv) stop("Input data file not found")

    dir.mod <- dirname(file.mod)
    fn.mod <- basename(file.mod)
    
### create temporary directory
    
    dirs.exist <- list.files(dir.mod,pattern=fnExtension(fnAppend(fn.mod,"dir[0-9]+"),""))
    no.dir.new <- max(c(0,as.numeric(sub(".+dir([0-9]+)","\\1",basename(dirs.exist)))))+1
    dir.tmp <- fnExtension(fnAppend(file.mod,paste0("dir",sprintf(fmt="%04d",no.dir.new))),"")
    dir.create(dir.tmp)
    file.mod.tmp <- file.path(dir.tmp,fn.mod)

    
    
### copy input data to temp dir. 
    if(copy.data){

        
        file.copy(extr.data$path.csv,dir.tmp)
### modify .mod to use local copy of input data. Notice the newfile
### arg to NMwriteSection creating file.mod.tmp.
        
        sec.data.new <- paste("$DATA",sub(extr.data$string,basename(extr.data$path.csv),extr.data$DATA,fixed=TRUE))
    } else {
### This works with NMsim but not with estimation.
        
        sec.data.new <- sub(extr.data$string,file.path(dir.data,basename(extr.data$path.csv)),extr.data$DATA,fixed=TRUE)
### this is a different interpretation of dir.data - ie. the relative change of path. It does not work.
        ## sec.data.new <- sub(extr.data$string,file.path(dir.data,extr.data$path.csv),extr.data$DATA,fixed=TRUE)
        
        if(length(sec.data.new)>1){
            sec.data.new <- c(paste("$DATA",sec.data.new[1]),sec.data.new[-1])
        } else {
            sec.data.new <- paste("$DATA",sec.data.new)
        }
    }
    NMwriteSection(files=file.mod,section="DATA",newlines=sec.data.new,newfile=file.mod.tmp,quiet=TRUE,backup = FALSE)

### copy .phi if found
    ## file.copy(fnExtension(file.mod,"phi"),dir.tmp)
    if(!is.null(files.needed)){
        file.copy(files.needed,dir.tmp)
    }

    
### identify tables that will be created
    meta.tables <- NMscanTables(file.mod.tmp,meta.only=TRUE,as.fun="data.table")

### execute nonmem
    exts.cp <- c("lst","xml","ext","cov","cor","coi","phi","msf","msfi","msfo","shk" )

    dir.mod.abs <- getAbsolutePath(dir.mod)
    if(system.type=="linux"){

        lines.script <- NMrunLin(fn.mod,dir.mod.abs,exts.cp,meta.tables,path.nonmem=path.nonmem,clean=clean)
        path.script <- file.path(dir.tmp,"run_nonmem.sh")
        writeTextFile(lines.script,path.script)
        Sys.chmod(path.script, mode = "0777", use_umask = FALSE)
    }
    
    if(system.type=="windows"){
        lines.script <- NMrunWin(fn.mod,dir.mod.abs,exts.cp,meta.tables,path.nonmem=path.nonmem,clean=clean)
        path.script <- file.path(dir.tmp,"run_nonmem.bat")
        writeTextFile(lines.script,path.script)
    }

    
    ## system(path.script)
    path.script

}
