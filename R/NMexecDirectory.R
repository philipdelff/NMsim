##' Generate system command to call Nonmem directly
##' @keywords internal
callNonmemDirect <- function(file.mod,path.nonmem){
    bfile.mod <- basename(file.mod)
    sprintf("cd %s; %s %s %s; cd -",dirname(file.mod),path.nonmem,bfile.mod,fnExtension(bfile.mod,".lst"))
}

##' Execute Nonmem inside a dedicated directory
##' 
##' @param dir.data If NULL, data will be copied into the temporary
##'     directory, and Nonmem will read it from there. If not NULL,
##'     dir.data must be the relative path from where Nonmem is run to
##'     where the input data file is stored. This would be ".." if the
##'     run directory is created in a directory where the data is
##'     stored.
##' @import NMdata
##' @return A bash shell script for execution of Nonmem
##' @keywords internal

### like execute but in R.
## copy necessary files into temporary
## run nonmem
## copy resulting files back out

## maybe just use bbr?

## do not export. NMexec will call this.

NMexecDirectory <- function(file.mod,path.nonmem,files.needed,dir.data=".."){

    ## if(missing(method)||is.null(method)) method <- "directory"
    ## if(!(is.characther(method) && length(method)==1)||!method%in%cc(directory,direct)){
    ##     stop("method must be one of 'directory' and 'direct' - and only a single string.")
    ## }

    copy.data <- FALSE
    if(is.null(dir.data)){
        copy.data <- TRUE
    }     
    if(missing(files.needed)) files.needed <- NULL
    extr.data <- NMextractDataFile(file.mod)
    
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
        sec.data.new <- paste("$DATA",sub(extr.data$string,file.path(dir.data,basename(extr.data$path.csv)),extr.data$DATA,fixed=TRUE))
    }
    

### copy .phi if found
    ## file.copy(fnExtension(file.mod,"phi"),dir.tmp)
    if(!is.null(files.needed)){
        file.copy(files.needed,dir.tmp)
    }
    
### identify tables that will be created
    meta.tables <- NMscanTables(file.mod.tmp,meta.only=TRUE,as.fun="data.table")

### execute nonmem
    
    ## callNonmem(file.mod=file.mod.tmp,nonmem=nonmem)
    
    lines.bash <- c(
        "#!/bin/bash"
       ,"shopt -s extglob"
       ,"WD0=$PWD"
       ,
        sprintf("cd %s;" ,dir.tmp)
       ,sprintf("%s %s %s",path.nonmem,fn.mod,fnExtension(fn.mod,".lst"))
       ## ,"cd $WD0"
       ,sprintf("cp %s/*.+(lst|xml|ext|cov|cor|coi|phi|msf|msfi|msfo) %s",dir.tmp,dir.mod)
       ,sprintf("cp %s %s",paste(meta.tables[,file],collapse=" "),dir.mod)
       ,""
    )

    path.script <- file.path(dir.tmp,"run_nonmem.sh")
    con.newfile <- file(path.script,"wb")
    writeLines(lines.bash,con=con.newfile)
    close(con.newfile)
    Sys.chmod(path.script,mode="0577")
    
    ## system(path.script)
    path.script
#### the rest should be done in bash - have callNonmem take care of it
    
### copy wanted files back to orig location of file.mod 


### optionally remove temporary dir
}
