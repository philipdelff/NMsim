##' @keywords internal
callNonmemDirect <- function(file.mod,path.nonmem){
    bfile.mod <- basename(file.mod)
    sprintf("cd %s; %s %s %s; cd -",dirname(file.mod),path.nonmem,bfile.mod,fnExtension(bfile.mod,".lst"))
}

##' @import NMdata 
##' @keywords internal 

### like execute but in R.
## copy necessary files into temporary
## run nonmem
## copy resulting files back out

## maybe just use bbr?

## do not export. NMexec will call this.

NMexecDirectory <- function(file.mod,path.nonmem,files.needed){

    ## if(missing(method)||is.null(method)) method <- "directory"
    ## if(!(is.characther(method) && length(method)==1)||!method%in%cc(directory,direct)){
    ##     stop("method must be one of 'directory' and 'direct' - and only a single string.")
    ## }
    
    if(missing(files.needed)) files.needed <- NULL
    extr.data <- NMextractDataFile(file.mod)
    


### checks
    if(!file.exists(file.mod)) stop("file.mod must point to an existing file")
    if(!extr.data$exists.file) stop("Input data file not found")

    dir.mod <- dirname(file.mod)
    fn.mod <- basename(file.mod)
    
### create temporary directory
    
    dirs.exist <- list.files(dir.mod,pattern=fnExtension(fnAppend(fn.mod,"dir[0-9]+"),""))
    no.dir.new <- max(c(0,as.numeric(sub(".+dir([0-9]+)","\\1",basename(dirs.exist)))))+1
    dir.tmp <- fnExtension(fnAppend(file.mod,paste0("dir",sprintf(fmt="%04d",no.dir.new))),"")
    dir.create(dir.tmp)
    file.mod.tmp <- file.path(dir.tmp,fn.mod)
    
### copy input data to temp dir. 
    file.copy(extr.data$path,dir.tmp)
    
### modify .mod to use local copy of input data. Notice the newfile
### arg to NMwriteSection creating file.mod.tmp.
    sec.data.new <- paste("$DATA",sub(extr.data$string,basename(extr.data$path),extr.data$DATA,fixed=TRUE))
    NMwriteSection(files=file.mod,section="DATA",newlines=sec.data.new,newfile=file.mod.tmp,quiet=TRUE)

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
       ,"cd $WD0"
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
