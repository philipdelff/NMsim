##sprintf("call %s %s %s",path.nonmem,fn.mod,fnExtension(fn.mod,".lst"))
NMrunWin <- function(fn.mod,dir.mod.abs,exts.cp,meta.tables,path.nonmem){

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

##     path.nonmem <- FALSE
    name <- FALSE

### Section end: Dummy variables, only not to get NOTE's in pacakge checks

    cp.if.pres <- function(fn,dest){

        ## foldername <- "foldername"
        ## filename <- "filename"
        ## location <- "location"
        ## location2 <- "location2"

        sprintf("IF EXIST \"%s\" COPY /Y \"%s\" \"%s\"",fn,fn,dest)
    }

    lines.bat <- c(
        sprintf("call %s %s %s",path.nonmem,fn.mod,fnExtension(fn.mod,".lst"))
       ,
        paste(unlist(lapply(fnExtension(fn.mod,exts.cp),cp.if.pres,dest=dir.mod.abs)),collapse="\n")
       ,
        paste(unlist(lapply(meta.tables[,name],cp.if.pres,dest=dir.mod.abs)),collapse="\n")
    )

    lines.bat
}


