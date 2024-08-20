cleaningPatterns <- function(clean){
    if(! clean %in% 1:4){
        stop ("only clean values 1, 2, 3, and 4 are supported")
    }
    c("nonmem","worker*","FDATA*","fort.*","WK_*")
}

NMrunLin <- function(fn.mod,dir.mod.abs,exts.cp,meta.tables,path.nonmem,clean){

name <- NULL
    
    lines.bash <- c(
        "#!/bin/bash"
       ,sprintf("%s %s %s",path.nonmem,fn.mod,fnExtension(fn.mod,".lst"))
### copy output tables back
        ## ,sprintf("cp \'%s\' \'%s\'",paste(meta.tables[,name],collapse="' '"),dir.mod.abs)
        ,paste0("find . -type f -name ",paste0("\'",meta.tables[,name],"\'")," -exec cp {} \'",dir.mod.abs,"\' \\;")
### copy wanted files back to orig location of file.mod 
       ,paste0("find . -type f -name ",paste0("\'*.",exts.cp,"\'")," -exec cp {} \'",dir.mod.abs,"\' \\;")
       ,""
    )

    rm.if.pres <- function(regex){
        sprintf("find . -type f -name \"%s\" -exec rm {} \\;",regex)     
    }

    if(clean>0 && clean<5){

        patterns.clean <- cleaningPatterns(clean)
        lines.bash <- c(lines.bash
                       ,paste(unlist(lapply(patterns.clean,
                                            rm.if.pres)),collapse="\n")
                        )
        
    }
    if(clean==5){
        lines.bash <- c(lines.bash,
                        sprintf("cd ..; rm -r \"%s\"",dir.mod.abs)
                        )
    }
    
    lines.bash
}

##sprintf("call %s %s %s",path.nonmem,fn.mod,fnExtension(fn.mod,".lst"))
NMrunWin <- function(fn.mod,dir.mod.abs,exts.cp,meta.tables,path.nonmem,clean){

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    ##     path.nonmem <- FALSE
    name <- FALSE

### Section end: Dummy variables, only not to get NOTE's in pacakge checks

    cp.if.pres <- function(fn,dest){
        sprintf("IF EXIST \"%s\" COPY /Y \"%s\" \"%s\"",fn,fn,dest)
    }
    rm.if.pres <- function(fn){
        sprintf("IF EXIST \"%s\" DEL \"%s\"",fn,fn)
    }

    lines.bat <- c(
        sprintf("call %s %s %s",path.nonmem,fn.mod,fnExtension(fn.mod,".lst"))
       ,
        paste(unlist(lapply(fnExtension(fn.mod,exts.cp),cp.if.pres,dest=dir.mod.abs)),collapse="\n")
       ,
        paste(unlist(lapply(meta.tables[,name],cp.if.pres,dest=dir.mod.abs)),collapse="\n")
    )

### Cleaning
    if(clean>0 && clean<5){
        lines.bat <- c(lines.bat,
                       paste(unlist(lapply(cleaningPatterns(clean),
                                           rm.if.pres)),collapse="\n")
                       )
    }

    if(clean==5){
        lines.bat <- c(lines.bat,
                        sprintf("CD .. & rd /s /q \"%s\"",dir.mod.abs)
                        )
    }
    
    lines.bat
}

