##' @keywords internal

typicalize <- function(file.sim,lines.sim,file.mod,return.text=FALSE,file.ext,Netas){

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    par.type <- NULL
    i <- NULL

### Section end: Dummy variables, only not to get NOTE's in pacakge checks
    

    ## files.needed.def <- NMsim_default(file.sim=file.sim,file.mod,data.sim)
    if(!is.null(file.sim)){
        lines.sim <- readLines(file.sim)
    }
    
    if(missing(Netas)) Netas <- NULL
    if(missing(file.ext)) file.ext <- NULL

    if(is.null(Netas)){
        
        if(is.null(file.ext)){
            file.ext <- fnExtension(file.mod,"ext")
        }
        if(!file.exists(file.ext)){
            stop("ext file not found and number of Etas not provided. See arguments file.ext and Netas.")
        } else {
            extres <- NMreadExt(file.ext,return="pars",as.fun="data.table")
            Netas <- extres[par.type=="OMEGA",max(i)]
        }
    }
    
    lines.omega <- paste(c("$OMEGA",rep("0 FIX",Netas),""),collapse="\n")
    lines.sim <- NMdata:::NMwriteSectionOne(lines=lines.sim,section="omega",newlines=lines.omega,backup=FALSE,quiet=TRUE)

    if(return.text){
        return(lines.sim)            
    }

    writeTextFile(lines=lines.sim,file=file.sim)

    return(file.sim)
}
