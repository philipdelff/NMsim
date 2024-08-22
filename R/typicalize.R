##' @keywords internal

typicalize <- function(file.sim,lines.sim,file.mod,return.text=FALSE,file.ext,Netas){

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    par.type <- NULL
    i <- NULL

### Section end: Dummy variables, only not to get NOTE's in pacakge checks
    

    ## files.needed.def <- NMsim_default(file.sim=file.sim,file.mod,data.sim)
    if(!is.null(file.sim)){
        lines.sim <- readLines(file.sim)
        sections.sim <- NMreadSection(lines=lines.sim)
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

## OMEGAP and OMEGAPD for NWPRI
    if("OMEGAP"%in%names(sections.sim)) {
        lines.omegap <- paste(c("$OMEGAP",rep("1E-30 FIX",Netas),""),collapse="\n")
        lines.sim <- NMdata:::NMwriteSectionOne(lines=lines.sim,section="omegap",newlines=lines.omegap,backup=FALSE,quiet=TRUE)
    }

    if("OMEGAPD"%in%names(sections.sim)) {
        lines.omegapd <- paste(c("$OMEGAPD",rep("1 FIX",Netas),""),collapse="\n")
        lines.sim <- NMdata:::NMwriteSectionOne(lines=lines.sim,section="omegapd",newlines=lines.omegapd,backup=FALSE,quiet=TRUE)
    }
    
    if(return.text){
        return(lines.sim)            
    }

    writeTextFile(lines=lines.sim,file=file.sim)

    return(file.sim)
}
