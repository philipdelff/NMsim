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
        if(packageVersion("NMdata")<"0.1.6.932"){
            message("NMdata version 0.1.7 or newer needed for typical=TRUE to handle OMEGAP sections. Not handling $OMEGAP.")
        } else {
            lines.omegap <- paste(c("$OMEGAP",rep("1E-30 FIX",Netas),""),collapse="\n")
            lines.sim <- NMdata:::NMwriteSectionOne(lines=lines.sim,section="omegap",newlines=lines.omegap,backup=FALSE,quiet=TRUE)
        }
    }

    if("OMEGAPD"%in%names(sections.sim)) {
        if(packageVersion("NMdata")<"0.1.6.932"){
            message("NMdata version 0.1.7 or newer needed for typical=TRUE to handle OMEGAPD sections. Not handling $OMEGAPD.")
        } else {
            lines.omegapd <- paste(c("$OMEGAPD",rep("1 FIX",Netas),""),collapse="\n")
            lines.sim <- NMdata:::NMwriteSectionOne(lines=lines.sim,section="omegapd",newlines=lines.omegapd,backup=FALSE,quiet=TRUE)
        }
    }
    
    if(return.text){
        return(lines.sim)            
    }

    writeTextFile(lines=lines.sim,file=file.sim)

    return(file.sim)
}
