##' @import NMdata

NMsim_typical <- function(path.sim,path.mod,data.sim,return.text=FALSE){

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    par.type <- NULL
    i <- NULL

### Section end: Dummy variables, only not to get NOTE's in pacakge checks



    files.needed.def <- NMsim_default(path.sim=path.sim,path.mod,data.sim)
    lines.sim <- readLines(path.sim)

    extres <- NMreadExt(fnExtension(path.sim,"ext"))
    Netas <- extres$pars[par.type=="OMEGA",max(i)]

    lines.omega <- paste(c("$OMEGA",rep("0 FIX",Netas,"")),collapse="\n")
    lines.sim <- NMdata:::NMwriteSectionOne(lines=lines.sim,section="omega",newlines=lines.omega,backup=FALSE,quiet=TRUE)

    if(return.text){
        return(lines.sim)            
    }

    writeTextFile(lines=lines.sim,file=path.sim)

    return(path.sim)
}