##' Typical subject simiulation method
##' 
##' Like \code{NMsim_default} but with all ETAs=0, giving a
##' "typical subject" simulation. Do not confuse this with a
##' "reference subject" simulation which has to do with covariate
##' values. Technically all ETAs=0 is obtained by replacing
##' \code{$OMEGA} by a zero matrix.
##' 
##' @param path.sim See \code{?NMsim}.
##' @param path.mod See \code{?NMsim}.
##' @param data.sim See \code{?NMsim}.
##' @param return.text If TRUE, just the text will be returned, and
##'     resulting control stream is not written to file.
##' @import NMdata
##' @return Path to simulation control stream
##' @keywords internal


NMsim_typical <- function(path.sim,path.mod,data.sim,return.text=FALSE){

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    par.type <- NULL
    i <- NULL

### Section end: Dummy variables, only not to get NOTE's in pacakge checks


    files.needed.def <- NMsim_default(path.sim=path.sim,path.mod,data.sim)
    lines.sim <- readLines(path.sim)
    
    extres <- NMreadExt(fnExtension(path.mod,"ext"))
    Netas <- extres$pars[par.type=="OMEGA",max(i)]

    lines.omega <- paste(c("$OMEGA",rep("0 FIX",Netas,"")),collapse="\n")
    lines.sim <- NMdata:::NMwriteSectionOne(lines=lines.sim,section="omega",newlines=lines.omega,backup=FALSE,quiet=TRUE)

    if(return.text){
        return(lines.sim)            
    }

    writeTextFile(lines=lines.sim,file=path.sim)

    return(path.sim)
}
