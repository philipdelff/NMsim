##' Typical subject simiulation method
##' 
##' Like \code{NMsim_default} but with all ETAs=0, giving a
##' "typical subject" simulation. Do not confuse this with a
##' "reference subject" simulation which has to do with covariate
##' values. Technically all ETAs=0 is obtained by replacing
##' \code{$OMEGA} by a zero matrix.
##' 
##' @param file.sim See \code{?NMsim}.
##' @param file.mod See \code{?NMsim}.
##' @param data.sim See \code{?NMsim}.
##' @param return.text If TRUE, just the text will be returned, and
##'     resulting control stream is not written to file.
##' @import NMdata
##' @return Path to simulation control stream
##' @export


NMsim_typical <- function(file.sim,file.mod,data.sim,return.text=FALSE){

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    par.type <- NULL
    i <- NULL

### Section end: Dummy variables, only not to get NOTE's in pacakge checks


    files.needed.def <- NMsim_default(file.sim=file.sim,file.mod,data.sim)
    lines.sim <- readLines(file.sim)
    
    extres <- NMreadExt(fnExtension(file.mod,"ext"),return="pars",as.fun="data.table")
    Netas <- extres[par.type=="OMEGA",max(i)]

    lines.omega <- paste(c("$OMEGA",rep("0 FIX",Netas,"")),collapse="\n")
    lines.sim <- NMdata:::NMwriteSectionOne(lines=lines.sim,section="omega",newlines=lines.omega,backup=FALSE,quiet=TRUE)

    if(return.text){
        return(lines.sim)            
    }

    writeTextFile(lines=lines.sim,file=file.sim)

    return(file.sim)
}
