##' Simulate with parameter variability using the NONMEM NWPRI subroutine
##' 
##' @description Modify control stream for simulation with uncertainty
##'     using inverse-Wishart distribution for OMEGA and SIGMA
##'     parameters
##'
##' This function does not run any simulations. To simulate, using
##' this method, see `NMsim()`. See examples.
##'
##' 
##' @param file.sim The path to the control stream to be edited. This function overwrites the contents of the file pointed to by file.sim.
##' @param file.mod Path to the path to the original input control stream provided as `file.mod` to `NMsim()`.
##' @param data.sim Included for compatibility with `NMsim()`. Not used.
##' @param PLEV Used in \code{$PRIOR NWPRI PLEV=0.999}. This is a 
##'     NONMEM argument to the NWPRI subroutine. When PLEV < 1, a 
##'     value of THETA will actually be obtained using a truncated 
##'     multivariate normal distribution, i.e. from an ellipsoidal 
##'     region R1 over which  only  a fraction of mass of the 
##'     normal occurs. This fraction is given by PLEV.
##' @details Simulate with parameter uncertainty. THETA parameters are
##'     sampled from a multivariate normal distribution while OMEGA
##'     and SIGMA are simulated from the inverse-Wishart
##'     distribution. Correlations of OMEGA and SIGMA parameters will
##'     only be applied within modeled "blocks".
##' @references \href{https://ascpt.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002\%2Fpsp4.12422&file=psp412422-sup-0001-Supinfo1.pdf}{inverse-Wishart degrees of freedom calculation for OMEGA and SIGMA: NONMEM tutorial part II, supplement 1, part C.}

##' @seealso NMsim_VarCov
##' @import NMdata
##' @import data.table
##' @author Brian Reilly, Philip Delff
##' @examples
##' \dontrun{
##' simres <- NMsim(file.path,method.sim=NMsim_WPRI,typical=TRUE,subproblems=500)
##' }
##' @export

NMsim_NWPRI <- function(file.sim,file.mod,data.sim,PLEV=0.999){


    NMdata:::messageWrap("\nNMsim_NWPRI() currently only reliably simulates typical THETAs. Simulation with variability on OMEGA and SIGMA cannot be trust. Always run this method in NMsim with `typical=TRUE`",fun.msg=message)

    if(packageVersion("NMdata")<"0.1.6.932"){
        stop("NMsim_NWPRI requires NMdata 0.1.7 or later.")
    }
    

### done add OMEGA/SIGMA blocks

    
    . <- NULL
    DF <- NULL
    DF2 <- NULL
    FIX <- NULL
    N <- NULL
    blocksize <- NULL
    est <- NULL
    i <- NULL
    imin <- NULL
    iblock <- NULL
    j <- NULL
    line <- NULL
    par.name <- NULL
    par.type <- NULL
    par.type.i <- NULL
    par.type.j <- NULL
    parameter <- NULL
    parameter.i <- NULL
    parameter.j <- NULL
    return.text <- NULL
    se <- NULL
    value <- NULL


    
    
### NMsim_default() is run because it inserts $SIMULATION instead of
### $ESTIMATION and a few other things that are always needed.
    files.needed.def <- NMsim_default(file.sim=file.sim,file.mod,data.sim)
    lines.sim <- readLines(file.sim)

    cov <- NMreadCov(fnExtension(file.mod,".cov"))
    pars <- NMreadExt(file.mod,return="pars",as.fun="data.table")[,value:=est]


#### Section start: add OMEGA block information based on off diagonal values ####
### This section is almost copied from NMdata::NMreadExt. Only mergeCheck() call because common.cols=drop.x is introduced in 0.1.7. However, with current requirement
if(F){
    tab.blocks <- rbind(pars[par.type%in%c("OMEGA","SIGMA"),.(par.type,i=i,j=j,value)],
                        pars[par.type%in%c("OMEGA","SIGMA"),.(par.type,i=j,j=i,value)])[
        abs(value)>1e-9,.(iblock=min(i,j),blocksize=max(abs(j-i))+1),by=.(par.type,i)]

    ## pars0 <- copy(pars)
    ## tab.blocks
    pars <- mergeCheck(pars[,setdiff(colnames(pars),c("iblock","blocksize")),with=FALSE],
                       tab.blocks,by=cc(par.type,i),all.x=T,quiet=TRUE)

    ## pars[par.type%in%c("OMEGA","SIGMA"),.(i,j,iblock,blocksize,value)]

    pars[abs(i-j)>(blocksize-1),(c("iblock","blocksize")):=list(NA,NA)]
    pars[!is.na(iblock),imin:=min(i),by=.(iblock)]
    pars[j<imin,(c("iblock","blocksize")):=list(NA,NA)]
    pars[,imin:=NULL]

    ## pars[par.type%in%c("OMEGA","SIGMA"),.(i,j,iblock,blocksize,imin,value)]
    
    pars[par.type%in%c("OMEGA","SIGMA")&i==j&is.na(iblock),iblock:=i]
    pars[par.type%in%c("OMEGA","SIGMA")&i==j&iblock==i&is.na(blocksize),blocksize:=1]
}

###  Section end: add OMEGA block information based on off diagonal values
    
### Add degrees of freedom for inverse-wishart distribution for OMEGA/SIGMA
    pars[par.type%in%c("OMEGA","SIGMA")&i==j&!is.na(iblock), N := 2*((est**2)/(se**2)) + 1]
    pars[par.type%in%c("OMEGA","SIGMA")&i==j&!is.na(iblock), DF := N-blocksize-1]
    ## DF cannot be smaller than the number of parameters in the block
    pars[par.type%in%c("OMEGA","SIGMA")&i==j&!is.na(iblock), DF := ifelse(DF<blocksize, blocksize, DF)]
    # If parameter is fixed, set DF=block size to indicate we want a point estimate.
    pars[par.type%in%c("OMEGA","SIGMA")&i==j&!is.na(iblock), DF := ifelse(FIX==1, blocksize, DF)]
    ## take the minimum DF per omega/sigma matrix:
    pars[par.type%in%c("OMEGA","SIGMA")&i==j&!is.na(iblock), DF2 := min(DF, na.rm = TRUE), by = .(par.type,iblock)]
    
    nwpri_df = unique(pars[par.type%in%c("OMEGA","SIGMA")&i==j&!is.na(iblock),.(par.type,iblock, DF2)])
    nwpri_df[,line := paste0("$", par.type,"PD ", DF2, " FIX")]
### done add degrees of freedom
    
    ## derive the different sets of new lines needed
    ## $THETAP section
    thetas <- pars[par.type=="THETA"][order(i)]
    lines.thetap <- c("$THETAP", paste(thetas[,est], "FIX"))
    ## $THETAPV
    cov.l <- mat2dt(cov,as.fun="data.table")
    cov.l <- addParType(cov.l,suffix="i")
    cov.l <- addParType(cov.l,suffix="j")
    lines.thetapv <-
        NMcreateMatLines(cov.l[par.type.i=="THETA"&par.type.j=="THETA", .(i=j, j=i, value, parameter.i, parameter.j, par.type.i,  par.name, par.type.j)], type="OMEGA")
    lines.thetapv <- sub("\\$OMEGA","\\$THETAPV",lines.thetapv)
    lines.thetapv = prettyMatLines(lines.thetapv)
    
    ## $OMEGAP
    # note: set 0 FIXED sigmas/omegas to 1e-30 to avoid non-semi-positive definite matrices error
    lines.omegap <- NMcreateMatLines(pars[par.type=="OMEGA",.(par.type,parameter,par.name,i,j,FIX,value=ifelse(value==0,1e-30,value))],type="OMEGA")
    lines.omegap <- sub("\\$OMEGA","\\$OMEGAP",lines.omegap)
                                        # below was for previous version of NMcreateMatLines where it would not add FIX after non-block omegas. This was updated (in testing now)
                                        # lines.omegap  = sapply(lines.omegap, FUN = function(.x) ifelse((!grepl("BLOCK",.x)&!grepl("FIX",.x)), paste0(.x, " FIX"), .x), USE.NAMES = FALSE)
    lines.omegap <- lapply(X=lines.omegap, FUN=function(.x) ifelse(grepl("BLOCK",.x), return(prettyMatLines(block_mat_string = .x)), return(.x))) 
    lines.omegap <- unlist(lines.omegap)
    
    ## $OMEGAPD
    lines.omegapd = nwpri_df[par.type=="OMEGA"]$line
    
    ## $SIGMAP
    # note: set 0 FIXED sigmas/omegas to 1e-30 to avoid non-semi-positive definite matrices error
    lines.sigmap <- NMcreateMatLines(pars[par.type=="SIGMA",.(par.type,parameter,par.name,i,j,FIX,value=ifelse(value==0,1e-30,value))],type="SIGMA")
    lines.sigmap <- sub("\\$SIGMA","\\$SIGMAP",lines.sigmap)
                                        # lines.sigmap  = sapply(lines.sigmap, FUN = function(.x) ifelse((!grepl("BLOCK",.x)&!grepl("FIX",.x)), paste0(.x, " FIX"), .x), USE.NAMES = FALSE)
    lines.sigmap <- lapply(X=lines.sigmap, FUN=function(.x) ifelse(grepl("BLOCK",.x), return(prettyMatLines(block_mat_string = .x)), return(.x))) 
    lines.sigmap <- unlist(lines.sigmap)
    
    ## $SIGMAPD
    lines.sigmapd = nwpri_df[par.type=="SIGMA"]$line
    
    ## $PRIOR
    lines.prior = sprintf("$PRIOR NWPRI PLEV=%f",PLEV)
    
    all.lines = c(lines.prior, lines.thetap, lines.thetapv, lines.omegap, lines.omegapd, lines.sigmap, lines.sigmapd)

    
    ## insert the lines into file.sim using NMdata::NMwriteSection(). Please see other simulation methods for inspiration - NMsim_typical is a simple one that shows the drill.
    lines.sim <- NMdata:::NMwriteSectionOne(lines=lines.sim, section="SIMULATION", location="before", newlines=all.lines, backup=FALSE,quiet=TRUE)

### add TRUE=PRIOR to $SIMULATION
    
    lines.sim <- NMdata:::NMwriteSectionOne(lines=lines.sim,section="SIMULATION",location="after",newlines="TRUE=PRIOR",backup=FALSE,quiet=TRUE)

### update the simulation control stream
    ## if(return.text){
    ##     return(lines.sim)
    ## }
    
    writeTextFile(lines=lines.sim,file=file.sim)
    
    
    return(file.sim)

}
