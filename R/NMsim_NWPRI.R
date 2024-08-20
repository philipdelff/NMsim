##' Simulate with uncertainty using inverse-Wishart distribution for OMEGA and SIGMA parameters
##' @param file.sim See \code{?NMsim}.
##' @param file.mod See \code{?NMsim}.
##' @param data.sim See \code{?NMsim}.
##' @param PLEV Used in \code{$PRIOR NWPRI PLEV=0.999}
##' @details Simulate with parameter uncertainty. THETA parameters are
##'     sampled from a multivariate normal distribution while OMEGA
##'     and SIGMA are simulated from the inverse Wishart
##'     distribution. Correlations of OMEGA and SIGMA parameters will
##'     only be applied within modeled "blocks".
##' @references DF calculation: NONMEM tutorial part II, supplement 1,
##'     part C.
##'     https://ascpt.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fpsp4.12422&file=psp412422-sup-0001-Supinfo1.pdf
##' @seealso NMsim_VarCov
##' @import NMdata
##' @import data.table
##' @export

NMsim_NWPRI <- function(file.sim,file.mod,data.sim,PLEV=0.999){

    . <- NULL
    DF <- NULL
    DF2 <- NULL
    FIX <- NULL
    N <- NULL
    blocksize <- NULL
    est <- NULL
    i <- NULL
    iblock <- NULL
    j <- NULL
    line <- NULL
    par.name <- NULL
    par.type <- NULL
    par.type.i <- NULL
    par.type.j <- NULL
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

####### identifying iblock and blocksize. Needed until NMdata >= 0.1.7
### add OMEGA block information based on off diagonal values
    tab.blocks <- rbind(pars[par.type%in%c("OMEGA","SIGMA"),.(par.type,i=i,j=j,value)],
                        pars[par.type%in%c("OMEGA","SIGMA"),.(par.type,i=j,j=i,value)])[
        abs(value)>1e-9,.(iblock=min(i,j),blocksize=max(abs(j-i))+1),by=.(par.type,i)]
    
    pars <- mergeCheck(pars[,setdiff(colnames(pars),c("iblock","blocksize")),with=FALSE],tab.blocks,by=cc(par.type,i),all.x=T,quiet=TRUE)
    pars[abs(i-j)>(blocksize-1),(c("iblock","blocksize")):=list(NA,NA)]

    pars[par.type%in%c("OMEGA","SIGMA")&i==j&is.na(iblock),iblock:=i]
    pars[par.type%in%c("OMEGA","SIGMA")&i==j&iblock==i,blocksize:=1]
### done add OMEGA/SIGMA blocks

    
### Add degrees of freedom for inverse-wishart distribution for OMEGA/SIGMA
    pars[par.type%in%c("OMEGA","SIGMA")&i==j&!is.na(iblock), N := 2*((est**2)/(se**2)) + 1]
    pars[par.type%in%c("OMEGA","SIGMA")&i==j&!is.na(iblock), DF := N-blocksize-1]
    ## DF cannot be smaller than the number of parameters in the block
    pars[par.type%in%c("OMEGA","SIGMA")&i==j&!is.na(iblock), DF := ifelse(DF<blocksize, blocksize, DF)]
    ## If parameter is fixed, set DF=dimension of omega/sigma block for an uninformative distribution
    pars[par.type%in%c("OMEGA","SIGMA")&i==j&!is.na(iblock), DF := ifelse(FIX==1, blocksize, DF)]
    ## take the minimum DF per omega/sigma matrix:
    pars[par.type%in%c("OMEGA","SIGMA")&i==j&!is.na(iblock), DF2 := min(DF, na.rm = TRUE), by = .(par.type,iblock)]
    
    nwpri_df = unique(pars[par.type%in%c("OMEGA","SIGMA")&i==j&!is.na(iblock),.(par.type,iblock, DF2)])
    nwpri_df[,line := paste0("$", par.type,"PD ", DF2, " FIXED")]
### done add degrees of freedom
    
    ## derive the different sets of new lines needed
    ## $THETAP section
    thetas <- pars[par.type=="THETA"][order(i)]
    lines.thetap <- c("$THETAP", paste(thetas[,est], "FIXED"))
    ## $THETAPV
    cov.l <- mat2dt(cov,as.fun="data.table")
    cov.l <- addParType(cov.l,suffix="i")
    cov.l <- addParType(cov.l,suffix="j")
    lines.thetapv <-
        NMcreateMatLines(cov.l[par.type.i=="THETA"&par.type.j=="THETA", .(i=j, j=i, value, parameter.i, parameter.j, par.type.i,  par.name, par.type.j)], type="OMEGA")
    lines.thetapv <- sub("\\$OMEGA","\\$THETAPV",lines.thetapv)
    lines.thetapv = prettyMatLines(lines.thetapv)
    
    ## $OMEGAP
    lines.omegap <- NMcreateMatLines(pars[par.type=="OMEGA"],type="OMEGA")
    lines.omegap <- sub("\\$OMEGA","\\$OMEGAP",lines.omegap)
                                        # below was for previous version of NMcreateMatLines where it would not add FIX after non-block omegas. This was updated (in testing now)
                                        # lines.omegap  = sapply(lines.omegap, FUN = function(.x) ifelse((!grepl("BLOCK",.x)&!grepl("FIX",.x)), paste0(.x, " FIX"), .x), USE.NAMES = FALSE)
    lines.omegap <- lapply(X=lines.omegap, FUN=function(.x) ifelse(grepl("BLOCK",.x), return(prettyMatLines(block_mat_string = .x)), return(.x))) |> unlist()
    
    ## $OMEGAPD
    lines.omegapd = nwpri_df[par.type=="OMEGA"]$line
    
    ## $SIGMAP
    lines.sigmap <- NMcreateMatLines(pars[par.type=="SIGMA"],type="SIGMA")
    lines.sigmap <- sub("\\$SIGMA","\\$SIGMAP",lines.sigmap)
                                        # lines.sigmap  = sapply(lines.sigmap, FUN = function(.x) ifelse((!grepl("BLOCK",.x)&!grepl("FIX",.x)), paste0(.x, " FIX"), .x), USE.NAMES = FALSE)
    lines.sigmap <- lapply(X=lines.sigmap, FUN=function(.x) ifelse(grepl("BLOCK",.x), return(prettyMatLines(block_mat_string = .x)), return(.x))) |> unlist()
    
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
