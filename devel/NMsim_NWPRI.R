##' @import NMdata
##' @import data.table

NMsim_NWPRI <- function(file.sim,file.mod,data.sim){

### NMsim_default() is run because it insert $SIMULATION instead of $ESTIMATION and a few other things that are always needed. 
    files.needed.def <- NMsim_default(file.sim=file.sim,file.mod,data.sim)
    lines.sim <- readLines(file.sim)

    ## now we just have to do what is specific to this type of simulation
    
    cov <- NMreadCov(fnExtension(file.mod,".cov"))
    pars <- NMreadExt(file.mod,return="pars")

### add OMEGA block information based on off diagonal values
    tab.blocks <- rbind(pars[par.type%in%c("OMEGA","SIGMA"),.(par.type,i=i,j=j,value)],
                        pars[par.type%in%c("OMEGA","SIGMA"),.(par.type,i=j,j=i,value)])[
        abs(value)>1e-9,.(iblock=min(i,j),blocksize=max(abs(j-i))+1),by=.(par.type,i)]
    
    pars <- mergeCheck(pars,tab.blocks,by=cc(par.type,i),all.x=T)
    pars[abs(i-j)>(blocksize-1),(c("iblock","blocksize")):=list(NA,NA)]

    pars[par.type%in%c("OMEGA","SIGMA")&i==j&is.na(iblock),iblock:=i]
    pars[par.type%in%c("OMEGA","SIGMA")&i==j&iblock==i,blocksize:=1]
### done add OMEGA/SIGMA blocks
    

    
    ## derive the different sets of new lines needed

    ## insert the lines into file.sim using NMdata::NMwriteSection(). Please see other simulation methods for inspiration - NMsim_typical is a simple one that shows the drill.

    
    return(file.sim)

}
