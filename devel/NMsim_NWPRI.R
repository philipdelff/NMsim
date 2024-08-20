##' @import NMdata
##' @import data.table
# devtools::load_all(here::here("wdirs/NMdata"))
# NMdata::NMdataConf(as.fun = "data.table")
# devtools::load_all(here::here("wdirs/NMsim"))
# file.mod <- here::here("models/mod_lorlatinib_estimate/mod_lorlatinib_estimate.mod")
# data.sim = data.table::fread(here::here("derived_data/simulated_nonmem_dataset_mod_lorlatinib.csv"))
# file.sim = here::here("models/mod_lorlatinib_estimate/NMsim/mod_lorlatinib_estimate_noname/mod_lorlatinib_estimate_noname.mod")

NMsim_NWPRI <- function(file.sim,file.mod,data.sim){

### NMsim_default() is run because it insert $SIMULATION instead of $ESTIMATION and a few other things that are always needed. 
    files.needed.def <- NMsim_default(file.sim=file.sim,file.mod,data.sim)
    lines.sim <- readLines(file.sim)

    ## now we just have to do what is specific to this type of simulation
    
    cov <- NMreadCov(fnExtension(file.mod,".cov"))
    pars <- NMreadExt(file.mod,return="pars")[,value:=est]
    
### Add degrees of freedom for inverse-wishart distribution for OMEGA/SIGMA
    # reference for DF calculation:  
    #  NONMEM tutorial part II, supplement 1, part C:
    #  https://ascpt.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fpsp4.12422&file=psp412422-sup-0001-Supinfo1.pdf
    pars[par.type%in%c("OMEGA","SIGMA")&i==j&!is.na(iblock), N := 2*((est**2)/(se**2)) + 1]
    pars[par.type%in%c("OMEGA","SIGMA")&i==j&!is.na(iblock), DF := N-blocksize-1]
    # DF cannot be smaller than the number of parameters in the block
    pars[par.type%in%c("OMEGA","SIGMA")&i==j&!is.na(iblock), DF := ifelse(DF<blocksize, blocksize, DF)]
    # If parameter is fixed, set DF=dimension of omega/sigma block for an uninformative distribution
    pars[par.type%in%c("OMEGA","SIGMA")&i==j&!is.na(iblock), DF := ifelse(FIX==1, blocksize, DF)]
    # take the minimum DF per omega/sigma matrix:
    pars[par.type%in%c("OMEGA","SIGMA")&i==j&!is.na(iblock), DF2 := min(DF, na.rm = TRUE), by = .(par.type,iblock)]
    
    nwpri_df = unique(pars[par.type%in%c("OMEGA","SIGMA")&i==j&!is.na(iblock),.(par.type,iblock, DF2)])
    nwpri_df[,line := paste0("$", par.type,"PD ", DF2, " FIXED")]
### done add degrees of freedom
    
    ## derive the different sets of new lines needed
    ## $THETAP section
    thetas <- pars[par.type=="THETA"][order(i)]
    lines.thetap <- c("$THETAP", paste(thetas[,est], "FIXED"))
    ## $THETAPV
    cov.l <- mat2dt(cov)
    cov.l <- NMdata:::addParType(cov.l,suffix="i")
    cov.l <- NMdata:::addParType(cov.l,suffix="j")
    lines.thetapv <-
        NMsim:::NMcreateMatLines(cov.l[par.type.i=="THETA"&par.type.j=="THETA", .(i=j, j=i, value, parameter.i, parameter.j, par.type.i,  par.name, par.type.j)], type="OMEGA")
    lines.thetapv <- sub("\\$OMEGA","\\$THETAPV",lines.thetapv)
    lines.thetapv = prettyMatLines(lines.thetapv)
 
    ## $OMEGAP
    lines.omegap <- NMsim:::NMcreateMatLines(pars[par.type=="OMEGA"],type="OMEGA")
    lines.omegap <- sub("\\$OMEGA","\\$OMEGAP",lines.omegap)
    # below was for previous version of NMcreateMatLines where it would not add FIX after non-block omegas. This was updated (in testing now)
    # lines.omegap  = sapply(lines.omegap, FUN = function(.x) ifelse((!grepl("BLOCK",.x)&!grepl("FIX",.x)), paste0(.x, " FIX"), .x), USE.NAMES = FALSE)
    lines.omegap <- lapply(X=lines.omegap, FUN=function(.x) ifelse(grepl("BLOCK",.x), return(prettyMatLines(block_mat_string = .x)), return(.x))) |> unlist()
 
    ## $OMEGAPD
    lines.omegapd = nwpri_df[par.type=="OMEGA"]$line
    
    ## $SIGMAP
    lines.sigmap <- NMsim:::NMcreateMatLines(pars[par.type=="SIGMA"],type="SIGMA")
    lines.sigmap <- sub("\\$SIGMA","\\$SIGMAP",lines.sigmap)
    # lines.sigmap  = sapply(lines.sigmap, FUN = function(.x) ifelse((!grepl("BLOCK",.x)&!grepl("FIX",.x)), paste0(.x, " FIX"), .x), USE.NAMES = FALSE)
    lines.sigmap <- lapply(X=lines.sigmap, FUN=function(.x) ifelse(grepl("BLOCK",.x), return(prettyMatLines(block_mat_string = .x)), return(.x))) |> unlist()
    
    ## $SIGMAPD
    lines.sigmapd = nwpri_df[par.type=="SIGMA"]$line
    
    ## $PRIOR
    lines.prior = "$PRIOR NWPRI PLEV=0.999"
    
    all.lines = c(lines.prior, lines.thetap, lines.thetapv, lines.omegap, lines.omegapd, lines.sigmap, lines.sigmapd)
    
    ## insert the lines into file.sim using NMdata::NMwriteSection(). Please see other simulation methods for inspiration - NMsim_typical is a simple one that shows the drill.
    lines.sim <- NMdata:::NMwriteSectionOne(lines=lines.sim, section="SIMULATION", location="before", newlines=all.lines, backup=FALSE,quiet=TRUE)
    
    if(return.text){
        return(lines.sim)            
    }
    
    writeTextFile(lines=lines.sim,file=file.sim)
    
    return(file.sim)

}
