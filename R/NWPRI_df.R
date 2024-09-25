

NWPRI_df <- function(pars){
    pars <- copy(as.data.table(pars))
    
### Add degrees of freedom for inverse-wishart distribution for OMEGA/SIGMA
    pars[par.type%in%c("OMEGA","SIGMA")&i==j&!is.na(iblock), N := 2*((est**2)/(se**2)) + 1]
    pars[par.type%in%c("OMEGA","SIGMA")&i==j&!is.na(iblock), DF := N-blocksize-1]
    ## DF cannot be smaller than the number of parameters in the block
    ## pars[par.type%in%c("OMEGA","SIGMA")&i==j&!is.na(iblock), DF := ifelse(DF<blocksize, blocksize, DF)]
    pars[par.type%in%c("OMEGA","SIGMA")&i==j&!is.na(iblock), DF := pmax(DF,blocksize)]
    ## If parameter is fixed, set DF=block size to indicate we want a point estimate.
    ## pars[par.type%in%c("OMEGA","SIGMA")&i==j&!is.na(iblock), DF := ifelse(FIX==1, blocksize, DF)]
    pars[par.type%in%c("OMEGA","SIGMA")&i==j&!is.na(iblock)&FIX==1, DF:=blocksize]
    ## take the minimum DF per omega/sigma matrix:
    pars[par.type%in%c("OMEGA","SIGMA")&i==j&!is.na(iblock), DF2 := min(DF, na.rm = TRUE), by = .(par.type,iblock)]
    
    nwpri_df <- unique(pars[par.type%in%c("OMEGA","SIGMA")&i==j&!is.na(iblock),.(par.type,iblock, DF2)])
    nwpri_df
}
