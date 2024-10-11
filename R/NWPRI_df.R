##' Add degrees of freedom by OMEGA/SIGMA block
##'
##' Calculate and add degrees of freedom to be used for simulation
##' using the inverse Wishart distribution.
##'
##' @param pars Parameters in long format, as returned by
##'     `NMreadExt()`.
##' @return A data.table with DF2 added. See details.
##' @details The degrees of freedom are calculated as DF =
##'     2*((est**2)/(se**2)) + 1 -blocksize-1 DF2 is then adjusted to
##'     not be greater than the blocksize, and the minumum degrees of
##'     freedom observed in the block is applied to the full
##'     block. For fixed parameters, DF2 equals the blocksize.
##' @references \href{https://ascpt.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002\%2Fpsp4.12422&file=psp412422-sup-0001-Supinfo1.pdf}{inverse-Wishart degrees of freedom calculation for OMEGA and SIGMA: NONMEM tutorial part II, supplement 1, part C.}
##' @seealso NMsim_NWPRI

## Don't export


NWPRI_df <- function(pars){

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

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
    par.type <- NULL
    se <- NULL

### Section end: Dummy variables, only not to get NOTE's in pacakge checks

    pars <- copy(as.data.table(pars))
    
### Add degrees of freedom for inverse-wishart distribution for OMEGA/SIGMA
    pars[par.type%in%c("OMEGA","SIGMA")&i==j&!is.na(iblock), N := 2*((est**2)/(se**2)) + 1]
    pars[par.type%in%c("OMEGA","SIGMA")&i==j&!is.na(iblock), DF := N-blocksize-1]
    ## DF cannot be smaller than the number of parameters in the block
    pars[par.type%in%c("OMEGA","SIGMA")&i==j&!is.na(iblock), DF := pmax(DF,blocksize)]
    ## If parameter is fixed, set DF=block size to indicate we want a point estimate.
    pars[par.type%in%c("OMEGA","SIGMA")&i==j&!is.na(iblock)&FIX==1, DF:=blocksize]
    ## take the minimum DF per omega/sigma matrix:
    pars[par.type%in%c("OMEGA","SIGMA")&i==j&!is.na(iblock), DF2 := min(DF, na.rm = TRUE), by = .(par.type,iblock)]
    
    nwpri_df <- unique(pars[par.type%in%c("OMEGA","SIGMA")&i==j&!is.na(iblock),.(par.type,iblock, DF2)])
    nwpri_df
}
