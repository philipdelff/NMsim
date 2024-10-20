##' Create data set where each covariate is univariately varied
##' 
##' Each covariate is univariately varied while other covariates are
##' kept at reference values. This structure is often used for
##' forest-plot type simulations.
##' 
##' @param ... Covariates provided as lists - see examples. The
##'     name of the arguement must match columns in data set. An
##'     element called ref must contain either a reference value or a
##'     function to use to derive the reference value from data
##'     (e.g. `median`). Provide either `values` or `quantiles` to
##'     define the covariate values of interest (typically, the values
##'     that should later be simulated and maybe shown in a forest
##'     plot). `label` is optional - if missing, the argument name
##'     will be used.
##' @param data A data set needed if the reference(s) value of one or
##'     more covariates is/are provided as functions (like median), or
##'     if covariate values are provided as quantiles.
##' @param col.id The subject ID column name. Necessary because
##'     quantiles sould be quantiles of distribution of covariate on
##'     subjects, not on observations (each subject contributes once).
##' @param sigdigs Used for rounding of covariate values if using
##'     quantiles or if using a function to find reference.
##' @examples
##' \dontrun{
##' file.mod <- system.file("examples/nonmem/xgxr134.mod",package="NMdata")
##' res <- NMdata::NMscanData(file.mod)
##' expandCovLists(
##'     WEIGHTB=list(ref=70,values=c(40,60,80,100),label="Bodyweight (kg)"),
##' ## notice, values OR quantiles can be provided
##'     AGE=list(ref=median, quantiles=c(10,25,75,90)/100, label="Age (years)"
##'              ),
##'     data=res
##' )
##' }
##' @export

expandCovLists <- function(...,data,col.id,sigdigs=2){


#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    . <- NULL
    covvar <- NULL
    covref <- NULL
    . <- NULL
    covval <- NULL

###  Section end: Dummy variables, only not to get NOTE's in pacakge checks
    
    covlists <- list(...)
    if(!is.null(names(covlists))){
        for(n in 1:length(covlists)){
            covlists[[n]]$covvar <- names(covlists)[n]
        }
    }
    
    if(missing(data)) data <- NULL
    
    ## should add quantiles too
    covLists <- lapply(covlists,completeCov,data=data,col.id=col.id,sigdigs=sigdigs)
    allcovs <- rbindlist(covLists)
    ## add in covvar ref values. Merge on wide dt.ref, then adjust each row.
    dt.ref.w <- unique(allcovs[,.(covvar,covref)]) |> dcast(.~covvar,value.var="covref")
    dt.ref.w[,.:=NULL]

    
    allcovs <- allcovs[,dt.ref.w[],by=allcovs]
    for(I in 1:nrow(allcovs)){
        this.cov <- allcovs[I,covvar]
        allcovs[I,(this.cov):=covval]
    }
    
    allcovs[]
}


