##' Expand a set of covariate values into a data.set with reference
##' value
##' @param covlist A covariate specififed in a list. See
##'     ?expandCovLists.
##' @param data See ?expandCovLists.
##' @param col.id The subject ID column name. Necessary because
##'     quantiles sould be quantiles of distribution of covariate on
##'     subjects, not on observations (each subject contributes once).
##' @param sigdigs Used for rounding of covariate values if using
##'     quantiles or if using a function to find reference.
##' @keywords internal list should include file.mod <- file.mod <-
##'     system.file("examples/nonmem/xgxr134.mod",package="NMdata",quiet=TRUE)
##'     res <- NMdata::NMscanData(file.mod)
##'     completeCov(covlist=list(covvar="WEIGHTB",quantiles=c(25,75)/100,ref=median),data=res,sigdigs=3)
##' @importFrom stats quantile

completeCov <- function(covlist,data,col.id="ID",sigdigs=2){

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    ## quantile <- NULL

###  Section end: Dummy variables, only not to get NOTE's in pacakge checks

    
    if(!is.null(covlist$values) && !is.null(covlist$quantiles) ) stop("Please provide values _or_ quantiles, not both.")
    if(is.null(covlist$values) && is.null(covlist$quantiles) ) stop("Please provide values or quantiles.")

    
    ## add values if missing
    if(!is.null(covlist$quantiles)){
        if(!covlist$covvar %in% colnames(data)){
            stop(sprintf("Covariate %s is not a column in data.",covlist$covvar))
        }
        pars.id <- findCovs(data,by=col.id,as.fun="data.table")
        if(!covlist$covvar %in% colnames(pars.id)){
            stop(sprintf("Covariate %s is not constant within %s in data.",covlist$covvar,col.id))
        }
        covlist$values <- pars.id[,quantile(get(covlist$covvar),probs=covlist$quantiles)] |>
            signif(digits=sigdigs)
    }

    ## handle ref if a function
    if(is.function(covlist$ref)){
        if(!covlist$covvar %in% colnames(data)){
            stop(sprintf("Covariate %s is not a column in data.",covlist$covvar))
        }
        pars.id <- findCovs(data,by=col.id,as.fun="data.table")
        if(!covlist$covvar %in% colnames(pars.id)){
            stop(sprintf("Covariate %s is not constant within %s in data.",covlist$covvar,col.id))
        }

        covlist$ref <- pars.id[,covlist$ref(get(covlist$covvar))] |>
            signif(digits=sigdigs)
    }

    ## fill in label if missing
    if(is.null(covlist$label)) covlist$label <- paste(covlist$covvar)
    
    ## make data.table
    with(covlist,
         data.table(covvar=covvar,covval=c(values,ref),label=label,covref=ref,type=c(rep("value",length(values)),"ref"))
         )
    
}
