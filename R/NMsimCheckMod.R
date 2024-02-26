##' Check a simulation control streams for things that can cause trouble in NMsim
##' @param file.mod A control stream to check
##' @keywords internal
## Don't export

NMsimCheckMod <- function(file.mod){
    
    pk <- NULL
    pred <- NULL
    
    sections.mod <- NMreadSection(file.mod)
    names(sections.mod) <- tolower(names(sections.mod))
    secnames <- copy(names(sections.mod))

    lines.pred.or.pk <- c(sections.mod[["pk"]],sections.mod[["pred"]])
    if(!is.null(lines.pred.or.pk)){
        if(any(grepl("PRED_IGNORE_DATA_TEST",lines.pred.or.pk))){
            warning("PRED_IGNORE_DATA_TEST found in $PK or $PRED. If the ignore condition matches data records, they will be dropped. Consider if you can avoid this. If intended, you must still make sure that simulation input and output data is merged by a row identifier. See  ")
            }
    }

    
    if("error"%in%secnames){
        if(any(grepl("OBSERVATIONS +ONLY",sections.mod[["error"]]))){
            warning("OBSERVATIONS ONLY used in $ERROR. Predictions may not be evaluated for anything else than EVID=0 records.")
            }
    }
    return(NULL)
}
