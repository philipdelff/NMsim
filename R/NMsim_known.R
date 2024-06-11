##' NMsim_known is an old name for NMsim_EBE()
##' @param ... Everything passed to NMsim_EBE()
##' @export
##' @keywords internal


NMsim_known <- function(...){


    message("NMsim_known() has been renamed to NMsim_EBE(). Calling NMsim_EBE().")
    
    NMsim_EBE(...)

}
