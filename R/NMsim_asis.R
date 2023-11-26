##' Simulation method that uses the provided control stream as is
##'
##' The simplest of all method. It does nothing (but again,
##' \code{NMsim} handles `$INPUT`, `$DATA`, `$TABLE` and more. Use
##' this for instance if you already created a simulation (or
##' estimation actually) control stream and want NMsim to run it on
##' different data sets.
##'
##' @param file.sim See \code{?NMsim}.
##' @param file.mod See \code{?NMsim}.
##' @param data.sim See \code{?NMsim}.
##' @return Path to simulation control stream
##' @export 

NMsim_asis <- function(file.sim,file.mod,data.sim){

    return(file.sim)

}
