### this is run every time library(NMsim) or require(NMsim) is
### called.


##' @importFrom utils packageVersion
.onAttach <- function(libname,pkgname){
    packageStartupMessage(paste0("NMsim ",packageVersion("NMsim"),". Browse NMsim documentation at\nhttps://philipdelff.github.io/NMsim/"))
}
