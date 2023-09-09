library(devtools)
load_all()
getwd()

NMexec("inst/examples/nonmem/xgxr014.mod",sge=FALSE)

NMexec("inst/examples/nonmem/xgxr114.mod",sge=FALSE)

setwd("~/wdirs/NMsim/inst/examples/nonmem/")
list.files()

## NMexec must test if psn is available before running
NMexec("xgxr021.mod",sge=FALSE)


## NMexec must test if nonmem is available before running
NMexec("xgxr021.mod",sge=FALSE,method.execute="directory")


## this is bad. directory doesn't work. How can we make that
## unavailable when not called by NMsim?
load_all()
NMexec("xgxr021.mod",sge=FALSE,method.execute="directory",path.nonmem="/opt/nonmem/nm751/run/nmfe75")
