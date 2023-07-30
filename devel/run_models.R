library(devtools)
load_all()
getwd()
NMexec("inst/examples/nonmem/xgxr014.mod",sge=FALSE)

NMexec("inst/examples/nonmem/xgxr114.mod",sge=FALSE)

