library(NMdata)
library(NMsim)

NMdataConf(path.nonmem="/opt/NONMEM/nm75/run/nmfe75")
NMdataConf(as.fun="data.table")


NMexec("~/wdirs/NMsim/inst/examples/nonmem/xgxr022.mod",sge=FALSE)
NMexec("~/wdirs/NMsim/inst/examples/nonmem/xgxr032.mod",sge=FALSE)
NMexec("~/wdirs/NMsim/inst/examples/nonmem/xgxr033.mod",sge=TRUE,method.execute="psn",nc=72)
NMexec("~/wdirs/NMsim/inst/examples/nonmem/xgxr043.mod",sge=TRUE,method.execute="psn",nc=72)
NMexec("~/wdirs/NMsim/inst/examples/nonmem/xgxr044.mod",sge=TRUE,method.execute="psn",nc=72)
NMexec("~/wdirs/NMsim/inst/examples/nonmem/xgxr045.mod",sge=TRUE,method.execute="psn",nc=72)
NMexec("~/wdirs/NMsim/inst/examples/nonmem/xgxr046.mod",sge=FALSE)
NMexec("~/wdirs/NMsim/inst/examples/nonmem/xgxr132.mod",sge=FALSE)


