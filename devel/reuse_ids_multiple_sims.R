## used 0.0.4 from CRAN
install.packages("NMsim")

library(NMsim)
library(NMdata)
NMdataConf(path.nonmem="/opt/nonmem/nm751/run/nmfe75")

file.project <- function(...)file.path(system.file("examples",package="NMsim"),...)
file.mod <- file.project("nonmem/xgxr021.mod")

## make a sim to generate N subjects
n <- 10
dt.dos.dummy <- NMcreateDoses(AMT=1,TIME=0)
dt.sim <- addEVID2(dt.dos.dummy,time.sim=1,CMT=2)

NMcheckData(dt.sim,type.data="sim")

simres.dummy <- NMsim(file.mod=file.mod,
                      data=dt.sim,
                      text.table="PRED IPRED",
                      ##                      subproblems=n,
                      list.sections=list(SIM=function(x)sub("ONLYSIM[A-Z]*","",x)),
                      dir.sims="~/NMsim_vignette")

simres.dummy

lst.sim <- "~/NMsim_vignette/xgxr021_noname/NMsim_xgxr021_noname.lst"
NMsim:::NMreadPhi(fnExtension(lst.sim,"phi"))

## create doses
doses <- NMcreateDoses(TIME=c(0,24),AMT=c(300,150),addl=data.frame(ADDL=c(0,5),II=c(0,24)),CMT=1)
doses <- transform(doses,trt="300 mg then 150 mg QD")
