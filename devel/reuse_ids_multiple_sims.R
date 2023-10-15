#### this method works. We are reproducing the ETAS from one sim to
#### the next. But the NMsim_... naming convension is annoying. Also
#### to the right, the sim model names will grow longer. That should
#### be fixed.

## used 0.0.4 from CRAN
## install.packages("NMsim")

## library(NMsim)
library(NMdata)
## NMdataConf(path.nonmem="/opt/nonmem/nm751/run/nmfe75")
NMdataConf(path.nonmem="/opt/NONMEM/nm75/run/nmfe75")

## library(devtools)
## load_all("~/wdirs/NMsim")

library(NMsim)

file.project <- function(...)file.path(system.file("examples",package="NMsim"),...)
file.mod <- file.project("nonmem/xgxr021.mod")

## make a sim to generate N subjects
n <- 10
dt.dos.dummy <- NMcreateDoses(AMT=1,TIME=0)
dt.sim <- addEVID2(dt.dos.dummy,time.sim=1:4,CMT=2)

NMcheckData(dt.sim,type.data="sim")

simres.dummy <- NMsim(file.mod=file.mod,
                      data=dt.sim,
                      text.table="ID ETA1 ETA2 ETA3 ETA4 ETA5 NOAPPEND NOPRINT FIRSTONLY",
                      ##                      subproblems=n,
                      ## list.sections=list(SIM=function(x)sub("ONLYSIM[A-Z]*","",x)),
                      ## text.sim="",
                      name.sim="gensubjs",
                      dir.sims="~/NMsim_vignette")


source("~/wdirs/NMsim/R/genPhiFile.R")
library(data.table)
library(NMsim)
genPhiFile(simres.dummy,file="~/NMsim_vignette/xgxr021_gensubjs/NMsim_xgxr021_gensubjs.phi")

NMinfo(simres.dummy)
simres.reused <- NMsim(file.mod="~/NMsim_vignette/xgxr021_gensubjs/NMsim_xgxr021_gensubjs.mod",
                      data=dt.sim,
                      text.table="IPRED PRED ETA1 ETA2 ETA3 ETA4 ETA5 NOAPPEND NOPRINT",
                      name.sim="reusesubjs",
                      method.sim=NMsim_known,
                      dir.sims="~/NMsim_vignette")


simres.dummy
simres.reused





### 
