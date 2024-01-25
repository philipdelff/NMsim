library(devtools)
load_all()

library(NMdata)
NMdataConf(as.fun="data.table")
library(data.table)

dt.dos <- NMcreateDoses(AMT=300,TIME=0)
dt.sim <- addEVID2(doses=dt.dos,time.sim=c(1,6,12),CMT=2)
dt.sim[,BBW:=40][,ROW:=.I]

dt.sim.known <- egdt(dt.sim[,!("ID")],data.table(ID=101:105))
setorder(dt.sim.known,ID,TIME,EVID,CMT)


## NMdataConf(dir.psn="/opt/psn")
NMdataConf(path.nonmem ="C:/nm75g64/run/nmfe75.bat")
NMdataConf(dir.psn=NULL)
##

## 025 doesn't seem stable. Got Q~1e7 and Nonmem didn't run
file.mod <- "../tests/testthat/testData/nonmem/xgxr021.mod"
## NMdata:::NMreadExt( fnExtension(file.mod,"ext"))
## library(nonmem2R)
## extload(file.mod)

load_all()

set.seed(43)
simres <- NMsim(file.mod,
                data=dt.sim,
                text.table="PRED IPRED",
                dir.sims="testOutput",
                name.sim="default_01"
                ,method.update.inits="nmsim"
                )


## not specifying method.update.inits
simres <- NMsim(file.mod,
                data=dt.sim,
                text.table="PRED IPRED",
                dir.sims="testOutput",
                name.sim="default_01"
                )
