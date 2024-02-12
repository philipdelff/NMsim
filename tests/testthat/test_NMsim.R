## library(devtools)
if(F){
library(NMdata)
library(data.table)
data.table::setDTthreads(1)

dt.amt <- data.table(DOSE=c(100,400))
dt.amt[,AMT:=DOSE*1000]
dt.amt
doses.sd <- NMcreateDoses(TIME=0,AMT=dt.amt,as.fun="data.table")
doses.sd[,dose:=paste(DOSE,"mg")]
doses.sd[,regimen:="SD"]


dat.sim.sd <- addEVID2(doses.sd,time.sim=0:24,CMT=2,as.fun="data.table")
dat.sim <- copy(dat.sim.sd)

## NMcheckData(dat.sim)

dat.sim[,ROW:=.I]

head(dat.sim)

dat.sim[,BBW:=75]




context("NMsim")
test_that("Basic",{

###  On windows this gives and error that tmp.dat is not found.
    fileRef <- "testReference/NMsim_01.rds"

    file.mod <- "testData/nonmem/xgxr025.mod"
    sim1 <- NMsim(file.mod=file.mod,
                  data=dat.sim,
                  dir.sim="testOutput",
                  suffix.sim = "sd1",
                  seed=2342,
                  execute=FALSE)

    expect_equal_to_reference(sim1,fileRef)

})
}
