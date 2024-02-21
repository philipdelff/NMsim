## library(devtools)

library(NMdata)
library(data.table)
data.table::setDTthreads(1)


if(F){
    ## prepare a sim to read
    dt.amt <- data.table(DOSE=c(100,400))
    dt.amt[,AMT:=DOSE*1000]
    dt.amt
    doses.sd <- NMcreateDoses(TIME=0,AMT=dt.amt)
    doses.sd[,dose:=paste(DOSE,"mg")]
    doses.sd[,regimen:="SD"]
    doses.sd
    dat.sim.sd <- addEVID2(doses.sd,time.sim=0:24,CMT=2,as.fun="data.table")
    dat.sim <- copy(dat.sim.sd)
    dat.sim[,ROW:=.I]
    dat.sim[,BBW:=75]


    file.mod <- "testData/nonmem/xgxr021.mod"
    sim1 <- NMsim(file.mod=file.mod,
                  data=dat.sim,
                  dir.sim="testOutput",
                  suffix.sim = "sd1_NMreadSim",
                  seed=2342)

    
}

context("NMreadSim")

test_that("Basic",{
    fileRef <- "testReference/NMreadSim_01.rds"
    res1 <- NMreadSim("testOutput/NMsim_xgxr021_sd1_NMreadSim_paths.rds")
    
    expect_equal_to_reference(res1,fileRef)
})
