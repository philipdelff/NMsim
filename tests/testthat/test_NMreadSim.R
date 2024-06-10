## library(devtools)

#### need a function to drop NMsimVersion and NMsimTime from table
fix.time <- function(x){
    meta.x <- attr(x,"NMsimModTab")
    ## meta.x$time.call <- as.POSIXct("2020-02-01 00:01:01",tz="UTC")
    meta.x$NMsimVersion <- NULL
    meta.x$NMsimTime <- NULL
    
    setattr(x,"NMsimModTab",meta.x)
    invisible(x)
}

library(NMdata)
library(data.table)
data.table::setDTthreads(1)



context("NMreadSim")

## prepare a sim to read
dt.amt <- data.table(DOSE=c(100,400))
dt.amt[,AMT:=DOSE*1000]
dt.amt
doses.sd <- NMcreateDoses(TIME=0,AMT=dt.amt,as.fun="data.table")
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
              name.sim = "sd1_NMreadSim",
              seed.nm=2342)





test_that("Basic",{
    fileRef <- "testReference/NMreadSim_01.rds"
    ## ref <- readRDS(fileRef)
    res1 <- NMreadSim("testOutput/NMsim_xgxr021_sd1_NMreadSim_paths.rds")

    fix.time(res1)
    
    expect_equal_to_reference(res1,fileRef)
})

