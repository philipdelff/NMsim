## library(devtools)
## if(F){
library(NMdata)
### NMsim does not put "ROW" first with NMdata 0.1.5. Not sure why.
stopifnot(packageVersion("NMdata")>="0.1.6")
library(data.table)
data.table::setDTthreads(1)

NMdataConf(
    path.nonmem="/opt/NONMEM/nm75/run/nmfe75",
    dir.sims="testOutput/simtmp",dir.res="testOutput/simres")

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
                  name.sim = "sd1",
                  seed.nm=2342,
                  execute=FALSE,
                  method.update.inits="nmsim")

    ## ref <- readRDS(fileRef)
    expect_equal_to_reference(sim1,fileRef)

})
## }


if(FALSE){

    file.mod <- "testData/nonmem/xgxr021.mod"
    sim1 <- NMsim(file.mod=file.mod,
                  data=dat.sim,
                  dir.sim="testOutput",
                  suffix.sim = "sd1",
                  seed=2342,
                  ## execute=FALSE,
                  method.update.inits="nmsim"
                  )


}

test_that("modify.model",{

    fileRef <- "testReference/NMsim_02.rds"
        
    file.mod <- "testData/nonmem/xgxr021.mod"
    sim1 <- NMsim(file.mod=file.mod,
                  data=dat.sim,
                  dir.sim="testOutput",
                  name.sim = "sd1_modify",
                  seed.nm=2342,
                  modify.model=list(pk=add("CL=CL/2","V2=V2*2"),
                                    error=overwrite("W=1","W=2")),
                  execute=FALSE,
                  method.update.inits="nmsim")

    mod <- NMreadSection("testOutput/xgxr021_sd1_modify/xgxr021_sd1_modify.mod")
    

    ## ref <- readRDS(fileRef)
    expect_equal_to_reference(mod,fileRef)


})

test_that("NMsim_EBE",{

    fileRef <- "testReference/NMsim_EBE_03.rds"
        
    file.mod <- "testData/nonmem/xgxr021.mod"
    res <- NMscanInput(file.mod,file.mod=file.mod,apply.filters=T)
    
    dat.sim.ebe <- dat.sim[ID==1]
    dat.sim.ebe[,ID:=unique(res$ID)[1]]

    sim1 <- NMsim(file.mod=file.mod,
                  data=dat.sim.ebe,
                  dir.sim="testOutput",
                  name.sim = "sd1_EBE",
                  method.sim=NMsim_EBE,
                  seed.nm=2342,
                  execute=FALSE,
                  method.update.inits="nmsim")

    mod <- NMreadSection("testOutput/xgxr021_sd1_EBE/xgxr021_sd1_EBE.mod")
    

    ## ref <- readRDS(fileRef)
    expect_equal_to_reference(mod,fileRef)


})

if(F){
test_that("NMsim_VarCov",{

    fileRef <- "testReference/NMsim_VarCov_04.rds"
        
    file.mod <- "testData/nonmem/xgxr032.mod"

    sim1 <- NMsim(file.mod=file.mod,
                  data=dat.sim,
                  dir.sim="testOutput",
                  name.sim = "sd1_VarCov",
                  method.sim=NMsim_VarCov,
                  seed.nm=2342,
                  seed.R=2,
                  execute=FALSE,
                  method.update.inits="nmsim")

    mod <- NMreadSection("testOutput/xgxr032_sd1_VarCov/xgxr032_sd1_VarCov_1.mod")
    

    ## ref <- readRDS(fileRef)
    expect_equal_to_reference(mod,fileRef)

})
}
