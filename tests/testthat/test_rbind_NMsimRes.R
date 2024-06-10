## library(devtools)
## load_all()

library(NMdata)
NMdataConf(dir.psn=NULL)
NMdataConf(as.fun="data.table")

path.nonmem <- "/opt/NONMEM/nm75/run/nmfe75" 
file.exists(path.nonmem)
NMdataConf(path.nonmem=path.nonmem
          ,dir.sims="testOutput")



if(F){
    ## need a sim to test reading, rbinding etc
    dt.dos <- NMcreateDoses(AMT=300,TIME=0)
    dt.sim <- addEVID2(doses=dt.dos,time.sim=c(1,6,12),CMT=2)
    dt.sim[,BBW:=40][,ROW:=.I]

    file.mod <- "../../tests/testthat/testData/nonmem/xgxr021.mod"
    ## NMdata:::NMreadExt( fnExtension(file.mod,"ext"))
    ## library(nonmem2R)
    ## extload(file.mod)

    set.seed(43)
    simres <- NMsim(file.mod,
                    data=dt.sim,
                    table.var="PRED IPRED",
                    ## dir.sims="testData",
                    name.sim="default_01"
                    )

}

test_that("Basic",{

    fileRef <- "testReference/rbind_NMsimRes_01.rds"
    res1 <- NMreadSim("testOutput/NMsim_xgxr021_default_01_paths.rds")

    res1 <- res1[,rep:=1]
    res2 <- copy(res1)[,rep:=2]
    class(res1)
    r2 <- rbind.NMsimRes(res1,res2)
    
    expect_equal_to_reference(r2,fileRef)
})
