## library(devtools)
## load_all()

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
                    dir.sims="testOutput",
                    dir.res="testData/simres",
                    name.sim="default_01"
                    )

}

if(F){
test_that("Basic",{

    fileRef <- "testReference/rbind_NMsimRes_01.rds"
    res1 <- NMreadSim("testData/simres/xgxr021_default_01_MetaData.rds")

    res1 <- res1[,rep:=1]
    res2 <- copy(res1)[,rep:=2]
    class(res1)
    r2 <- rbind.NMsimRes(res1,res2)
    fix.time(r2)
    
    expect_equal_to_reference(r2,fileRef)

    if(F){
        ref <- readRDS(fileRef)
        compareCols(r2,ref)

        compareCols(attributes(r2)$NMsimModTab,
                    attributes(ref)$NMsimModTab,keep.names=FALSE)
    }

})
}
