##install.packages("NMsim",repos="https://cloud.r-project.org")
library(data.table)
library(NMdata)

packageVersion("NMdata")
## library(NMsim)
library(devtools)
load_all(export_all=FALSE)

NMdataConf(dir.psn=NULL)
NMdataConf(as.fun="data.table")
NMdataConf(dir.sims="testData/simtmp")
NMdataConf(dir.res="testData/simres")

dt.dos <- NMcreateDoses(AMT=300,TIME=0)
dt.sim <- addEVID2(doses=dt.dos,time.sim=c(1,6,12),CMT=2)
dt.sim[,BBW:=40][,ROW:=.I]

dt.sim.known <- egdt(dt.sim[,!("ID")],data.table(ID=101:105))
setorder(dt.sim.known,ID,TIME,EVID,CMT)


## NMdataConf(dir.psn="/opt/psn")
path.nonmem <- "/opt/nonmem/nm751/run/nmfe75"
##
path.nonmem <- "/opt/NONMEM/nm75/run/nmfe75" 
file.exists(path.nonmem)

#### need a function to drop NMsimVersion and NMsimTime from table
fix.time <- function(x){
    meta.x <- attr(x,"NMsimModTab")
    ## meta.x$time.call <- as.POSIXct("2020-02-01 00:01:01",tz="UTC")
    meta.x$NMsimVersion <- NULL
    meta.x$NMsimTime <- NULL
    
    setattr(x,"NMsimModTab",meta.x)
    invisible(x)
}


#### get rid of ROWMODEL2. Not needed for NMreadSim.


context("NMsim")
test_that("basic - default",{
    
    fileRef <- "testReference/NMsim_01.rds"

    ## 025 doesn't seem stable. Got Q~1e7 and Nonmem didn't run
    file.mod <- "../../tests/testthat/testData/nonmem/xgxr021.mod"
    ## NMdata:::NMreadExt( fnExtension(file.mod,"ext"))
    ## library(nonmem2R)
    ## extload(file.mod)

    set.seed(43)
    simres <- NMsim(file.mod,
                    data=dt.sim,
                    table.var="PRED IPRED",
                    dir.sims="testOutput",
                    name.sim="default_01"
                    )
    
    ## attributes(NMreadSim("testOutput/NMsim_xgxr021_default_01_paths.rds"))
    fix.time(simres)
    expect_equal_to_reference(simres,fileRef)

    if(F){
        compareCols(attributes(simres)$NMsimModTab,attributes(readRDS(fileRef))$NMsimModTab,keep.names=FALSE)
        simres.nometa <- copy(simres)
        unNMsimRes(simres.nometa)
        attributes(simres.nometa)
        expect_equal_to_reference(simres.nometa,fnAppend(fileRef,"noMeta"))
    }

})


test_that("basic - sge - dont wait",{

### using the same reference in test 1. Only diff is using sge=TRUE
    fileRef <- "testReference/NMsim_01.rds"

    file.mod <- "../../tests/testthat/testData/nonmem/xgxr021.mod"

    set.seed(43)
    ## simtab <- "testOutput/NMsim_xgxr021_default_01_paths.rds"
    ## if(doSims){
    simtab <- NMsim(file.mod,
                    data=dt.sim,
                    table.vars="PRED IPRED",
                    dir.sims="testOutput",
                    name.sim="default_01"
                   ,sge=TRUE
                    ##,reuse.results=TRUE
                    ## ,file.res=simtab
                    )
    ## }
    simres2 <- NMreadSim(simtab,wait=T)

    fix.time(simres2)
    
    expect_equal_to_reference(simres2,fileRef)


    if(F){
        ref <- readRDS(fileRef)
        compareCols(simres2,ref)

        compareCols(attributes(simres2)$NMsimModTab,
                    attributes(ref)$NMsimModTab)
    }

    
})

test_that("basic - sge - wait",{

### using the same reference in test 1. Only diff is using sge=TRUE and wait=TRUE
    fileRef <- "testReference/NMsim_01.rds"

    file.mod <- "../../tests/testthat/testData/nonmem/xgxr021.mod"

    set.seed(43)
    ## simtab <- "testOutput/NMsim_xgxr021_default_01_paths.rds"
    ## if(doSims){
    simres3 <- NMsim(file.mod,
                     data=dt.sim,
                     table.vars="PRED IPRED",
                     dir.sims="testOutput",
                     name.sim="default_01"
                    ,sge=TRUE
                    ,wait=TRUE
                    ,reuse.results=FALSE
                     ## ,file.res=simtab
                     )
    ## }
    ##simres2 <- NMreadSim(simtab,wait=T)

    fix.time(simres3)
    
    expect_equal_to_reference(simres3,fileRef)

})


test_that("basic - typical",{

    fileRef <- "testReference/NMsim_02.rds"

    file.mod <- "../../tests/testthat/testData/nonmem/xgxr021.mod"
    
    set.seed(43)
    simres <- NMsim(file.mod,
                    data=dt.sim,
                    table.vars="PRED IPRED" ,
                    dir.sims="testOutput",
                    method.sim=NMsim_typical,
                    ## method.sim=NMsim_asis,
                    name.sim="typsubj"
                    )

    expect_true(simres[,all(IPRED==PRED)])

    fix.time(simres)
    expect_equal_to_reference(simres,fileRef)


    if(F){
        ref <- readRDS(fileRef)
        compareCols(simres,ref)

        compareCols(attributes(simres)$NMsimModTab,
                    attributes(ref)$NMsimModTab)
    }

})

test_that("basic - known",{

    fileRef <- "testReference/NMsim_03.rds"

    file.mod <- "../../tests/testthat/testData/nonmem/xgxr021.mod"
    
    set.seed(43)
    simres <- NMsim(file.mod,
                    data=dt.sim.known,
                    table.vars="PRED IPRED" ,
                    dir.sims="testOutput",
                    method.sim=NMsim_known,
                    name.sim="known_01",
                    method.execute="nmsim",
                    path.nonmem=path.nonmem
                    )

    fix.time(simres)
    expect_equal_to_reference(simres,fileRef)


    if(F){
        ref <- readRDS(fileRef)
        compareCols(simres,ref)

        compareCols(attributes(simres)$NMsimModTab,
                    attributes(ref)$NMsimModTab)
    }

    
})



test_that("basic - spaces in paths",{

    fileRef <- "testReference/NMsim_04.rds"

    file.mod <- "testData/nonmem/folder with space/xgxr021.mod"

    set.seed(43)
    simres <- NMsim(file.mod,
                    data=dt.sim,
                    table.vars="PRED IPRED",
                    dir.sims="testOutput",
                    name.sim="default_01"
                    )

    unNMsimRes(simres)
    expect_equal_to_reference(simres,fileRef)

    file.mod <- "testData/nonmem/folder with space/xgxr021.mod"

    ## using PSN 
    set.seed(43)
    simres.psn <- NMsim(file.mod,
                        data=dt.sim,
                        table.vars="PRED IPRED",
                        dir.sims="testOutput",
                        name.sim="default_01",
                        method.execute="psn"
                        )

    ## no psn
    set.seed(43)
    simres.nm <- NMsim(file.mod,
                       data=dt.sim,
                       table.vars="PRED IPRED",
                       dir.sims="testOutput",
                       name.sim="default_01",
                       path.nonmem=path.nonmem,
                       method.update.inits="nmsim"
                       )

    expect_equal(simres.psn,simres.nm)

    if(F){
        compareCols(simres.psn,simres.nm)
    }
})


test_that("SAEM - default",{

    fileRef <- "testReference/NMsim_05.rds"

    file.mod <- "testData/nonmem/xgxr032.mod"

    set.seed(43)
    simres <- NMsim(file.mod,
                    data=dt.sim,
                    table.vars="PRED IPRED",
                    dir.sims="testOutput",
                    name.sim="default_01"
                    )

    fix.time(simres)
    expect_equal_to_reference(simres,fileRef)

    
    if(F){
        ref <- readRDS(fileRef)
        compareCols(simres,ref)

        compareCols(attributes(simres)$NMsimModTab,
                    attributes(ref)$NMsimModTab)
    }


})


test_that("SAEM - known",{

    fileRef <- "testReference/NMsim_06.rds"

    file.mod <- "testData/nonmem/xgxr032.mod"
    ## NMreadPhi(fnExtension(file.mod,"phi"))[,unique(ID)]

    ## source("~/wdirs/NMsim/devel/genPhiFile.R")
    ## res <- NMscanData(file.mod)
    ## genPhiFile(res,file="testOutput/tmp_etas.phi")
    
    
    set.seed(43)
    simres.5 <- NMsim(file.mod,
                      data=dt.sim.known,
                      table.vars="PRED IPRED",
                      dir.sims="testOutput",
                      name.sim="known_01"
                     ,method.sim=NMsim_known
                     ,method.execute="nmsim"
                     ,path.nonmem=path.nonmem
                      )

    ## simres.5
    fix.time(simres.5)
    expect_equal_to_reference(simres.5,fileRef)
    

    if(F){
        ref <- readRDS(fileRef)
        compareCols(simres,ref)

        compareCols(attributes(simres)$NMsimModTab,
                    attributes(ref)$NMsimModTab)
    }


})



## rbind(simres.3,simres.5) |>
## ggplot(aes(TIME,IPRED,colour=model))+geom_line()+
## facet_wrap(~ID,scales="free")


test_that("VPC",{

    
    file.mod <- "testData/nonmem/xgxr032.mod"
    nsims <- 10
    
    set.seed(43)
    simres.vpc <- NMsim(file.mod,
                        table.vars="PRED IPRED Y",
                        dir.sims="testOutput",
                        name.sim="vpc_01"
                       ,nsims=nsims
                       ,method.execute="nmsim"
                       ,path.nonmem=path.nonmem
                        )

    ## library(ggplot2)
    
    expect_equal(length(unique(simres.vpc$model)),nsims)

    expect_equal(nrow(simres.vpc),nsims*731)    

    ## derive PIs
    expect_equal(as.numeric(simres.vpc[EVID==0,quantile(Y,probs=.25)]),0.15568)

})


test_that("VPC with complicated INPUT",{

    
    file.mod <- "testData/nonmem/xgxr033.mod"
    nsims <- 2
    ## NMexec(file.mod,sge=FALSE)
    
    set.seed(43)
    simres.vpc <- NMsim(file.mod,
                        table.vars="PRED IPRED Y",
                        dir.sims="testOutput",
                        name.sim="vpc_01"
                       ,nsims=nsims
                       ,method.execute="nmsim"
                       ,path.nonmem=path.nonmem
                        )

    ## library(ggplot2)
    ## dims(simres.vpc)
    expect_equal(length(unique(simres.vpc$model)),nsims)

    expect_equal(nrow(simres.vpc),nsims*731)    


})


test_that("multiple data sets",{

    fileRef <- "testReference/NMsim_07.rds"
    file.mod <- "testData/nonmem/xgxr032.mod"
    data.multiple <- split(dt.sim.known,by="ID")
    ## data.multiple

    set.seed(43)
    simres.multidata <- NMsim(file.mod,
                              data=data.multiple
                             ,table.vars="PRED IPRED Y",
                              dir.sims="testOutput"
                             ,name.sim="datalist_01"
                             ,method.execute="nmsim"
                             ,path.nonmem=path.nonmem
                              )

    expect_equal(nrow(simres.multidata),nrow(dt.sim.known))


    
})

test_that("list of data sets - spaces in data names",{

    file.mod <- "testData/nonmem/xgxr032.mod"
    data.multiple <- split(dt.sim.known,by="ID")
    names(data.multiple) <- paste("1",names(data.multiple))

    set.seed(43)
    simres.multidata <- NMsim(file.mod,
                              data=data.multiple
                             ,table.vars="PRED IPRED Y",
                              dir.sims="testOutput"
                             ,name.sim="datalist_02"
                             ,method.execute="nmsim"
                             ,path.nonmem=path.nonmem
                              )

    expect_equal(nrow(simres.multidata),nrow(dt.sim.known))


    
})


test_that("multiple data sets on cluster",{
    file.mod <- "testData/nonmem/xgxr032.mod"
    data.multiple <- split(dt.sim.known,by="ID")
    
    set.seed(43)
    simres.multidata <- NMsim(file.mod,
                              data=data.multiple
                             ,table.vars="PRED IPRED Y",
                              dir.sims="testOutput"
                             ,name.sim="datalist_01"
                             ,method.execute="nmsim"
                             ,path.nonmem=path.nonmem
                             ,sge=TRUE
                              )

    list.files("testOutput/xgxr032_datalist_01")
    ## NMreadSim("testOutput/xgxr032_datalist_01/NMsim_paths.rds")
    class(simres.multidata)
    res <- NMreadSim(simres.multidata,wait=T)

    expect_equal(nrow(res),nrow(dt.sim.known))
    

})

test_that("default with renaming",{

    fileRef <- "testReference/NMsim_11.rds"

    file.mod <- "../../tests/testthat/testData/nonmem/xgxr021.mod"


    set.seed(43)
    simres <- NMsim(file.mod=c("ref"=file.mod),
                    data=dt.sim,
                    table.vars="PRED IPRED",
                    dir.sims="testOutput",
                    name.sim="default_01"
                    )

    expect_equal(unique(simres[,model]),"NMsim_ref_default_01")
    
    fix.time(simres)
    expect_equal_to_reference(simres,fileRef)
    


    if(F){
        ref <- readRDS(fileRef)
        compareCols(simres,ref)

        compareCols(attributes(simres)$NMsimModTab,
                    attributes(ref)$NMsimModTab)
    }


})

test_that("multiple data sets with renaming",{
    file.mod <- "../../tests/testthat/testData/nonmem/xgxr021.mod"
    data.multiple <- split(dt.sim.known[ID<=103],by="ID")
    ## data.multiple

    set.seed(43)
    simres.multidata <- NMsim(c(ref=file.mod),
                              data=data.multiple
                             ,table.vars="PRED IPRED Y",
                              dir.sims="testOutput"
                             ,name.sim="datalist_01"
                             ,method.execute="nmsim"
                             ,path.nonmem=path.nonmem
                             ,sge=TRUE
                             ,wait=T
                              )

    expect_equal(unique(simres.multidata[,model]),paste("NMsim_ref_datalist_01",101:103,sep="_"))
    
    expect_equal(nrow(simres.multidata),nrow(dt.sim.known[ID<=103]))
    
})

test_that("default with nc>1",{

    ## fileRef <- "testReference/NMsim_08.rds"

    
    file.mod <- "../../tests/testthat/testData/nonmem/xgxr021.mod"

    set.seed(43)
    expect_warning(
        simtab <- NMsim(file.mod,
                        data=dt.sim,
                        table.vars="PRED IPRED",
                        dir.sims="testOutput",
                        name.sim="default_nc"
                       ,method.execute="nmsim"
                       ,nc=2
                       ,sge=TRUE
                       ,path.nonmem="/opt/NONMEM/nm75/run/nmfe75"
                        )
    )
### last time I checked, this didnt work
    expect_error(simres <- NMreadSim(simtab,wait=T))
    ## expect_equal(
    ##     nrow(
    ##     simres
    ##     ),nrow(dt.sim)
    ## )
    
    ## expect_equal_to_reference(simres,fileRef)

})


test_that("transform",{
    ## options(warn=2)
    NMdataConf(reset=TRUE)
    NMdataConf(dir.res=NULL,allow.unknown=TRUE)
    NMdataConf(dir.sims=NULL,allow.unknown=TRUE)
    NMdataConf(dir.psn=NULL)
    NMdataConf(as.fun="data.table")

    fileRef <- "testReference/NMsim_09.rds"

    file.mod <- "../../tests/testthat/testData/nonmem/xgxr021.mod"

    set.seed(43)
    simres <- NMsim(file.mod,
                    data=dt.sim,
                    table.var="PRED IPRED",
                    dir.sims="testOutput",
                    name.sim="default_trans"
                   ,transform=list(IPRED=sqrt,PRED=function(x)x*1000)
                    )


    ## simres <- NMreadSim("testOutput/NMsim_xgxr021_default_trans_paths.rds")
    
    fix.time(simres)

    ## simres[,funs.transform:=NULL]
    meta.x <- attr(simres,"NMsimModTab")
    meta.x$funs.transform <- "removed"
    setattr(simres,"NMsimModTab",meta.x)

    ##     unlink(fileRef)
    expect_equal_to_reference(simres,fileRef)
    

})

test_that("dir.sims and dir.res with NMdataConf",{

    NMdataConf(dir.sims="testOutput/NMdataConfSim",
               dir.res="testOutput/NMdataConfRes",
               allow.unknown=TRUE)
    
    fileRef <- "testReference/NMsim_10.rds"

    file.mod <- "../../tests/testthat/testData/nonmem/xgxr021.mod"
    
    set.seed(43)
    simres <- NMsim(file.mod,
                    data=dt.sim,
                    table.var="PRED IPRED",
                    name.sim="default_dirconf"
                    )

    fix.time(simres)
    
    expect_equal_to_reference(simres,fileRef)

    NMdataConf(dir.sims=NULL,
               dir.res=NULL,
               allow.unknown=TRUE)

})


test_that("basic - a model that fails on NMTRAN",{
    
    ## fileRef <- "testReference/NMsim_01.rds"

    file.mod <- "../../tests/testthat/testData/nonmem/xgxr021.mod"

    dt.dos <- NMcreateDoses(AMT=300,TIME=0)
    dt.sim <- addEVID2(doses=dt.dos,time.sim=c(1,6,12),CMT=2)

    set.seed(43)
    expect_error(
        simres <- NMsim(file.mod,
                        data=dt.sim,
                        ## table.var="PRED IPRED",
                        dir.sims="testOutput",
                        name.sim="nmtranfail"
                       ,sge=F
                       ,wait=TRUE
                        )
    )
    ## expect_error(
    ##     NMreadSim(simres)
    ## )

})


test_that("Two models on one rds",{

    file.mod.1 <- "../../tests/testthat/testData/nonmem/xgxr021.mod"
    file.mod.2 <- "testData/nonmem/xgxr032.mod"

    dt.dos <- NMcreateDoses(AMT=300,TIME=0)
    dt.sim <- addEVID2(doses=dt.dos,time.sim=c(1,6,12),CMT=2)

    set.seed(43)
    simres <- NMsim(c(file.mod.1,file.mod.2),
                    data=dt.sim,
                    ## table.var="PRED IPRED",
                    dir.sims="testOutput",
                    name.sim="twomodels_01"
                   ,file.res="testOutput/twomodels_01_paths.rds"
                   ,table.vars=cc(PRED,IPRED)
                   ,sge=F
                   ,wait=TRUE
                    )

    ## readRDS("testOutput/twomodels_01_paths.rds")
    res <- NMreadSim("testOutput/twomodels_01_paths.rds")

    fix.time(simres)
    fix.time(res)
    
    expect_equal(simres,res)
    
})
