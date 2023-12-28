

library(NMdata)
NMdataConf(as.fun="data.table")
packageVersion("NMdata")
## library(NMsim)
library(devtools)
load_all()
dt.dos <- NMcreateDoses(AMT=300,TIME=0)
dt.sim <- addEVID2(doses=dt.dos,time.sim=c(1,6,12),CMT=2)
dt.sim[,BBW:=40][,ROW:=.I]

dt.sim.known <- egdt(dt.sim[,!("ID")],data.table(ID=101:105))
setorder(dt.sim.known,ID,TIME,EVID,CMT)



library(data.table)

## NMdataConf(dir.psn="/opt/psn")
path.nonmem <- "/opt/nonmem/nm751/run/nmfe75"
NMdataConf(dir.psn=NULL)
## path.nonmem <- "/opt/NONMEM/nm75/run/nmfe75" 



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
                    text.table="PRED IPRED",
                    dir.sims="testOutput",
                    name.sim="default_01"
                    )

    expect_equal_to_reference(simres,fileRef)

})


test_that("basic - typical",{

    fileRef <- "testReference/NMsim_02.rds"

    file.mod <- "../../tests/testthat/testData/nonmem/xgxr021.mod"
    
    set.seed(43)
    simres <- NMsim(file.mod,
                    data=dt.sim,
                    text.table="PRED IPRED" ,
                    dir.sims="testOutput",
                    method.sim=NMsim_typical,
                    ## method.sim=NMsim_asis,
                    name.sim="typsubj"
                    )

    expect_true(simres[,all(IPRED==PRED)])
    expect_equal_to_reference(simres,fileRef)


})

test_that("basic - known",{

    fileRef <- "testReference/NMsim_03.rds"

    file.mod <- "../../tests/testthat/testData/nonmem/xgxr021.mod"
    
    set.seed(43)
    simres <- NMsim(file.mod,
                    data=dt.sim.known,
                    text.table="PRED IPRED" ,
                    dir.sims="testOutput",
                    method.sim=NMsim_known,
                    name.sim="known_01",
                    method.execute="nmsim",
                    path.nonmem=path.nonmem
                    )

    expect_equal_to_reference(simres,fileRef)
    simres3. <- simres

})



test_that("basic - spaces in paths",{

    fileRef <- "testReference/NMsim_04.rds"

    file.mod <- "testData/nonmem/folder with space/xgxr021.mod"

    set.seed(43)
    simres <- NMsim(file.mod,
                    data=dt.sim,
                    text.table="PRED IPRED",
                    dir.sims="testOutput",
                    name.sim="default_01"
                    )

    expect_equal_to_reference(simres,fileRef)

    file.mod <- "testData/nonmem/folder with space/xgxr021.mod"

    ## using PSN 
    set.seed(43)
    simres <- NMsim(file.mod,
                    data=dt.sim,
                    text.table="PRED IPRED",
                    dir.sims="testOutput",
                    name.sim="default_01"
                    )

    ## no psn
    set.seed(43)
    simres <- NMsim(file.mod,
                    data=dt.sim,
                    text.table="PRED IPRED",
                    dir.sims="testOutput",
                    name.sim="default_01",
                    path.nonmem=path.nonmem,
                    method.update.inits="nmsim"
                    )

    

})


test_that("SAEM - default",{

    fileRef <- "testReference/NMsim_05.rds"

    file.mod <- "testData/nonmem/xgxr032.mod"

    set.seed(43)
    simres <- NMsim(file.mod,
                    data=dt.sim,
                    text.table="PRED IPRED",
                    dir.sims="testOutput",
                    name.sim="default_01"
                    )

    expect_equal_to_reference(simres,fileRef)
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
                      text.table="PRED IPRED",
                      dir.sims="testOutput",
                      name.sim="known_01"
                     ,method.sim=NMsim_known
                     ,method.execute="nmsim"
                     ,path.nonmem=path.nonmem
                      )

    simres.5

    expect_equal_to_reference(simres.5,fileRef)

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


    ## tab.paths <- readRDS("testOutput/xgxr032_datalist_01/NMsim_paths.rds")
    ## simres <- NMscanMultiple(tab.paths$path.sim.lst)
    simres <- NMreadSim("testOutput/xgxr032_datalist_01/NMsim_paths.rds")
})

test_that("multiple data sets on cluster",{
    data.multiple <- split(dt.sim.known,by="ID")
    data.multiple

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

    class(simres.multidata)
    res <- NMreadSim(simres.multidata)


})

test_that("default with renaming",{

    fileRef <- "testReference/NMsim_01.rds"

    file.mod <- "../../tests/testthat/testData/nonmem/xgxr021.mod"


    set.seed(43)
    simres <- NMsim(file.mod=c("ref"=file.mod),
                    data=dt.sim,
                    text.table="PRED IPRED",
                    dir.sims="testOutput",
                    name.sim="default_01"
                    )

    ## expect_equal_to_reference(simres,fileRef)
    
})

test_that("multiple data sets with renaming",{
    data.multiple <- split(dt.sim.known[ID<=103],by="ID")
    data.multiple

    set.seed(43)
    simres.multidata <- NMsim(c(ref=file.mod),
                              data=data.multiple
                             ,table.vars="PRED IPRED Y",
                              dir.sims="testOutput"
                             ,name.sim="datalist_01"
                             ,method.execute="nmsim"
                             ,path.nonmem=path.nonmem
                             ,sge=TRUE
                              )


})

test_that("default with nc>1",{

    ## fileRef <- "testReference/NMsim_01.rds"

    file.mod <- "../../tests/testthat/testData/nonmem/xgxr021.mod"

    set.seed(43)
    simres <- NMsim(file.mod,
                    data=dt.sim,
                    text.table="PRED IPRED",
                    dir.sims="testOutput",
                    name.sim="default_01"
                   ,method.execute="nmsim"
                   ,nc=72
                   ,sge=TRUE
                   ,path.nonmem="/opt/NONMEM/nm75/run/nmfe75"
                    )

    expect_equal_to_reference(simres,fileRef)

})
