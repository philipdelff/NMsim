path.nonmem <- "/opt/NONMEM/nm75/run/nmfe75" 
path.nonmem <- "/opt/nonmem/nm751/run/nmfe75" 


library(NMdata)
NMdataConf(as.fun="data.table")
library(NMsim)
dt.dos <- NMcreateDoses(AMT=300,TIME=0)
dt.sim <- addEVID2(doses=dt.dos,time.sim=c(1,6,12),CMT=2)
dt.sim[,BBW:=40][,ROW:=.I]


library(data.table)

NMdataConf(dir.psn="/opt/psn")


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
}

test_that("SAEM - known",{

    fileRef <- "testReference/NMsim_05.rds"

    file.mod <- "testData/nonmem/xgxr032.mod"
    NMreadPhi(fnExtension(file.mod,"phi"))[,unique(ID)]

    dt.sim.known <- egdt(dt.sim[,!("ID")],data.table(ID=101:105))
    setorder(dt.sim.known,ID,TIME,EVID,CMT)
    
    set.seed(43)
    simres <- NMsim(file.mod,
                    data=dt.sim.known,
                    text.table="PRED IPRED",
                    dir.sims="testOutput",
                    name.sim="known_01"
                   ,method.sim=NMsim_known
                   ,method.execute="nmsim"
                   ,path.nonmem=path.nonmem
                    )

    simres
    
    expect_equal_to_reference(simres,fileRef)
}
