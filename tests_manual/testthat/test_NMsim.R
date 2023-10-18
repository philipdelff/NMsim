path.nonmem <- "/opt/NONMEM/nm75/run/nmfe75" 


library(NMdata)
NMdataConf(as.fun="data.table")
library(NMsim)
dt.dos <- NMcreateDoses(AMT=300,TIME=0)
dt.sim <- addEVID2(doses=dt.dos,time.sim=c(1,6,12),CMT=2)
dt.sim[,BBW:=40][,ROW:=.I]


context("NMsim")
test_that("basic - default",{

    fileRef <- "testReference/NMsim_01.rds"

    file.mod <- "../../tests/testthat/testData/nonmem/xgxr025.mod"

    set.seed(43)
    simres <- NMsim(file.mod,data=dt.sim,
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

    file.mod <- "../../tests/testthat/testData/nonmem/folder with space/xgxr025.mod"

    set.seed(43)
    simres <- NMsim(file.mod,
                    data=dt.sim,
                    text.table="PRED IPRED",
                    dir.sims="testOutput",
                    name.sim="default_01"
                    )

    expect_equal_to_reference(simres,fileRef)

    file.mod <- "../../tests/testthat/testData/nonmem/folder with space/xgxr021.mod"

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
