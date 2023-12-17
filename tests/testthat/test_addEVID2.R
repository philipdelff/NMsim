context("addEVID2")
library(data.table)
data.table::setDTthreads(1) 
library(NMdata)

NMdataConf(reset=T)
test_that("Basic",{
    fileRef <- "testReference/addEVID2_01.rds"

    df.doses <- NMcreateDoses(TIME=c(0,12),AMT=10,CMT=2)
    seq.time <- c(0,4,12,24)

    res <- addEVID2(df.doses,time.sim=seq.time,CMT=2)

    df.doses
    res

    expect_equal_to_reference(res,fileRef)

})


test_that("Multiple compartments",{
    fileRef <- "testReference/addEVID2_02.rds"

    dt.doses <- NMcreateDoses(TIME=c(0,12),AMT=10,CMT=1)
    seq.time <- c(0,4,12,24)

    res <- addEVID2(dt.doses,time.sim=seq.time,CMT=c(2,3))

    ## dt.doses
    ## res
    ## readRDS(fileRef)
    expect_equal_to_reference(res,fileRef)

})



test_that("compartments with covariates",{
    fileRef <- "testReference/addEVID2_03.rds"

    dt.doses <- NMcreateDoses(TIME=c(0,12),AMT=10,CMT=1)
    seq.time <- c(0,4,12,24)
    dt.cmt <- data.table(CMT=c(2,3),analyte=c("parent","metabolite"))
    
    res <- addEVID2(dt.doses,time.sim=seq.time,CMT=dt.cmt)

    ## res
    ## readRDS(fileRef)

    expect_equal_to_reference(res,fileRef)

})



test_that("data.frame CMT",{
    fileRef <- "testReference/addEVID2_04.rds"

    df.doses <- NMcreateDoses(TIME=c(0,12),AMT=10,CMT=1,as.fun=as.data.frame)
    seq.time <- c(0,4,12,24)

    res <- addEVID2(df.doses,time.sim=seq.time,CMT=c(2,3),as.fun=as.data.frame)

    df.doses
    res
    ## readRDS(fileRef)
    
    expect_equal_to_reference(res,fileRef)

})


test_that("time with covariates",{
    fileRef <- "testReference/addEVID2_05.rds"

    dt.doses <- NMcreateDoses(TIME=data.table(regimen=c("SD","MD","MD"),TIME=c(0,0,12)),AMT=10,CMT=1)

    seq.time.sd <- data.table(regimen="SD",TIME=seq(0,6))
    seq.time.md <- data.table(regimen="MD",TIME=c(0,4,12,24))
    seq.time <- rbind(seq.time.sd,seq.time.md)
    
    res <- addEVID2(dt.doses,time.sim=seq.time,CMT=2)

    ## dt.doses
    ## res
    
    expect_equal_to_reference(res,fileRef)

})
