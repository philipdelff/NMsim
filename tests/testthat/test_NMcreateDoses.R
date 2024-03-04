library(data.table)
data.table::setDTthreads(1)
library(NMdata)

context("NMcreateDoses")

test_that("Basic",{
    fileRef <- "testReference/NMcreateDoses_01.rds"

    ## simple - ok
    res <- NMcreateDoses(TIME=0,AMT=10)
    expect_equal_to_reference(res,fileRef)

})


test_that("custom CMT value",{
    res1 <- NMcreateDoses(TIME=0,AMT=10)
    res2 <- NMcreateDoses(TIME=0,AMT=10,CMT=2)

    res1$CMT <- 2
    expect_equal(res1,res2)
})

test_that("Expand columns",{
    fileRef <- "testReference/NMcreateDoses_02.rds"
    res <- NMcreateDoses(TIME=c(0,12),AMT=10,CMT=2)

    expect_equal_to_reference(res,fileRef)

})

test_that("makes no sense to expand TIME",{
                                        #expect_error(
    NMcreateDoses(TIME=c(0),AMT=c(0,10),CMT=2)
                                        #)
})


test_that("non-unique AMT",{
    ## this is due to a bug that once was where AMT would get shortened because it's non-unique
    fileRef <- "testReference/NMcreateDoses_03.rds"
    res <- NMcreateDoses(TIME=c(0,1,4,5),AMT=c(2,1,4,2),CMT=c(1,1,2))
    expect_equal_to_reference(res,fileRef)
})

## II/ADDL simple example missing


## II/ADDL should only be applied to last event. addl.lastonly
## argument?
test_that("II/ADDL",{
    fileRef <- "testReference/NMcreateDoses_04.rds"
    res <- NMcreateDoses(TIME=c(0,12),AMT=10,addl=list(II=12,ADDL=3),CMT=2)

    expect_equal_to_reference(res,fileRef)
})

### covariates
test_that("covariates basics",{
    fileRef <- "testReference/NMcreateDoses_05.rds"

    res <- NMcreateDoses(TIME=c(0),AMT=data.table(DOSE=c(10,50),AMT=c(10,50)))
    expect_equal_to_reference(res,fileRef)
})


test_that("covariates basics 2",{
    fileRef <- "testReference/NMcreateDoses_06.rds"
    res <- NMcreateDoses(TIME=data.table(regimen=c("SD","MD","MD"),TIME=c(0,0,12)),AMT=10,CMT=1)

    expect_equal_to_reference(res,fileRef)
})


### TODO: covariates not spanning same covariate values. Should not be supported for now.
test_that("covariates not spanning same covariate values",{
    fileRef <- "testReference/NMcreateDoses_07.rds"

    res <- NMcreateDoses(TIME=data.table(regimen=c("SD","MD","MD"),TIME=c(0,0,12)),AMT=10,CMT=1,addl=list(II=12,ADDL=3,regimen="MD"))
    ##res
    expect_equal_to_reference(res,fileRef)
})



test_that("covariates spanning same covariate values",{
    ## This looks OK.
    fileRef <- "testReference/NMcreateDoses_08.rds"

    res <- NMcreateDoses(TIME=data.table(regimen=c("SD","MD","MD"),TIME=c(0,0,12)),AMT=data.table(AMT=c(10,5),regimen=c("SD","MD")),CMT=1)
    expect_equal_to_reference(res,fileRef)
})


test_that("covariates spanning same covariate values - addl",{
    ## This looks OK.
    fileRef <- "testReference/NMcreateDoses_09.rds"

    res <- NMcreateDoses(TIME=data.table(regimen=c("SD","MD","MD"),TIME=c(0,0,12)),AMT=10,CMT=1,addl=data.table(II=c(0,0,0,12),ADDL=c(0,0,0,3),regimen=c("SD",rep("MD",3))))
    expect_equal_to_reference(res,fileRef)
})



test_that("ID as covariate - not supported",{
    expect_error(
        NMcreateDoses(TIME=c(0),AMT=data.table(ID=c(10,50),AMT=c(10,50)))
    )
})


## not sure what extra this example is showing
## NMcreateDoses(TIME=data.table(ID=c(1,1,2,2,2),TIME=c(0,1,0,1,4)),AMT=data.table(ID=c(1,1,2,2),AMT=c(2,1,4,2)))



test_that("Expanding AMT within a covariate",{
    fileRef <- "testReference/NMcreateDoses_10.rds"

    res <- NMcreateDoses(TIME=c(0,1,4),AMT=data.table(DOSELEVEL=c(1,1,2,2),AMT=c(2,1,4,2)))
    expect_equal_to_reference(res,fileRef)
})




test_that("as.fun",{
    fileRef <- "testReference/NMcreateDoses_11.rds"
    res <- NMcreateDoses(TIME=0,AMT=10)
    expect_equal_to_reference(res,fileRef)

    fileRef <- "testReference/NMcreateDoses_12.rds"
    res <- NMcreateDoses(TIME=0,AMT=10,as.fun=as.data.frame)
    expect_equal_to_reference(res,fileRef)
})





test_that("data.frames accepted",{
    res.df <- NMcreateDoses(TIME=c(0),AMT=data.frame(DOSE=c(10,50),AMT=c(10,50)))
    res.dt <- NMcreateDoses(TIME=c(0),AMT=data.table(DOSE=c(10,50),AMT=c(10,50)))
    expect_equal(res.df,res.dt)
})


