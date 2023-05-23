library(data.table)
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
    expect_error(
        NMcreateDoses(TIME=c(0),AMT=c(0,10),CMT=2)
    )
})


## II/ADDL should only be applied to last event. addl.lastonly
## argument?
test_that("II/ADDL",{
    fileRef <- "testReference/NMcreateDoses_03.rds"
    res <- NMcreateDoses(TIME=c(0,12),AMT=10,addl=list(II=12,ADDL=3),CMT=2)

    expect_equal_to_reference(res,fileRef)
})

### covariates
test_that("covariates basics",{
    fileRef <- "testReference/NMcreateDoses_04.rds"
    res <- NMcreateDoses(TIME=data.table(regimen=c("SD","MD","MD"),TIME=c(0,0,12)),AMT=10,CMT=1)

    expect_equal_to_reference(res,fileRef)
})


### covariates not spanning same covariate values. Should not be supported for now.
res <- NMcreateDoses(TIME=data.table(regimen=c("SD","MD","MD"),TIME=c(0,0,12)),AMT=10,CMT=1,addl=list(II=12,ADDL=3,regimen="MD"))
res

## covariates not spanning same covariate values. Should not be supported for now.
res <- NMcreateDoses(TIME=data.table(regimen=c("SD","MD","MD"),TIME=c(0,0,12)),AMT=10,CMT=1,addl=data.table(II=12,ADDL=3,regimen="MD"))
res

## This looks OK.
NMcreateDoses(TIME=data.table(regimen=c("SD","MD","MD"),TIME=c(0,0,12)),AMT=10,CMT=1,addl=data.table(II=c(0,0,0,12),ADDL=c(0,0,0,3),regimen=c("SD",rep("MD",3))))


NMcreateDoses(TIME=data.table(regimen=c("SD","MD","MD"),TIME=c(0,0,12)),AMT=data.table(AMT=c(10,5),regimen=c("SD","MD")),CMT=1)


NMcreateDoses(TIME=c(0),AMT=data.table(DOSE=c(10,50),AMT=c(10,50)))

## ID as covariate - not supported
expect_error(
    NMcreateDoses(TIME=c(0),AMT=data.table(ID=c(10,50),AMT=c(10,50)))
)


## not sure what extra this example is showing
## NMcreateDoses(TIME=data.table(ID=c(1,1,2,2,2),TIME=c(0,1,0,1,4)),AMT=data.table(ID=c(1,1,2,2),AMT=c(2,1,4,2)))

## not ok - stop necessary
expect_error(
    NMcreateDoses(TIME=c(0,1,4),AMT=data.table(AMT=c(2,1,4,2)))
)

## ok
NMcreateDoses(TIME=c(0,1,4),AMT=data.table(DOSELEVEL=c(1,1,2,2),AMT=c(2,1,4,2)))

## must return error. AMT is longer than TIME.
### this is a bug. Difference because amt is not unique.
NMcreateDoses(TIME=c(0,1,4),AMT=c(2,1,4,2))
NMcreateDoses(TIME=c(0,1,4),AMT=c(2,1,4,20))
## must return error. AMT is longer than TIME.
NMcreateDoses(TIME=c(0,1,4),AMT=c(2,1,4,2),CMT=2)
NMcreateDoses(TIME=c(0),AMT=c(2,1),CMT=2)


## as.fun
NMcreateDoses(TIME=0,AMT=10)
NMcreateDoses(TIME=0,AMT=10,as.fun=tibble::as_tibble)

}




test_that("data.frames accepted",{
    res.df <- NMcreateDoses(TIME=c(0),AMT=data.frame(DOSE=c(10,50),AMT=c(10,50)))
    res.dt <- NMcreateDoses(TIME=c(0),AMT=data.table(DOSE=c(10,50),AMT=c(10,50)))
    expect_equal(res.df,res.dt)
})
