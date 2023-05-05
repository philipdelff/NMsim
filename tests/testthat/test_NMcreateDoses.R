context("NMscanTables")

test_that("Multiple output table formats",{

    library(data.table)
    library(NMdata)

    fileRef <- "testReference/NMcreateDoses_01.rds"

    ## simple - ok
    NMcreateDoses(TIME=0,AMT=10)
    NMcreateDoses(TIME=0,AMT=10,CMT=2)

    ## expand columns
    NMcreateDoses(TIME=c(0,12),AMT=10,CMT=2)
    ## makes no sense to expand TIME
    NMcreateDoses(TIME=c(0),AMT=c(0,10),CMT=2)

    ## II/ADDL should only be applied to last event. addl.lastonly
    ## argument?
    NMcreateDoses(TIME=c(0,12),AMT=10,addl=list(II=12,ADDL=3),CMT=2)
    
### covariates 
    NMcreateDoses(TIME=c(0),AMT=data.table(DOSE=c(10,50),AMT=c(10,50)))

    ## ID as covariate
    NMcreateDoses(TIME=c(0),AMT=data.table(ID=c(10,50),AMT=c(10,50)))

## not sure what extra this example is givinv
    NMcreateDoses(TIME=data.table(ID=c(1,1,2,2,2),TIME=c(0,1,0,1,4)),AMT=data.table(ID=c(1,1,2,2),AMT=c(2,1,4,2)))

    ## not ok - stop necessary
    NMcreateDoses(TIME=c(0,1,4),AMT=data.table(ID=c(1,1,2,2),AMT=c(2,1,4,2)))

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


})


test_that("data.frames accepted",{
    res.df <- NMcreateDoses(TIME=c(0),AMT=data.frame(DOSE=c(10,50),AMT=c(10,50)))
    res.dt <- NMcreateDoses(TIME=c(0),AMT=data.table(DOSE=c(10,50),AMT=c(10,50)))
    expect_equal(res.df,res.dt)
})
