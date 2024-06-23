library(NMdata)
NMdataConf(path.nonmem="c:/nm75g64/run/nmfe75.bat")
library(devtools)
load_all()

context("NMexec")

file.inp <- function(...)file.path("../../tests/testthat/testData/nonmem",...)

### psn

test_that("default",{

    fileRef <- "testReference/NMexec_01.rds"

    ## file.copy(file.inp("xgxr021.mod"),"testOutput/")
    ## file.mod <- "testOutput/xgxr021.mod"
    file.mod <- "testData/nonmem/xgxr022.mod"

    ## set.seed(43)
    res <- NMexec(file.mod  )

    expect_equal_to_reference(simres,fileRef)

})


test_that("default - no sge",{

    fileRef <- "testReference/NMexec_02.rds"

    file.mod <- "testData/nonmem/xgxr022.mod"

    ## set.seed(43)
    res <- NMexec(file.mod,sge=FALSE  ,path.nonmem="c:/nm75g64/run/nmfe75.bat",method.execute = "NMsim")

    expect_equal_to_reference(simres,fileRef)

})

test_that("default - specify no of threads",{

    fileRef <- "testReference/NMexec_03.rds"

    file.mod <- "testData/nonmem/xgxr022.mod"

    ## set.seed(43)
    res <- NMexec(file.mod,nc=72)

    expect_equal_to_reference(simres,fileRef)

})
