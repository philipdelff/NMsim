library(NMdata)
## path.nonmem <- "c:/nm75g64/run/nmfe75.bat"
path.nonmem = "/opt/NONMEM/nm75/run/nmfe75"
## NMdataConf(path.nonmem=path.nonmem)
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
### notice, this returns NULL
    res <- NMexec(file.mod ,method.execute="psn" )

    expect_equal_to_reference(res,fileRef)

    if(F){
        ref <- readRDS(fileRef)
    }
    
})

test_that("method.exec=nmsim",{

    fileRef <- "testReference/NMexec_02.rds"

    file.mod <- "testData/nonmem/xgxr022.mod"

    ## set.seed(43)
    res <- NMexec(file.mod,method.execute = "NMsim")

    expect_equal_to_reference(res,fileRef)

    if(F){
        ref <- readRDS(fileRef)
        ref
    }


})


test_that("default - no sge - method.exec=nmsim",{

    fileRef <- "testReference/NMexec_03.rds"

    file.mod <- "testData/nonmem/xgxr022.mod"

    ## set.seed(43)
    res <- NMexec(file.mod,sge=FALSE  ,path.nonmem=path.nonmem,method.execute = "NMsim")

    expect_equal_to_reference(res,fileRef)

    if(F){
        ref <- readRDS(fileRef)
        ref
    }


})

test_that("default - specify no of threads",{

    fileRef <- "testReference/NMexec_04.rds"

    file.mod <- "testData/nonmem/xgxr022.mod"

    ## set.seed(43)
    res <- NMexec(file.mod,nc=72)

    expect_equal_to_reference(res,fileRef)

})


test_that("multiple $TABLE",{

    fileRef <- "testReference/NMexec_05.rds"

    file.mod <- "testData/nonmem/xgxr032.mod"

    res <- NMexec(file.mod , path.nonmem=path.nonmem,sge=F)

    ext.res <- NMreadExt(file.mod)
    dim.ext <- dim(ext.res)
    expect_equal_to_reference(dim.ext,fileRef)

    if(F){
        ref <- readRDS(fileRef)
    }
    
})
