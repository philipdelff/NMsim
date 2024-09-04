library(NMdata)
library(NMsim)
path.nonmem <- "c:/nm75g64/run/nmfe75.bat"
path.nonmem = "/opt/NONMEM/nm75/run/nmfe75"
## NMdataConf(path.nonmem=path.nonmem)
library(devtools)
load_all()

## NMdataConf(dir.psn="C:/tools/")
## NMdataConf(dir.psn="C:/Users/Philip Delff/software/PsN-5.3.1/strawberry/perl/site/lib/PsN_5_3_1")
dir.exists("C:/Users/Philip Delff/software/PsN-5.3.1/strawberry/perl/site/lib/PsN_5_3_1")
NMdataConf(dir.psn="C:/Users/Philip Delff/software/PsN-5.3.1/strawberry/perl/bin")
NMsimTestConf()


context("NMexec")

file.inp <- function(...)file.path("../../tests/testthat/testData/nonmem",...)

### psn
test_that("psn, sge",{

    fileRef <- "testReference/NMexec_01.rds"

    ## file.copy(file.inp("xgxr021.mod"),"testOutput/")
    ## file.mod <- "testOutput/xgxr021.mod"
    file.mod <- "testData/nonmem/xgxr022.mod"

    ## set.seed(43)
### notice, this returns NULL
    ### with sge
    res <- NMexec(file.mod ,method.execute="psn" )

    expect_equal_to_reference(res,fileRef)

    if(F){
        ref <- readRDS(fileRef)
    }
    
})


test_that("psn, no sge",{

    fileRef <- "testReference/NMexec_01b.rds"

    ## file.copy(file.inp("xgxr021.mod"),"testOutput/")
    ## file.mod <- "testOutput/xgxr021.mod"
    file.mod <- "testData/nonmem/xgxr022.mod"

    ## set.seed(43)
### notice, this returns NULL
    ### with sge
    res <- NMexec(file.mod ,method.execute="psn" ,sge=FALSE)

    expect_equal_to_reference(res,fileRef)

    if(F){
        ref <- readRDS(fileRef)
    }
    
})


test_that("method.exec=nmsim, sge=FALSE",{

    fileRef <- "testReference/NMexec_02.rds"

    file.mod <- "testData/nonmem/xgxr022.mod"

    ## set.seed(43)
    res <- NMexec(file.mod,method.execute = "NMsim",sge=F)

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
    res <- NMexec(file.mod,sge=FALSE  ,path.nonmem=path.nonmem,method.execute = "NMsim",wait=T)
    
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


test_that("multiple $TABLE on sge",{

    fileRef <- "testReference/NMexec_05.rds"

    file.mod <- "testData/nonmem/xgxr032.mod"

    res <- NMexec(file.mod , path.nonmem=path.nonmem,sge=T)

    ext.res <- NMreadExt(file.mod)
    dim.ext <- dim(ext.res)
    expect_equal_to_reference(dim.ext,fileRef)

    if(F){
        ref <- readRDS(fileRef)
    }
    
})

test_that("multiple $TABLE",{

    fileRef <- "testReference/NMexec_05.rds"

    file.mod <- "testData/nonmem/xgxr032.mod"

    res <- NMexec(file.mod , path.nonmem=path.nonmem,sge=F,clean=5)

    ext.res <- NMreadExt(file.mod)
    dim.ext <- dim(ext.res)
    expect_equal_to_reference(dim.ext,fileRef)

    if(F){
        ref <- readRDS(fileRef)
    }
    
})
