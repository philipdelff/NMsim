context("simPopEtas")
library(data.table)
data.table::setDTthreads(1) 
library(NMdata)

NMdataConf(reset=T)
test_that("Basic",{
    fileRef <- "testReference/simPopEtas_01.rds"
    ## file.mod <- "../../inst/examples/nonmem/xgxr022.mod"
    file.mod <- "testData/nonmem/xgxr025.mod"
    res <- simPopEtas(file.mod,N=10,seed=4)

    expect_equal_to_reference(res,fileRef)

})


