context("NMseed")

test_that("basic",{

    fileRef <- "testReference/NMseed_01.rds"

    if(F){
        NMseed(models=data.table(1))
        NMseed(models=data.table(1),nseeds=2)
    }

    set.seed(42)
    res1 <- NMseed(models=data.table(1:3),nseeds=2,dist=c("","UNIFORM"))


    expect_equal_to_reference(res1,fileRef)
    
    set.seed(42)
    res2 <- NMseed(models=data.table(1:3),dist=c("","UNIFORM"))

    expect_identical(res1,res2)
})


test_that("manually provided values",{

    fileRef <- "testReference/NMseed_02.rds"


    set.seed(42)
    res1 <- NMseed(models=data.table(1:3),dist=c("","UNIFORM"),values=data.table(v1=1:3,v2=5:7))


    expect_equal_to_reference(res1,fileRef)
       
})

test_that("Only one value per source",{

    fileRef <- "testReference/NMseed_03.rds"


    set.seed(42)
    res1 <- NMseed(models=data.table(1:3),dist=c("","UNIFORM"),values=data.table(v1=1,v2=7))


    expect_equal_to_reference(res1,fileRef)
       
})


