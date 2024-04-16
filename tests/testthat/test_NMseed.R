context("NMseed")

test_that("basic",{

    fileRef <- "testReference/NMseed_01.rds"

    if(F){
        NMseed(models=data.table(1))
        NMseed(models=data.table(1),nseeds=2)
    }

    set.seed(42)
    res <- NMseed(models=data.table(1:3),nseeds=2,type=c("","UNIFORM"))


    expect_equal_to_reference(res,fileRef)
    

    res <- NMseed(models=data.table(1:3),dist=c("","UNIFORM"))
res    
})
