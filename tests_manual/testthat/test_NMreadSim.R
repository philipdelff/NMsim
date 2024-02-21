## setwd("~/wdirs/NMsim/tests_manual/testthat/")
res1 <- NMreadSim("testOutput/NMsim_xgxr021_default_01_paths.rds")

setwd("..")
res2 <- NMreadSim("testthat/testOutput/NMsim_xgxr021_default_01_paths.rds")    

expect_equal(res1,res2)
setwd("testthat/")
