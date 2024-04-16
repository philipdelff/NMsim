context("rbind.NMsimRes")

test_that("basic",{
    ## we need some sim results

    sim1 <- NMreadSim("testOutput/NMsim_xgxr021_sd1_NMreadSim_paths.rds")
    sim2 <- copy(sim1)

    class(sim1)

    allsim <- rbind(sim1,sim2)

    class(allsim)
    expect_equal(nrow(allsim),nrow(sim1)+nrow(sim2))

})
