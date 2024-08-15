##install.packages("NMsim",repos="https://cloud.r-project.org")
library(data.table)
library(NMdata)

packageVersion("NMdata")
## library(NMsim)
library(devtools)
load_all(export_all=FALSE)

NMdataConf(dir.psn=NULL)
NMdataConf(as.fun="data.table")

dt.dos <- NMcreateDoses(AMT=300,TIME=0)
dt.sim <- addEVID2(doses=dt.dos,time.sim=c(1,6,12),CMT=2)
dt.sim[,BBW:=40][,ROW:=.I]

dt.sim.known <- egdt(dt.sim[,!("ID")],data.table(ID=101:105))
setorder(dt.sim.known,ID,TIME,EVID,CMT)



#### need a function to drop NMsimVersion and NMsimTime from table
fix.time <- function(x){
    meta.x <- attr(x,"NMsimModTab")
    ## meta.x$time.call <- as.POSIXct("2020-02-01 00:01:01",tz="UTC")
    meta.x$NMsimVersion <- NULL
    meta.x$NMsimTime <- NULL
    
    setattr(x,"NMsimModTab",meta.x)
    invisible(x)
}



path.nonmem <- "C:/"

test_that("basic - exec with NM and PSN",{

    fileRef <- "testReference/NMsimWin_01.rds"

    file.mod <- "testData/nonmem/xgxr033.mod"

    set.seed(43)
    simres <- NMsim(file.mod,
                    data=dt.sim,
                    table.vars="PRED IPRED",
                    dir.sims="testOutput",
                    name.sim="default_01"
                    ,path.nonmem=path.nonmem
                    )

    fix.time(simres)
    expect_equal_to_reference(simres,fileRef)


    ## using PSN 
    set.seed(43)
    simres.psn <- NMsim(file.mod,
                        data=dt.sim,
                        text.table="PRED IPRED",
                        dir.sims="testOutput",
                        name.sim="default_01",
                        method.execute="psn"
                        )
    fix.time(simres.psn)

    expect_equal(simres.psn,simres)

})


test_that("basic - update inits with NM and PSN",{

    fileRef <- "testReference/NMsimWin_02.rds"

    file.mod <- "testData/nonmem/xgxr033.mod"

    set.seed(43)
    simres <- NMsim(file.mod,
                    data=dt.sim,
                    table.vars="PRED IPRED",
                    dir.sims="testOutput",
                    name.sim="default_01"
                   ,path.nonmem=path.nonmem
                   ,method.update.inits="nmsim"
                    )

    fix.time(simres)
    expect_equal_to_reference(simres,fileRef)


    ## using PSN to update inits
    set.seed(43)
    simres.psn <-  NMsim(file.mod,
                    data=dt.sim,
                    table.vars="PRED IPRED",
                    dir.sims="testOutput",
                    name.sim="default_01"
                   ,path.nonmem=path.nonmem
                   ,method.update.inits="psn"
                    )

    fix.time(simres.psn)

    expect_equal(simres.psn,simres)

})






test_that("basic - spaces in paths",{

    fileRef <- "testReference/NMsimWin_03.rds"

    file.mod <- "testData/nonmem/folder with space/xgxr021.mod"

    set.seed(43)
    simres <- NMsim(file.mod,
                    data=dt.sim,
                    text.table="PRED IPRED",
                    dir.sims="testOutput",
                    name.sim="default_01"
                    )

    fix.time(simres)
    expect_equal_to_reference(simres,fileRef)

    file.mod <- "testData/nonmem/folder with space/xgxr021.mod"

    ## using PSN 
    set.seed(43)
    simres.psn <- NMsim(file.mod,
                        data=dt.sim,
                        text.table="PRED IPRED",
                        dir.sims="testOutput",
                        name.sim="default_01",
                        method.execute="psn"
                        )

    ## no psn
    set.seed(43)
    simres.nm <- NMsim(file.mod,
                       data=dt.sim,
                       text.table="PRED IPRED",
                       dir.sims="testOutput",
                       name.sim="default_01",
                       path.nonmem=path.nonmem,
                       method.update.inits="nmsim"
                       )

    expect_equal(simres.psn,simres.nm)

})



#### 
## using PSN 
set.seed(43)
simres.psn <- NMsim(file.mod,
                    data=dt.sim,
                    text.table="PRED IPRED",
                    dir.sims="testOutput",
                    name.sim="default_01",
                    method.execute="psn"
                    )

## no psn
set.seed(43)
simres.nm <- NMsim(file.mod,
                   data=dt.sim,
                   text.table="PRED IPRED",
                   dir.sims="testOutput",
                   name.sim="default_01",
                   path.nonmem=path.nonmem,
                   method.update.inits="nmsim"
                   )
