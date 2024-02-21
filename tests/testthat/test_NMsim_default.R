if(F){
    ## remember setwd()
    

    NMdataConf(as.fun="data.table")
    library(data.table)

    dt.amt <- data.table(DOSE=c(100,400))
    dt.amt[,AMT:=DOSE*1000]
    dt.amt
    doses.sd <- NMcreateDoses(TIME=0,AMT=dt.amt)
    doses.sd[,dose:=paste(DOSE,"mg")]
    doses.sd[,regimen:="SD"]
    doses.sd


    dat.sim.sd <- addEVID2(doses.sd,time.sim=0:24,CMT=2)
    dat.sim <- copy(dat.sim.sd)

    dat.sim[,ROW:=.I]
    NMcheckData(dat.sim)

    

    head(dat.sim)

    dat.sim[,BBW:=75]

    file.mod <- "testData/nonmem/xgxr025.mod"
    sim1 <- NMsim(path.mod=file.mod,data=dat.sim,
                  dir.sim="testOutput",suffix.sim = "sd1",
                  seed=2342)
    sim1


    load_all("~/wdirs/NMsim")
    sim.n10 <- NMsim(path.mod=file.mod,data=dat.sim,
                     dir.sim="testOutput",suffix.sim = "sd1",
                     nsims=10,
                     seed=2342)
    sim.n10

}
