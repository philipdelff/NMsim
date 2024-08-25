### so far disabled because test relies on NMdata 0.1.7
if(F){
    dt.amt <- data.table(DOSE=c(100,400))
    dt.amt[,AMT:=DOSE*1000]
    dt.amt
    doses.sd <- NMcreateDoses(TIME=0,AMT=dt.amt,as.fun="data.table")
    doses.sd[,dose:=paste(DOSE,"mg")]
    doses.sd[,regimen:="SD"]


    dat.sim.sd <- addEVID2(doses.sd,time.sim=0:24,CMT=2,as.fun="data.table")
    dat.sim <- copy(dat.sim.sd)

    ## NMcheckData(dat.sim)

    dat.sim[,ROW:=.I]

    head(dat.sim)

    dat.sim[,BBW:=75]


    test_that("NMsim_EBE",{

        fileRef <- "testReference/NMsim_NWPRI_01.rds"
        
        file.mod <- "testData/nonmem/xgxr032.mod"


        sim1 <- NMsim(file.mod=file.mod,
                      data=dat.sim,
                      dir.sim="testOutput",
                      name.sim = "sd1_NWPRI",
                      method.sim=NMsim_NWPRI,
                      seed.nm=2342,
                      execute=FALSE,
                      method.update.inits="nmsim")

        mod <- NMreadSection("testOutput/xgxr032_sd1_NWPRI/xgxr032_sd1_NWPRI.mod")
        

        ## ref <- readRDS(fileRef)
        expect_equal_to_reference(mod,fileRef)

        if(F){
            ref <- readRDS(fileRef)
            ref$OMEGA
            mod$OMEGA 
            ref$SIGMA
            mod$SIGMA
            ref$SIMULATION
            mod$SIMULATION
        }


    })
}
