setwd("/data/prod_vx708_001_analysis/trunk/analysis/cohortA2")

library(ggplot2)

library(devtools)

load_all("~/wdirs/NMexec")

library(NMdata)
NMdataConf(as.fun="data.table")

NMexec("models/run0155.mod",sge=F)
res1 <- NMscanData("models/run0155.mod")
res1[,.N,by=.(DOSE,AMT)]

doses <- NMcreateDoses(TIME=0,AMT=data.table(AMT=1e5,DOSE=100),RATE=-2)

## sim just one subject
dat.sim <- addEVID2(doses,time.sim=0:24,CMT=2)
dat.sim[,DOSE:=100]
dat.sim

sim1 <- NMsim(path.mod="models/run0155.mod",data=dat.sim,dir.sim="simulations",suffix.sim = "try1")

## Sim multiple subjects
dat.sim2 <- egdt(
    dat.sim[,!("ID")],
    data.table(ID=1:100)
    )
setorder(dat.sim2,ID,TIME,EVID)
dat.sim2[,REC:=.I]

sim2 <- NMsim(path.mod="models/run0155.mod",data=dat.sim2,dir.sim="simulations",suffix.sim = "try2")
dims(sim1,sim2)


ggplot(sim2[EVID==2],aes(TIME,exp(IPRED)))+geom_point()
ggplot(sim2[EVID==2],aes(TIME,exp(IPRED),group=ID))+geom_line()

