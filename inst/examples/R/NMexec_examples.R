setwd("/data/home/philipde/wdirs/NMexec/inst/examples/")

library(ggplot2)

library(devtools)

script <-  "https://github.com/philipdelff/NMexec"

load_all("~/wdirs/NMexec")
library(NMdata)
NMdataConf(as.fun="data.table")

file.mod <- "nonmem/xgxr014.mod"
file.lst <- fnExtension(file.mod,".lst")
## sge=F to not submit to the cluster
NMexec(file.mod,sge=F)
res1 <- NMscanData(file.mod)

### summarize data/fit
res1[,.N,by=.(DOSE,AMT)]


#### Section start: Simulate a couple of doses ####
dt.amt <- data.table(AMT=c(100,200,400)*1000,DOSE=c(100,200,400))
doses.sd <- NMcreateDoses(TIME=0,AMT=dt.amt)
doses.sd[,dose:=paste(DOSE,"mg")]
doses.sd[,regimen:="SD"]


### multiple dose regimens are easily created with NMcreateDoses too
## Specifying the time points explicitly
dt.amt <- data.table(AMT=c(200,100,800,400)*1000,DOSE=c(100,100,400,400))
doses.md.1 <- NMcreateDoses(TIME=seq(0,by=24,length.out=7),AMT=dt.amt)
doses.md.1[,dose:=paste(DOSE,"mg")]
doses.md.1[,regimen:="QD"]
doses.md.1
## or using ADDL+II
dt.amt <- data.table(AMT=c(200,100,800,400)*1000,DOSE=c(100,100,400,400))
doses.md.2 <- NMcreateDoses(TIME=c(0,24),AMT=dt.amt,addl=data.table(ADDL=c(0,5),II=c(0,24)))
doses.md.2[,dose:=paste(DOSE,"mg")]
doses.md.2[,regimen:="QD"]
doses.md.2

doses.all <- rbind(doses.sd,doses.md.2,fill=T)

load_all("~/wdirs/NMexec")
## sim just one subject per ID
dat.sim.sd <- addEVID2(doses.sd,time.sim=0:24,CMT=2)
dat.sim.md <- addEVID2(doses.md.2,time.sim=0:(24*7),CMT=2)
dat.sim <- rbind(dat.sim.sd,dat.sim.md,fill=TRUE)
dat.sim[,ID:=.GRP,by=.(regimen,ID,DOSE)]
print(dat.sim,topn=5)

dat.sim[,.N,by=.(regimen,DOSE)]

dat.sim[,REC:=NULL]
NMcheckData(dat.sim)


sim1 <- NMsim(path.mod=file.mod,data=dat.sim,dir.sim="simulations",suffix.sim = "try1",seed=2342)

ggplot(sim1,aes(TIME,PRED,colour=dose))+geom_line()+
    labs(x="Hours",y="Concentration (ng/mL)")+
    facet_wrap(~regimen)

###  Section end: Simulate a couple of doses
