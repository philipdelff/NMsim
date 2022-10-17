setwd("/data/home/philipde/wdirs/NMexec/inst/examples/")

library(ggplot2)
library(devtools)
library(data.table)

script <-  "https://github.com/philipdelff/NMexec/tree/main/inst/examples/R/NMexec_examples.R"

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


## sim just one subject per ID
dat.sim.sd <- addEVID2(doses.sd,time.sim=0:24,CMT=2)
dat.sim.md <- addEVID2(doses.md.2,time.sim=0:(24*7),CMT=2)
dat.sim1 <- rbind(dat.sim.sd,dat.sim.md,fill=TRUE)
dat.sim1[,ID:=.GRP,by=.(regimen,ID,DOSE)]
print(dat.sim1,topn=5)

dat.sim1[,.N,by=.(regimen,DOSE)]


NMcheckData(dat.sim1)


### Remember, if Nonmem needs a column somewhere in the control
### stream, we will need to provide it.
sim1 <- NMsim(path.mod=file.mod,data=dat.sim1,dir.sim="simulations",suffix.sim = "try1"
             ,seed=2342)
## in this case, it's just the row identifier, so we just create
## one. A typical example would have been a covariate.
dat.sim1[,ROW:=.I]
sim1 <- NMsim(path.mod=file.mod,data=dat.sim1,dir.sim="simulations",suffix.sim = "try1"
             ,seed=2342)


ggplot(sim1,aes(TIME,PRED,colour=dose))+geom_line()+
    labs(x="Hours",y="Concentration (ng/mL)")+
    facet_wrap(~regimen,scales="free")


### NMsim can reuse sim results if found
sim1.reused <- NMsim(path.mod=file.mod,data=dat.sim1,dir.sim="simulations",suffix.sim = "try1"
             ,seed=2342
             ,reuse.results=TRUE)


###  Section end: Simulate a couple of doses

#### Section start: Sim multiple subjects by adding them to the dataset ####

dat.sim2 <- egdt(
    dat.sim1[,!("ID")]
   ,
    data.table(ID=1:1000)
)
dat.sim2[,ID:=.GRP,by=.(ID,regimen,dose)]

setorder(dat.sim2,regimen,dose,ID,TIME,EVID)
dat.sim2[,.N,by=.(regimen,dose,EVID)]
dat.sim2[,.N,by=.(ID)]

sim2 <- NMsim(path.mod=file.mod,data=dat.sim2,dir.sim="simulations",suffix.sim = "try2",seed=39119)
dims(sim1,sim2)


p2.profs <- ggplot(sim2[EVID==2],aes(TIME,IPRED,group=interaction(ID),colour=dose))+
    geom_line()+
    labs(x="Hours since first dose",y="Concentrations") +
    facet_wrap(~regimen,scales="free_x")

p2.profs


dt.pi <- sim2[EVID==2,
              setNames(as.list(quantile(IPRED,probs=c(.5,.025,.975))),cc(median,lower,upper))
             ,by=.(regimen,dose,DOSE,TIME)]

p2.pi <- ggplot(dt.pi,aes(TIME,median,colour=dose,fill=dose))+geom_line()+
    geom_ribbon(aes(ymin=lower,ymax=upper),alpha=.5)+
    labs(x="Hours since first dose",y="Concentrations") +
    facet_wrap(~regimen,scales="free_x")

p2.pi



###  Section end: Sim multiple subjects by adding them to the dataset


#### Section start: Sim multiple subjects by Nonmem's $SUBPROBLEM option ####

### In order for this to work, we need the development version of
### NMdata. Version 0.0.13 on CRAN+MPN will throw an error.
load_all("~/wdirs/NMdata",export_all=FALSE)
NMdataConf(as.fun="data.table")

load_all("~/wdirs/NMexec",export_all=FALSE)

sim3 <- NMsim(path.mod=file.mod,data=dat.sim1,dir.sim="simulations",suffix.sim = "subproblems"
             ,subproblems=1000,seed=39119)

dt.pi <- sim3[EVID==2,
              setNames(as.list(quantile(IPRED,probs=c(.5,.025,.975))),cc(median,lower,upper))
             ,by=.(regimen,dose,DOSE,TIME)]

p3.pi <- p2.pi %+% dt.pi



### section end: Sim multiple subjects by Nonmem's $SUBPROBLEM option


#### Section start: Comparisson of the two methods for simulation of multiple subjects ####


sim.23 <- rbind(transform(sim2,sim="Larger dataset"),
                transform(sim3,sim="Subproblem")
                ,fill=TRUE)
dt.pi <- sim.23[EVID==2,
              setNames(as.list(quantile(IPRED,probs=c(.5,.025,.975))),cc(median,lower,upper))
             ,by=.(sim,regimen,dose,DOSE,TIME)]

p.pi.23 <- ggplot(dt.pi,aes(TIME,median,colour=dose,fill=dose))+geom_line()+
    geom_ribbon(aes(ymin=lower,ymax=upper),alpha=.5)+
    facet_wrap(regimen~sim,scales="free_x")
p.pi.23


### Section end: Comparisson of the two methods for simulation of multiple subjects




#### Section start: Save plots and tables with stamps using tracee ####

install.packages("tracee",repos="https://cloud.r-project.org")


library(tracee)
ggwrite(p2.pi,file="outputs/example_plot_pi.png",script=script)

library(pracma)
exp.id <- sim2[EVID==2,.(AUC=trapz(TIME,IPRED),Cmax=max(IPRED)),by=.(ID,DOSE,dose)] |>
    melt(measure.vars=cc(AUC,Cmax))

exp.pop <- exp.id[,.(median=median(value),ci=sprintf("(%s;%s)",signif(quantile(value,probs=.025),3),signif(quantile(value,probs=.975),3))),keyby=.(dose,variable)]

library(tracee)
library(flextable)
ft.exp.pop <- flextable(exp.pop)

writeFlextab(ft.exp.pop,file="outputs/example_table_exposure.png",
             script=script,formats=cc(png, docx, pptx, html))

### Section end: Save plots and tables with stamps using tracee

