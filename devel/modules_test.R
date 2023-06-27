library(dplyr)

dt.amt <- data.frame(DOSE=c(100,400))
dt.amt <- within(dt.amt,{AMT=DOSE*1000})
dt.amt
doses.sd <- NMcreateDoses(TIME=0,AMT=dt.amt)
doses.sd <- within(doses.sd,{
    dose=paste(DOSE,"mg")
    regimen="SD"
})
doses.sd



doses.all <- bind_rows(doses.sd)

## Add simulation records
dat.sim.sd <- addEVID2(doses.sd,time.sim=0:24,CMT=2)

## Stack simulation data, reassign ID to be unique in combined dataset
dat.sim1 <- bind_rows(dat.sim.sd)
dat.sim1 <- as.data.table(dat.sim1)[,ID:=.GRP,by=.(regimen,ID,DOSE)]
dat.sim1 <- as_tibble(dat.sim1)
## dat.sim1 %>%
##     group_by(regimen,ID,DOSE) %>%
##     mutate(ID=)
## quick look at top and bottom of sim data and a quick summary
print(dat.sim1,topn=5)
## as.data.table(dat.sim1)[,.N,by=.(regimen,DOSE,EVID)]

dat.sim1 <- as.data.table(dat.sim1)
setorder(dat.sim1,ID,TIME,EVID)
dat.sim1$ROW <- 1:nrow(dat.sim1)

dat.sim1 <- NMorderColumns(dat.sim1)


### Check simulation data
NMcheckData(dat.sim1)



#### Simulations
reuse.results <- TRUE
Nsubjects <- 50
Nmods <- 50

## new subjects
setwd("~/wdirs/NMsim")

unloadNamespace("NMsim")
unloadNamespace("NMdata")
load_all("~/wdirs/NMdata")
load_all()




file.mod <- "inst/examples/nonmem/xgxr014.mod"

NMsim_default$fun.mod(file.mod,seed=1)
NMsim_typical$fun.mod(file.mod,seed=1)
NMsim_known$fun.mod(file.mod,seed=1)

simres <- NMsim(path.mod=file.mod,
                data=dat.sim1
                ##               ,path.nonmem="/opt/NONMEM/nm75/run/nmfe75"
               ,method.update.inits="nmsim"
                )



NMsim()
