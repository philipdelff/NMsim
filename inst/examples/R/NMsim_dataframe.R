load_all()
library(devtools)
load_all("../../../../NMdata")

library(dplyr)
library(tibble)
load_all("../../../../NMdata")


dt.amt <- data.frame(DOSE=c(100,400))
dt.amt <- within(dt.amt,{AMT=DOSE*1000})
dt.amt
doses.sd <- NMcreateDoses(TIME=0,AMT=dt.amt)
doses.sd <- within(doses.sd,{
    dose=paste(DOSE,"mg")
    regimen="SD"
})
doses.sd


### multiple dose regimens with loading are easily created with NMcreateDoses too
## We use ADDL+II (either method easy)
dt.amt <- data.frame(AMT=c(200,100,800,400)*1000,DOSE=c(100,100,400,400))
doses.md <- NMcreateDoses(TIME=c(0,24),AMT=dt.amt,addl=data.frame(ADDL=c(0,5),II=c(0,24)))
doses.md <- within(doses.md,{
    dose=paste(DOSE,"mg")
    regimen="QD"
})
doses.md
## doses.md$ID <- max(doses.sd$ID)+doses.md$ID


## we will simulate with SD and QD
doses.all <- bind_rows(doses.sd,doses.md)




## Add simulation records - longer for QD regimens
dat.sim.sd <- addEVID2(doses.sd,time.sim=0:24,CMT=2)
dat.sim.md <- addEVID2(doses.md,time.sim=0:(24*7),CMT=2)

## Stack simulation data, reassign ID to be unique in combined dataset
dat.sim1 <- bind_rows(dat.sim.sd,dat.sim.md)
dat.sim1 <- as.data.table(dat.sim1)[,ID:=.GRP,by=.(regimen,ID,DOSE)]
dat.sim1 <- as_tibble(dat.sim1)
## dat.sim1 %>%
##     group_by(regimen,ID,DOSE) %>%
##     mutate(ID=)
## quick look at top and bottom of sim data and a quick summary
print(dat.sim1,topn=5)
dat.sim1[,.N,by=.(regimen,DOSE,EVID)]
dat.sim1 %>%
    group_by(regimen,DOSE,EVID) %>%
    summarize(length(EVID)) 


## remember some of the dosing records represent multiple (6) doses
## NMexpandDoses(dat.sim1)[,.N,by=.(regimen,DOSE,EVID)]
NMexpandDoses(dat.sim1) %>%
    group_by(regimen,DOSE,EVID) %>%
    summarize(length(EVID)) 

dat.sim1 <- as.data.table(dat.sim1)
setorder(dat.sim1,ID,TIME,EVID)
dat.sim1$ROW <- 1:nrow(dat.sim1)

NMorderColumns(dat.sim1)


### Check simulation data
NMcheckData(dat.sim1)


#### Simulations
## new subjects

## typical subject

## from bootstrap

## known subjects
