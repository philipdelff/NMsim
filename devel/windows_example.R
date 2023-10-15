library(data.table)
library(NMsim)

library(NMdata)
NMdataConf(dir.psn="C:/Users/Philip Delff/software/PsN-5.3.1/strawberry/perl/bin")

file.project <- function(...)file.path(system.file("examples",package="NMsim"),...)
file.mod <- file.project("nonmem/xgxr021.mod")



### multiple dose regimens with loading are easily created with NMcreateDoses too
## We use ADDL+II (either method easy)
doses <- NMcreateDoses(TIME=c(0,24),AMT=c(300,150),addl=data.frame(ADDL=c(0,5),II=c(0,24)),CMT=1)
doses <- transform(doses,trt="300 mg then 150 mg QD")

## Add simulation records - longer for QD regimens
dat.sim <- addEVID2(doses,time.sim=0:(24*7),CMT=2)

## sort data set 
setorder(dat.sim,ID,TIME,EVID)

## Adding a row identifier (generally not necessary)
dat.sim$ROW <- 1:nrow(dat.sim)



simres <- NMsim(file.mod=file.mod,
                data=dat.sim,
                dir.sims="~/winex_NMsim")

