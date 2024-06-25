

##install.packages("NMsim",repos="https://cloud.r-project.org")
library(data.table)
library(NMdata)

packageVersion("NMdata")
library(NMsim)

## library(devtools)
## load_all(export_all=FALSE)


## NMdataConf(dir.psn="/opt/psn")
path.nonmem <- "/opt/nonmem/nm751/run/nmfe75"
##
## path.nonmem <- "/opt/NONMEM/nm75/run/nmfe75" 
file.exists(path.nonmem)


library(devtools)
load_all("~/wdirs/NMdata")

NMdataConf(dir.psn=NULL)
NMdataConf(as.fun="data.table"
          ,path.nonmem=path.nonmem
          ,dir.sims= "testData/simtmp"
          ,dir.res= "testData/simres")



#### need a function to drop NMsimVersion and NMsimTime from table
fix.time <- function(x){
    meta.x <- attr(x,"NMsimModTab")
    ## meta.x$time.call <- as.POSIXct("2020-02-01 00:01:01",tz="UTC")
    meta.x$NMsimVersion <- NULL
    meta.x$NMsimTime <- NULL
    
    setattr(x,"NMsimModTab",meta.x)
    invisible(x)
}



dt.dos <- NMcreateDoses(AMT=300,TIME=0)
dt.sim <- addEVID2(doses=dt.dos,time.sim=c(1,6,12),CMT=2)
dt.sim[,BBW:=40][,ROW:=.I]



#### tests

## no mod
file.mod <- "testData/nonmem/xgxr041.mod"


## NMexec(file.mod,sge=FALSE,method.execute="NMsim",dir.data="testData/data")
## NMexec(file.mod,sge=FALSE,method.execute="NMsim",dir.data=NULL)

sres <- NMsim(file.mod=file.mod
             ,data=dt.sim
             ,name.sim="default"
              )


sres <- NMsim(file.mod=file.mod
             ,data=dt.sim
             ,name.sim="auc1"
             ,list.sections=list(MODEL=function(x)c(x,"COMP=(AUC)"),
                                 DES=function(x)c(x,"DADT(4)=A(2)/V2"),
                                 ERROR=function(x)c(x,"AUC=A(4)")
                                 )
             ,table.vars=c("PRED","IPRED","AUC")
              )

sres

### a tte model

file.mod <- "/home/philip/wdirs/TTEgraph2/inst/CaseStudies/Simulation1/Nonmem/run1.mod"
## NMexec(file.mod,method.execute="NMsim",sge=F)

load_all("~/wdirs/NMsim")

sres <- NMsim(file.mod=file.mod
             ,name.sim="tte1"
             ,list.sections=list(MODEL=function(x)c(x,"COMP=(TTE)")
                                ,PK=function(x)c(x,"A_0(2)=0",
                                                 "IF(ICALL.EQ.4) THEN",
                                                 "IF(NEWIND.NE.2) THEN",
                                                 "CALL RANDOM(2,R)",
                                                 "COM(1) = R  ; Store the random number --> Survival",
                                                 "ENDIF",
                                                 "ENDIF")
                                ,DES=function(x)c(x,"DADT(2)=1")
                                ,ERROR=function(x)c(x,"IF(ICALL.EQ.4) THEN",
                                                    "TTE = A(2)",
                                                    "ENDIF",
                                                    "DUMMY=EPS(1)")
                                ,DATA=function(x) c(x,"$ABB COMRES=2")
                                ,SIMULATION = function(x) c(x,"(1992 UNIFORM)")
                                ,THETA=function(x) c(x,"$SIGMA 1 FIX")
                                 )
             ,seed="(323) (98 UNIFORM)"
             ,table.vars=c("PRED","CHZ", "SUR", "HAZNOW")
              )

sres


####### tte VPC
DirDerived.tte <- "~/wdirs/TTEgraph2/inst/CaseStudies/Simulation1/DerivedData"
fileSimTTE <- file.path(DirDerived.tte,'TTE_sim_dat.rds')
obs.tte <- as.data.table(readRDS(fileSimTTE))[EVID==0]
## obs.tte <- as.data.table(readRDS(fileSimTTE))[STIME<2]

sres.vpc <- NMsim(file.mod=file.mod
                 ,name.sim="tte1"
                 ,list.sections=list(MODEL=function(x)c(x,"COMP=(TTE)")
                                    ,PK=function(x)c(x,"A_0(2)=0",
                                                     "IF(ICALL.EQ.4) THEN",
                                                     "IF(NEWIND.NE.2) THEN",
                                                     "CALL RANDOM(2,R)",
                                                     "COM(1) = R  ; Store the random number --> Survival",
                                                     "ENDIF",
                                                     "ENDIF")
                                    ,DES=function(x)c(x,"DESSUR=EXP(-A(1))
IF(COM(1).LT.DESSUR) THEN
DADT(2)=1
ELSE
DADT(2)=0
ENDIF
                             ")
,ERROR=function(x)c(x,"IF(ICALL.EQ.4) THEN",
                                                        "TTESIM = A(2)",
                                                        "ENDIF",
                                                        "DUMMY=EPS(1)")
,DATA=function(x) c(x,"$ABB COMRES=2")
##,SIMULATION = function(x) c(x,"(1992 UNIFORM)")
,THETA=function(x) c(x,"$SIGMA 1 FIX")
,SUBROUTINE=function(x)(sub("TOL=9","TOL=6",x))
)
,args.seed=list(dist=c("","","UNIFORM"))
,table.vars=c("PRED","CHZ", "SUR", "HAZNOW","TTESIM")
## ,nsims=100
,subproblems=100
                  )

sres.vpc[,TIME:=TTESIM]



## install.packages("tidyvpc")
## library(tidyvpc)
## install.packages("vpc")

library(vpc)

## setnames(obs.tte ,"TTE","TIME")
obs.tte[,TIME:=TTE]

vpc1 <- vpc_tte(
    sim = sres.vpc[EVID==0],
    obs = obs.tte,
    rtte = FALSE
)

vpc1

library(ggplot2)
library(patchwork)

ggplot(obs.tte[EVID==0],aes(TIME))+geom_histogram()+
    ggplot(sres.vpc[EVID==0],aes(TIME))+
    geom_histogram()+
    plot_layout(ncol=1)


    scale_y_continuous(labels=scales::percent)## +
## labs(title=scenario1
##     ,x=labels$time)



####### sim tte and dropout
