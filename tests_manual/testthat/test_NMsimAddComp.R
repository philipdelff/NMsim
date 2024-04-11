

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
path.nonmem <- "/opt/NONMEM/nm75/run/nmfe75" 
file.exists(path.nonmem)

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

file.mod <- "testData/nonmem/xgxr033.mod"
NMsim(file.mod=file.mod
      ,data=dt.sim)
