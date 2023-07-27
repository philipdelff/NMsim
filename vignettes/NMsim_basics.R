## ---- include = FALSE---------------------------------------------------------
##knitr::opts_chunk$set(dev = "cairo_pdf")
knitr::opts_chunk$set(
                      collapse = TRUE
                     ,comment = "#>"
                     ,fig.width=7
                     ,cache=FALSE
                  )

## this changes data.table syntax. I think we can do without.
## knitr::opts_chunk$set(tidy.opts=list(width.cutoff=60), tidy=TRUE)

## ----setup,include=F----------------------------------------------------------
## library(devtools)
## load_all("../../../../NMdata")
## load_all()
library(NMsim)
library(data.table)
library(NMdata)
library(dplyr)
library(tibble)
library(ggplot2)
library(patchwork)
library(tracee)
library(tidyr)

theme_set(theme_bw())
this.script <- "NMsim_basics.Rmd"
writeOutput <- TRUE
file.project <- function(...)file.path(system.file("examples",package="NMsim"),...)
found.files <- list.files(file.project("nonmem/NMsim"),pattern="noname\\.(lst|xml|ext|cov|cor|coi|phi|msf|msfi|msfo|tab)",full.names=TRUE)
unlink(found.files)

## ----dsCreateSim,include=FALSE------------------------------------------------
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
setorder(dat.sim1,ID,TIME,EVID)
dat.sim1$ROW <- 1:nrow(dat.sim1)

dat.sim1 <- NMorderColumns(dat.sim1)


dat.sim1 <- as_tibble(dat.sim1)

## -----------------------------------------------------------------------------
NMexpandDoses(dat.sim1) %>%
    group_by(ID,regimen,DOSE,EVID,AMT) %>%
    summarize(N=length(EVID)) %>%
    spread(EVID,N)

print(as.data.table(dat.sim1),topn=5)

## ----sim-simplest,eval=FALSE--------------------------------------------------
#  ## file.mod <- "../nonmem/xgxr014.mod"
#  file.mod <- file.project("nonmem/xgxr014.mod")
#  simres <- NMsim(path.mod=file.mod,
#                  data=dat.sim1
#                  )

## ----sim-twomodels,eval=FALSE-------------------------------------------------
#  ## file.mod <- "../nonmem/xgxr014.mod"
#  file.mod <- file.project(c("nonmem/xgxr014.mod","nonmem/xgxr114.mod"))
#  simres <- NMsim(path.mod=file.mod,
#                  data=dat.sim1
#                  )

## ----eval=FALSE---------------------------------------------------------------
#  dat.sim.1000 <- NMdata::egdt(
#                              as.data.table(dat.sim1)[,!("ID")]
#                             ,
#                              data.table(ID=1:1000)
#                          )
#  dat.sim.1000[,ID:=.GRP,by=.(ID,regimen,dose)]
#  
#  setorder(dat.sim.1000,regimen,dose,ID,TIME,EVID)

## ----sim-1000-data,eval=FALSE-------------------------------------------------
#  file.mod <- file.project("nonmem/xgxr014.mod")
#  simres <- NMsim(path.mod=file.mod,
#                  data=dat.sim.1000
#                  )

## ----sim-1000-subproblem,eval=FALSE-------------------------------------------
#  ## file.mod <- "../nonmem/xgxr014.mod"
#  file.mod <- file.project("nonmem/xgxr014.mod")
#  simres <- NMsim(path.mod=file.mod,
#                  data=dat.sim1,
#                  subproblems=1000
#                  )
#  simres <- as.data.table(simres)[,ID:=.GRP,by=.(NMREP,ID,regimen,dose)]

## ----eval=FALSE---------------------------------------------------------------
#  simres.typ <- NMsim(path.mod=file.mod,
#                      data=dat.sim1,
#                      method.sim=NMsim_typical)
#  

## ----eval=FALSE---------------------------------------------------------------
#  base.sim.known <- dat.sim.md[dat.sim.md$DOSE==400,]
#  
#  res.mod <- NMscanData(file.mod)
#  ids <- res.mod[,.(ID=unique(ID))]
#  
#  
#  dat.sim1.known <- merge(ids,
#                          base.sim.known[,setdiff(colnames(base.sim.known),c("ID"))]
#                          )
#  setorder(dat.sim1.known,ID,TIME,EVID)
#  
#  res.known <- NMsim(file.mod,
#                     data=dat.sim1.known,
#                     suffix.sim="known1",
#                     text.table="PRED IPRED CL V KA"
#                    ,method.sim=NMsim_known
#                    ,path.nonmem="/opt/NONMEM/nm75/run/nmfe75"
#                     )

## ----known-pkpd,eval=FALSE----------------------------------------------------
#  res.pksim <- NMsim(file.mod,
#                     data=pdsim,
#                     suffix.sim="pkpd",
#                    ,method.sim=NMsim_known
#                    ,text.table="ROW IPRED"
#                    ,path.nonmem="/opt/NONMEM/nm75/run/nmfe75"
#                     )
#  

## ----VarCov,eval=FALSE--------------------------------------------------------
#  file.mod.cov <- "inst/examples/nonmem/xgxr114.mod"
#  NMsim(
#      path.mod=file.mod.cov,
#      data=dat.sim1
#     ,method.sim=NMsim_VarCov ## Var-Cov parameter sampling
#     ,name.sim="VarCov"       ## a recognizable directory name
#     ,nsims=500               ## sampling 500 models
#     ,method.execute="psn"    ## use PSN's execute to allow for parallel execution
#     ,sge=TRUE                ## run simulations in parallel please
#  )

## ----VarCov-collect,eval=FALSE------------------------------------------------
#  simres.VarCov <- NMscanMultiple(dir=
#                          ,file.pattern=".+\\.lst$"
#                          ,merge.by.row=FALSE,quiet=T
#                          ,as.fun="data.table")
#  

## ----VarCov-summarize,eval=FALSE----------------------------------------------
#  ci.VarCov <- simres.VarCov[,by=.(TIME,dose,regimen)]

## ----bootstrap-execute,eval=FALSE---------------------------------------------
#  ## generate a vector with paths to all the input control streams
#  mods.bootstrap <- list.files(path="../nonmem/bs1_014_N1000/m1",pattern=".+\\.mod$",full.names = T)
#  
#  NMsim(
#      path.mod=mods.bootstrap,
#      data=dat.sim1
#     ,method.sim=NMsim_default ## a single simulation with each sampled model
#     ,name.sim="bootstrap"       ## a recognizable directory name
#     ,method.execute="psn"    ## use PSN's execute to allow for parallel execution
#     ,sge=TRUE                ## run simulations in parallel please
#  )

## ----bootstrap-collect,eval=FALSE---------------------------------------------
#  simres.bootstrap <- NMscanMultiple(dir=
#                          ,file.pattern=".+\\.lst$"
#                          ,merge.by.row=FALSE,quiet=T
#                          ,as.fun="data.table")
#  

## ----bootstrap-summarize,eval=FALSE-------------------------------------------
#  ci.bootstrap <- simres.bootstrap[,by=.(TIME,dose,regimen)]

## ----eval=FALSE---------------------------------------------------------------
#  addResVar(simres,path.ext=fnExtension(file.mod,"ext"))

