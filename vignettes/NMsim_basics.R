## ----include = FALSE----------------------------------------------------------
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
dt.amt <- subset(dt.amt,DOSE==400)
doses.md <- NMcreateDoses(TIME=c(0,24),AMT=dt.amt,addl=data.frame(ADDL=c(0,5),II=c(0,24)))
doses.md <- within(doses.md,{
    dose=paste(DOSE,"mg")
    regimen="QD"
})
doses.md
## doses.md$ID <- max(doses.sd$ID)+doses.md$ID


## we will simulate with SD and QD
## doses.all <- bind_rows(doses.sd
##                        ,
##                        doses.md
##                        )


## Add simulation records - longer for QD regimens
dat.sim.sd <- addEVID2(doses.sd,time.sim=0:24,CMT=2)
dat.sim.md <- addEVID2(doses.md,time.sim=0:(24*7),CMT=2)

## Stack simulation data, reassign ID to be unique in combined dataset
dat.sim1 <- bind_rows(
    ##  dat.sim.sd
    ## ,
    dat.sim.md
)
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
#  file.mod <- file.project("nonmem/xgxr017.mod")
#  file.mod <- "~/xgxg_data/nonmem/xgxr018.mod"
#  file.mod <- "~/xgxg_data/nonmem/xgxr021.mod"
#  simres <- NMsim(path.mod=file.mod,
#                  data=dat.sim1
#                  ## ,method.update.inits="nmsim"
#                 ,dir.sims="~/NMsim_test"
#                 ,seed=12345
#                  )

## ----eval=FALSE---------------------------------------------------------------
#  as.data.table(simres) |>
#      melt(measure.vars=cc(PRED,IPRED))|>
#  	ggplot(aes(TIME,value,colour=variable))+
#      geom_line()+
#      facet_wrap(~regimen+dose)
#  
#  datl <- as.data.table(simres) |>
#      melt(measure.vars=cc(PRED,IPRED,Y))
#  ## datl[,type:="Prediction"]
#  ## datl[variable=="Y",type:="Simulation"]
#  
#  	ggplot(datl,aes(TIME,value,colour=variable))+
#      geom_line(data=function(x)x[variable!="Y"])+
#      geom_point(data=function(x)x[variable=="Y"])+
#      facet_wrap(~regimen+dose)

## ----sim-twomodels,eval=FALSE-------------------------------------------------
#  ## file.mod <- "../nonmem/xgxr014.mod"
#  file.mod <- file.project(c("nonmem/xgxr014.mod","nonmem/xgxr114.mod"))
#  simres <- NMsim(path.mod=file.mod,
#                  data=dat.sim1
#                  )

## ----eval=FALSE---------------------------------------------------------------
#  ggplot(simres,aes(TIME,PRED,colour=model))+geom_line()+
#      facet_wrap(c("regimen","dose"),scales="free")

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
#  simres.pi <- simres[,setNames(as.list(quantile(IPRED,probs=c(.05,.5,.95))),cc(ll,median,ul)),
#                      by=.(regimen,TIME,dose)]
#  simres.pi$type <- "pi"
#  simres.pi$pi.cover <- "90%"
#  
#  p.pi.typ <- ggplot(simres.pi,aes(TIME,fill=dose))+
#      geom_ribbon(aes(ymin=ll,ymax=ul,alpha=pi.cover))+
#      geom_line(aes(y=median,colour=dose))+
#      facet_wrap(~regimen,scales="free_x")+
#      scale_alpha_manual(values=c("90%"=.5))+
#      labs(x="Hours since first dose",y="Concentration (ng/mL)")
#  
#  p.pi.typ

## ----eval=FALSE---------------------------------------------------------------
#  simres.typ <- NMsim(path.mod=file.mod,
#                      data=dat.sim1,
#                      name.sim="typSubj",
#                      method.sim=NMsim_typical)
#  

## ----eval=FALSE---------------------------------------------------------------
#  p.typ <- ggplot(simres.typ,aes(TIME,IPRED))+geom_line()+
#      geom_line(aes(y=PRED),colour=2)
#  p.typ
#  

## ----eval=FALSE---------------------------------------------------------------
#  base.sim.known <- dat.sim.md[dat.sim.md$DOSE==400,]
#  
#  res.mod <- NMscanData(file.mod)
#  ids <- data.frame(ID=unique(res.mod$ID))
#  
#  
#  dat.sim1.known <- merge(ids,
#                          base.sim.known[,setdiff(colnames(base.sim.known),c("ID")),with=FALSE]
#                          )
#  setorder(dat.sim1.known,ID,TIME,EVID)
#  
#  res.known <- NMsim(file.mod,
#                     data=dat.sim1.known,
#                     suffix.sim="known1",
#                     text.table="PRED IPRED CL V2 KA"
#                    ,method.sim=NMsim_known
#                     ## ,method.update.inits="nmsim"
#                    ,path.nonmem="/opt/NONMEM/nm75/run/nmfe75"
#                     )

## ----pddata,include=FALSE,eval=FALSE------------------------------------------
#  ## pd <- readRDS("~/wdirs/NMsim/inst/examples/data/xgxr_pd.rds")
#  pd <- system.file("examples/data/xgxr_pd.rds", package = "NMsim")

## ----known-pkpd,eval=FALSE----------------------------------------------------
#  ## Take dose records from PK model estimation input data
#  pkres <- NMscanData(file.mod,as.fun="data.table")
#  pkdos <- pkres[EVID==1,.(ID, TIME, EVID, CMT, AMT)]
#  ## Take PD data observation records (`pdsamples`)
#  pd[,ROWPD:=.I]
#  pdsamples <- pd[EVID==0,.(ROWPD,ID,TIME,EVID=2)]
#  ## Stack `pkdos` and `pdsamples` to one data set (`pdsim`)
#  pdsim <- rbind(pkdos,pdsamples,fill=TRUE)
#  pdsim[,DV:=NA]
#  ## pdsim[,all(ID%in%pkres$ID)]
#  ## pdsim[,.N,by=ID%in%pkres$ID]
#  pdsim <- pdsim[ID%in%pkres$ID]
#  setorder(pdsim,ID,TIME,EVID)

## ----known-pkpd-run,eval=FALSE------------------------------------------------
#  
#  res.pksim <- NMsim(file.mod,
#                     data=pdsim,
#                     suffix.sim="pkpd",
#                    ,method.sim=NMsim_known
#                    ,path.nonmem="/opt/NONMEM/nm75/run/nmfe75"
#                    ,text.table="IPRED PRED"
#                     )
#  

## ----merge-pdres,eval=FALSE---------------------------------------------------
#  setnames(res.pksim,"IPRED","PKIPRED")
#  pd2 <- mergeCheck(pd,res.pksim[,.(ROWPD,PKIPRED)],by="ROWPD",all.x=TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  ggplot(pd2[PKIPRED>0],aes(PKIPRED,LIDV))+
#      geom_point()+
#      lims(x=c(.001,.5))+
#      labs(x="Individual PK prediction",y="Observed PD value") ## +
#  ## scale_x_log10(limits=c(.001,.5))

## ----VarCov,eval=FALSE--------------------------------------------------------
#  ## file.mod.cov <- file.project("nonmem/xgxr114.mod")
#  NMdataConf(path.nonmem="/opt/NONMEM/nm75/run/nmfe75")
#  
#  file.mod.cov <- "~/xgxg_data/nonmem/xgxr017.mod"
#  NMsim(
#      path.mod=file.mod.cov,
#      data=dat.sim1
#     ,dir.sims="simulations"
#     ,method.sim=NMsim_VarCov ## Var-Cov parameter sampling
#     ,name.sim="VarCov"       ## a recognizable directory name
#     ,nsims=500               ## sampling 500 models
#      ## ,method.execute="psn"    ## use PSN's execute to allow for parallel execution
#     ,method.execute="directory"    ##
#     ,sge=TRUE                ## run simulations in parallel please
#  )

## ----VarCov-collect,eval=FALSE------------------------------------------------
#  simres.VarCov <- NMscanMultiple(## dir="simulations/xgxr114_VarCov"
#      dir=file.path("simulations",paste0(basename(fnExtension(file.mod.cov,"")),"_VarCov"))
#     ,file.pattern=".+\\.lst$"
#     ,merge.by.row=FALSE,quiet=T
#     ,as.fun="data.table")
#  

## ----VarCov-summarize,eval=FALSE----------------------------------------------
#  
#  allresl <- melt(simres.VarCov[EVID==2],measure.vars=cc(PRED,IPRED),variable.name="pred.type",value.name="pred")
#  
#  sum.res.model <- allresl[,
#                           .(predm=quantile(pred,probs=c(.5)))
#                          ,by=.(model,regimen,dose,DOSE,TIME,pred.type)]
#  
#  
#  sum.res <- sum.res.model[,
#                           setNames(as.list(quantile(predm,probs=c(.025,.5,.975))),cc(predml,predmm,predmu))
#  
#                          ,by=.(regimen,dose,DOSE,TIME,pred.type)]
#  
#  
#  
#  
#  ggplot(sum.res,aes(x=TIME,fill=dose))+
#      geom_ribbon(aes(ymin=predml,ymax=predmu),alpha=.5)+
#      geom_line(aes(y=predmm,colour=dose))+
#      ## facet_grid(regimen~pred.type,scales="free_x")
#      facet_wrap(cc(regimen,pred.type),scales="free_x")
#  
#  ## 400 mg only
#  p.cipi.VarCov <- ggplot(sum.res[DOSE==400],aes(x=TIME,fill=pred.type))+
#      geom_ribbon(aes(ymin=predml,ymax=predmu),alpha=.5)+
#      geom_line(aes(y=predmm,colour=pred.type))+
#      facet_wrap("regimen",scales="free_x")+
#      labs(x="Hours since first dose",y="Concentration (ng/mL)")
#  
#  
#  

## ----bootstrap-execute,eval=FALSE---------------------------------------------
#  ## generate a vector with paths to all the input control streams
#  
#  ## mods.bootstrap <- list.files(path="../nonmem/bs1_014_N1000/m1",pattern=".+\\.mod$",full.names = T)
#  mods.bootstrap <- list.files(path="~/xgxg_data/nonmem/bootstrap_dir2/m1/",pattern=".+\\.mod$",full.names = T)
#  
#  NMsim(
#      path.mod=mods.bootstrap,
#      data=dat.sim1
#     ,method.sim=NMsim_default ## a single simulation with each sampled model
#     ,dir.sims="simulations/bootstrap"
#      ## ,name.sim="bootstrap"       ## a recognizable directory name
#     ,method.execute="psn"    ## use PSN's execute to allow for parallel execution
#     ,text.table="PRED IPRED"
#     ,sge=TRUE                ## run simulations in parallel please
#  )

## ----bootstrap-collect,eval=FALSE---------------------------------------------
#  ## ~/xgxg_data/nonmem/bootstrap_dir2/m1/NMsim/bs_pr1_100_bootstrap/NMsim_bs_pr1_100_bootstrap.mod
#  files.lst <- list.files("simulations/bootstrap",recursive=TRUE,pattern=".lst$",full.names=TRUE)
#  
#  simres.bootstrap <- NMscanMultiple(files=files.lst
#                                     ## dir="
#                                     ## ,file.pattern=".+\\.lst$"
#                                    ,merge.by.row=FALSE,quiet=T
#                                    ,as.fun="data.table")
#  
#  allresl <- melt(simres.bootstrap[EVID==2],measure.vars=cc(PRED,IPRED),variable.name="pred.type",value.name="pred")
#  
#  sum.res.model <- allresl[,
#                           .(predm=quantile(pred,probs=c(.5)))
#                          ,by=.(model,regimen,dose,DOSE,TIME,pred.type)]
#  
#  
#  sum.res.bootstrap <- sum.res.model[,
#                                     setNames(as.list(quantile(predm,probs=c(.025,.5,.975))),cc(predml,predmm,predmu))
#  
#                                    ,by=.(regimen,dose,DOSE,TIME,pred.type)]
#  
#  

## ----bootstrap-summarize,eval=FALSE-------------------------------------------
#  
#  p.cipi.bootstrap <- ggplot(sum.res.bootstrap[DOSE==400],aes(x=TIME,fill=pred.type))+
#      geom_ribbon(aes(ymin=predml,ymax=predmu),alpha=.5)+
#      geom_line(aes(y=predmm,colour=pred.type))+
#      facet_wrap("regimen",scales="free_x")+
#      labs(x="Hours since first dose",y="Concentration (ng/mL)")
#  
#  p.cipi.bootstrap
#  

## ----compare-plots,eval=FALSE-------------------------------------------------
#  
#  p.cipi.VarCov+
#      lims(y=c(0,12500))+
#      labs(subtitle="Covariance step")+
#      p.cipi.bootstrap+
#      lims(y=c(0,12500))+
#      labs(subtitle="Bootstrap")

## ----eval=FALSE---------------------------------------------------------------
#  simres.with.resvar <- addResVar(simres,path.ext=fnExtension(file.mod,"ext"),par.type="SIGMA",prop=1,add=2)

## ----include=FALSE,eval=FALSE-------------------------------------------------
#  dt.inp2 <- rbind(
#      transform(dt.inp,CLSCALE=1,sim="CL unchanged")
#      ,transform(dt.inp,CLSCALE=2,sim="Double CL")
#  )
#  dt.inp2[,ID:=.GRP,by=.(sim,ID)]
#  NMcheckData(dt.inp2)
#  
#  
#  simres2 <- NMsim(path.mod=path.model,dt.inp2,name.sim="20mg_CL2"
#                  ,list.sections=list(PK=function(x)c(x,"CLP=CLP*CLSCALE","CLM=CLM*CLSCALE")))
#  
#  ggplot(simres2[EVID==2],aes(TIME,PRED,colour=sim))+geom_line()+facet_wrap(~compound)
#  
#  
#  ## Configuration of how to execute Nonmem
#  `NMsim` supports two ways
#  
#  ## Other important arguments to `NMsim`
#  
#  <!-- simres.typ$type <- "typical" -->

