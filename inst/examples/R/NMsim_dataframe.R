library(devtools)
load_all("../../../../NMdata")
load_all()


library(dplyr)
library(tibble)
library(ggplot2)
library(patchwork)
library(tracee)


theme_set(theme_bw())
this.script <- "NMsim/inst/examples/R/NMsim_dataframe.R"
writeOutput <- TRUE

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
## as.data.table(dat.sim1)[,.N,by=.(regimen,DOSE,EVID)]
dat.sim1 %>%
    group_by(regimen,DOSE,EVID) %>%
    summarize(length(EVID)) 


## remember some of the dosing records represent multiple (6) doses
## NMexpandDoses(dat.sim1)[,.N,by=.(regimen,DOSE,EVID)]
NMexpandDoses(dat.sim1) %>%
    group_by(ID,regimen,DOSE,EVID) %>%
    summarize(N=length(EVID)) %>%
    tidyr::spread(EVID,N)

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
file.mod <- "../nonmem/xgxr014.mod"
simres <- NMsim(path.mod=file.mod,
                data=dat.sim1
##               ,path.nonmem="/opt/NONMEM/nm75/run/nmfe75"
                 )

simres <- NMsim(path.mod=file.mod,
                data=dat.sim1,
                dir.sim="../simulations"
               ,name.sim = "df1",
                seed=41,
                subproblems=Nsubjects
               ,reuse.results=reuse.results
                ## ,method.execute="psn-execute"
               ,path.nonmem="/opt/NONMEM/nm75/run/nmfe75"
                )

simres <- as.data.table(simres)[,ID:=.GRP,by=.(NMREP,ID)]
head(simres,n=3)
ggplot(simres,aes(TIME,PRED,colour=dose))+
    geom_line()+
    labs(x="Hours",y="Concentration (ng/mL)")+
    facet_wrap(~regimen,scales="free")



## typical subject
simres.typ <- NMsim(path.mod=file.mod,
                    data=dat.sim1,
                    type.sim="typical",
                    dir.sim="../simulations",suffix.sim = "df1.typ",
                    seed=334
                    ,method.execute="psn-execute"
                   ,reuse.results=reuse.results)
simres.typ$type <- "typical"

simres.pi <- simres[,setNames(as.list(quantile(IPRED,probs=c(.05,.95))),cc(ll,ul)),
                    by=.(regimen,TIME,dose)]
simres.pi$type <- "pi"
simres.pi$pi.cover <- "90%"

simres.all <- bind_rows(simres.typ,simres.pi)



p.pi.typ <- ggplot(simres.all,aes(TIME,fill=dose))+
    geom_ribbon(aes(ymin=ll,ymax=ul,alpha=pi.cover),data=function(x)filter(x,type=="pi"))+
    geom_line(aes(y=IPRED,colour=dose),data=function(x)filter(x,type=="typical"),size=.6)+
    facet_wrap(~regimen,scales="free_x")+
    scale_alpha_manual(values=c("90%"=.5))+
    labs(x="Hours since first dose",y="Concentration (ng/mL)")

ggwrite(p.pi.typ,file="../outputs/exdf_pi_typ.png",script=this.script,save=writeOutput)

## from bootstrap
## allmods.bs <- list.files(path="../nonmem/bs1_014_N1000/m1",pattern=".+\\_[0-9].mod$",full.names = T)
## allmods.bs <- list.files(path="../nonmem/bs1_014_N1000/m1",pattern=".+\\_1.mod$",full.names = T)
allmods.bs <- list.files(path="../nonmem/bs1_014_N1000/m1",pattern=".+\\.mod$",full.names = T)
dt.mods.bs <- data.table(path=allmods.bs)[
   ,modnum:=as.numeric(sub(".+_([0-9]+)\\.mod","\\1",allmods.bs))]
allmods.bs <- dt.mods.bs[modnum<=Nmods,path]

#### The function passed to seed makes NMsim generate seeds running
#### this function. If seed doesn't vary, ETAs will be the same in all
#### models. Here, we use PRED for the simulation without BSV. We let
#### the ETAs vary according to OMEGA. This allows to look at
#### variability of the estimate of THETAs, OMEGA + the variability
#### described by OMEGA. Currently, NMsim does not support application
#### of SIGMAS onto IPRED and PRED, beyond what's already simulated in
#### the nonmem model.


NMsim(allmods.bs,
      data=dat.sim1,
      dir.sim="../simulations/xgxr014_bs1",
      suffix.sim="sim1_bs",
      subproblems=150
     ,sge=TRUE
     ,seed=function()sample(1:1e8,size=1),
      text.table="PRED IPRED"
      ,method.execute="psn-execute"
     ,reuse.results=reuse.results
     ,path.nonmem="/opt/NONMEM/nm75/run/nmfe75"
      )


## read all models in dir
## allres <- NMscanMultiple(dir="../simulations/xgxr014_bs1",file.pattern = ".+\\.lst",merge.by.row=FALSE,quiet=T)
## only read the first Nmods models
lsts.sim <- paste0("NMsim_bs_pr1_",1:Nmods,"_sim1_bs.lst")
allres <- NMscanMultiple(files=file.path("../simulations/xgxr014_bs1",lsts.sim),merge.by.row=FALSE,quiet=T)
## Now derive confidence intervals using quantiles of PRED, IPRED, EXTRAVAR by time or derived metrics by ID and model first.

setDT(allres)
if(F) { ## individual lines
    ggplot(allres[EVID==2],aes(TIME,PRED,group=interaction(model,dose),colour=dose))+geom_line(alpha=.3)+
        facet_wrap(~regimen)

    ggplot(allres[EVID==2],aes(TIME,IPRED,group=interaction(model,dose),colour=dose))+geom_line(alpha=.3)+
        facet_wrap(~regimen)
}

allresl <- melt(allres[EVID==2],measure.vars=cc(PRED,IPRED),variable.name="pred.type",value.name="pred")

sum.res.model <- allresl[,
                         .(predm=quantile(pred,probs=c(.5)))
                        ,by=.(model,regimen,dose,DOSE,TIME,pred.type)]


sum.res <- sum.res.model[,
                         setNames(as.list(quantile(predm,probs=c(.025,.5,.975))),cc(predml,predmm,predmu))

                        ,by=.(regimen,dose,DOSE,TIME,pred.type)]

sum.res


ggplot(sum.res,aes(x=TIME,fill=dose))+
    geom_ribbon(aes(ymin=predml,ymax=predmu),alpha=.5)+
    geom_line(aes(y=predmm,colour=dose))+
    ## facet_grid(regimen~pred.type,scales="free_x")
    facet_wrap(cc(regimen,pred.type),scales="free_x")

## 400 mg only
p.cipi <- ggplot(sum.res[DOSE==400],aes(x=TIME,fill=pred.type))+
    geom_ribbon(aes(ymin=predml,ymax=predmu),alpha=.5)+
    geom_line(aes(y=predmm,colour=pred.type))+
    facet_wrap("regimen",scales="free_x")+
    labs(x="Hours since first dose",y="Concentration (ng/mL)")

ggwrite(p.cipi,file="../outputs/exdf_ci_pi_bs.png",script=this.script,save=writeOutput)


## known subjects
NMdataConf(as.fun=NULL)

## Simulating the 400 mg QD regimen for selected patients
base.sim.known <- dat.sim.md[dat.sim.md$DOSE==400,]

## apply doses to a subset of known subjects. NB, max allowed depends
## on subsetting.
nsubj.known <- 5
res.mod <- NMscanData(file.mod)
idvars <- findCovs(res.mod,by="ID")

ids <- idvars[idvars$DOSE==300,cc(ID,STUDY)][1:nsubj.known,]
## cartesian product or outer join.
dat.sim1.known <- merge(ids,
                        base.sim.known[,setdiff(colnames(base.sim.known),c("ID"))]
                        )
setorder(dat.sim1.known,ID,TIME,EVID)

res.known <- NMsim(file.mod,
                   data=dat.sim1.known,
                   dir.sim="../simulations/xgxr014_known1",
                   suffix.sim="known1",
                   text.table="PRED IPRED CL V KA"
                  ,type.sim="known"
                  ,nmquiet=FALSE
                  ,reuse.results=reuse.results
                  ,path.nonmem="/opt/NONMEM/nm75/run/nmfe75"
                   )

p.known.time.ipred <-
    ggplot(res.known,aes(TIME,IPRED,colour=factor(ID)))+
    geom_line()+
    labs(x="Hours since first dose",y="Concentration (ng/mL)")

ggwrite(p.known.time.ipred,file="../outputs/exdf_known_time_ipred.png",script=this.script,save=writeOutput)

#### check parameters in simulations against pk model fit
res.check <- merge(
    unique(as.data.table(res.known)[,c("ID","CL", "V","KA"),with=FALSE]) |> melt(id.vars="ID")
   ,
    unique(as.data.table(idvars)[ID%in%ids$ID,c("ID","CL", "V","KA"),with=FALSE]) |> melt(id.vars="ID")
   ,by=c("ID","variable")
   ,all.x=TRUE
   ,suffixes=c("model","sim")
)

p.known.check <- ggplot(res.check,aes(valuemodel,valuesim))+
    geom_abline(slope=1,intercept=0)+
    geom_point()+
    facet_wrap(~variable,scales="free")

ggwrite(p.known.check,file="../outputs/exdf_known_check.png",script=this.script,save=writeOutput)

