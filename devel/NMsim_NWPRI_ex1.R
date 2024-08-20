file.mod <- "example_nonmem_models/lorlatinib_sim_est/mod_lorlatinib_estimate.mod"


## need a relevant simulation data set


simres <- NMsim(file.mod,
                data=data.sim,
                method.sim=NMsim_NWPRI
                dir.sims
                )
                

#### The model shown in the NMsim-ParamUncertain vignette
unloadNamespace("NMsim")
unloadNamespace("NMdata")

load_all("~/wdirs/NMdata")
load_all("~/wdirs/NMsim")

file.project <- function(...)file.path(system.file("examples",package="NMsim"),...)
## file.project <- function(...)file.path("~/wdirs/NMsim/inst/examples",...)
file.mod <- file.project("nonmem/xgxr032.mod")
dat.sim <- read_fst("simulate-results/dat_sim.fst")

simlsts.NWPRI <- NMsim(
    file.mod=file.mod,              ## Path to estimation input control stream
    data=dat.sim                    ## simulation input data
   ,dir.sims="~/NMsim_vignette/tmp" ## where to store temporary simulation files
   ,dir.res="simulate-results"      ## where to store simulation results files
   ,table.vars="PRED IPRED Y"         ## Let Nonmem write a minimum output table
   ,method.sim=NMsim_NWPRI         ## Var-Cov parameter sampling
   ,name.sim="NWPRI"               ## a recognizable directory name
   ,subproblems=500                       ## sampling 500 models
   ,sge=FALSE                        ## run simulations in parallel please
)


### I tried changing the OMEGAPD values from 1 to 0. Same result.
NMexec("/data/home/philipde/NMsim_vignette/tmp/xgxr032_NWPRI/xgxr032_NWPRI_man.mod",sge=F)

## I dropped all the zero-valued OMEGA and SIGMA elements and recoded the numbering in the code. That worked.
NMexec("/data/home/philipde/NMsim_vignette/tmp/xgxr032_NWPRI_man2.mod",sge=F)

res <- NMscanData("/data/home/philipde/NMsim_vignette/tmp/xgxr032_NWPRI_man2.lst")

### I set them to 1e-9. That ran. 
## However, PRED does not vary
NMexec("/data/home/philipde/NMsim_vignette/tmp/xgxr032_NWPRI_man3.mod",sge=F)

allres <- NMscanData("/data/home/philipde/NMsim_vignette/tmp/xgxr032_NWPRI_man3.lst",as.fun="data.table")


### Then I tried to remove FIX/FIXED from $OMEGAPD
NMexec("/data/home/philipde/NMsim_vignette/tmp/xgxr032_NWPRI_man4.mod",sge=F)

allres <- NMscanData("/data/home/philipde/NMsim_vignette/tmp/xgxr032_NWPRI_man3.lst",as.fun="data.table")

## long format so calculations can be done by prediction type.
allresl <- melt(allres[EVID==2],
                measure.vars=c("PRED","IPRED"),
                variable.name="pred.type",
                value.name="pred.value")

## deriving median by model and time to have a single value per model
## and time point. This is only needed in case multiple subjects are
## simulated by each model.
sum.res.model <- allresl[,
                         .(predm=median(pred.value))
                        ,by=.(model,TIME,pred.type,NMREP)]

ggplot(sum.res.model[pred.type=="PRED"],aes(TIME,predm,group=NMREP))+
    geom_line()


sum.uncertain <- sum.res.model[
   ,setNames(as.list(quantile(predm,probs=c(.025,.5,.975))),
             c("predml","predmm","predmu"))
   ,by=.(model,TIME,pred.type)]

library(ggplot2)
ggplot(sum.uncertain,aes(x=TIME,fill=pred.type))+
    geom_ribbon(aes(ymin=predml,ymax=predmu),alpha=.5)+
    geom_line(aes(y=predmm,colour=pred.type))+
    labs(x="Hours since first dose",y="Concentration (ng/mL)")
