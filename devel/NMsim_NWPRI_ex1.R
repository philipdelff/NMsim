#### testing NMdata::NMreadExt()
unloadNamespace("NMsim")
unloadNamespace("NMdata")
library(devtools)
library(here)
setwd("/data/sandbox/trunk/analysis/NMsim/wdirs")
load_all("NMdata")
load_all("NMsim")

file.mod <- here::here("wdirs/NMsim/devel/example_nonmem_models/lorlatinib_sim_est/mod_lorlatinib_estimate.mod")
data.sim <- fread(here("wdirs/NMsim/devel/example_nonmem_models/derived_data/simulated_nonmem_dataset_mod_lorlatinib.csv"))

NMreadSection(file.mod,section="OMEGA")

quiet <- NMreadSection(file.mod)[c("THETA","OMEGA","SIGMA")] |> lapply(function(x)cat(paste(paste(x,collapse="\n"),"\n\n")))

pars = NMreadExt(file.mod,return="pars",as.fun="data.table")[,.(parameter,par.name,i,j,iblock,blocksize,value)]
pars[, parlab := stringr::str_remove_all(string = parameter, pattern = "\\(|\\)") %>% stringr::str_replace(",", "\\_")]

## need a relevant simulation data set

### NMreadExt() test end


simres <- NMsim(file.mod,
                data=data.sim,
                method.sim=NMsim_NWPRI,
                method.execute = "nmsim",
                path.nonmem="/opt/NONMEM/nm75/run/nmfe75",
                subproblems=500,
                modify.model = list(
                   # name the THETAS/OMEGAS/SIGMAS in $ERROR so we can compare the distributions to other methods. 
                   ERROR = function(.x)
                      c(
                         .x,
                         paste0(pars$parlab, " = ", pars$par.name)
                      )
                ),
                table.vars = paste0(
                   "PRED IPRED Y ",
                   paste0(pars$parlab, collapse = " ")
                )
                )
                

library(tidyverse)
dplyr::select(simres, NMREP, THETA1:SIGMA1_1 ) %>% 
   distinct() %>% 
   pivot_longer(!NMREP) %>%
   # mutate(keep = ifelse(length(unique(value))==1, 0, 1), .by = name) %>% 
   # filter(keep==1) %>% 
   
   ggplot(aes(x = value)) + 
   geom_histogram(bins=25)+
   # geom_density() + 
   facet_wrap(~ name, scales = "free") +
   theme_bw()




#### The model shown in the NMsim-ParamUncertain vignette
unloadNamespace("NMsim")
unloadNamespace("NMdata")

load_all("~/wdirs/NMdata")
load_all("~/wdirs/NMsim")

NMdataConf()

file.project <- function(...)file.path(system.file("examples",package="NMsim"),...)
## file.project <- function(...)file.path("~/wdirs/NMsim/inst/examples",...)
file.mod <- file.project("nonmem/xgxr032.mod")
dat.sim <- read_fst(here::here("wdirs/NMsim/vignettes/simulate-results/dat_sim.fst"))
pars = NMreadExt(file.mod,return="pars",as.fun="data.table")[,.(parameter,par.name,i,j,iblock,blocksize,value)]
pars[, parlab := stringr::str_remove_all(string = parameter, pattern = "\\(|\\)") %>% stringr::str_replace(",", "\\_")]

simlsts.NWPRI <- NMsim(
    file.mod=file.mod,              ## Path to estimation input control stream
    data=dat.sim                    ## simulation input data
   ,dir.sims=here::here("wdirs/NMsim/devel/NMsim_vignette/tmp") ## where to store temporary simulation files
   ,dir.res="simulate-results"      ## where to store simulation results files
   ,modify.model = list(
      # name the THETAS/OMEGAS/SIGMAS in $ERROR so we can compare the distributions to other methods. 
      ERROR = function(.x) c(  .x,   paste0(pars$parlab, " = ", pars$par.name) )
   )
   ,table.vars = paste0( "PRED IPRED Y ", paste0(pars$parlab, collapse = " ") )   
   ,method.sim=NMsim_NWPRI         ## Var-Cov parameter sampling
   ,name.sim="NWPRI"               ## a recognizable directory name
   ,subproblems=500                       ## sampling 500 models
   ,sge=FALSE                        ## run simulations in parallel please
)

library(tidyverse)
dplyr::select(simlsts.NWPRI, THETA1:NMREP ) %>% 
   distinct() %>% 
   pivot_longer(!NMREP) %>%
   # mutate(keep = ifelse(length(unique(value))==1, 0, 1), .by = name) %>% 
   # filter(keep==1) %>% 
   
   ggplot(aes(x = value)) + 
   geom_histogram(bins=25)+
   # geom_density() + 
   facet_wrap(~ name, scales = "free") +
   theme_bw()


### I tried changing the OMEGAPD values from 1 to 0. Same result.
NMexec("/data/home/philipde/NMsim_vignette/tmp/xgxr032_NWPRI/xgxr032_NWPRI_man.mod",sge=F)

## I dropped all the zero-valued OMEGA and SIGMA elements and recoded the numbering in the code. That worked.
NMexec("/data/home/philipde/NMsim_vignette/tmp/xgxr032_NWPRI_man2.mod",sge=F)

res <- NMscanData("/data/home/philipde/NMsim_vignette/tmp/xgxr032_NWPRI_man2.lst")

### I set them to 1e-9. That ran. 
## However, PRED does not vary
NMexec("/data/home/philipde/NMsim_vignette/tmp/xgxr032_NWPRI_man3.mod",sge=F)

allres <- NMscanData("/data/home/philipde/NMsim_vignette/tmp/xgxr032_NWPRI_man3.lst",as.fun="data.table")
allres[,LTVCL]
allres[TIME==11,]

### Then I tried to remove FIX/FIXED from $OMEGAPD
## NMexec("/data/home/philipde/NMsim_vignette/tmp/xgxr032_NWPRI_man4.mod",sge=F)

## allres <- NMscanData("/data/home/philipde/NMsim_vignette/tmp/xgxr032_NWPRI_man4.lst",as.fun="data.table")

## long format so calculations can be done by prediction type.
allresl <- melt(allres[EVID==2],
                measure.vars=c("PRED","IPRED"),
                variable.name="pred.type",
                value.name="pred.value")

allresl[pred.type=="PRED"&TIME==8,pred.value]

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
