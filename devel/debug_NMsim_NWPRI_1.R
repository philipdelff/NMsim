#### testing NMdata::NMreadExt()
unloadNamespace("NMsim")
unloadNamespace("NMdata")
library(devtools)
library(here)
library(scales)
library(tidyverse)
library(stringr)


### lets try to make the directories in the script only depend on wdirs. And maybe one more for setwd if needed
## wdirs <- "/data/sandbox/trunk/analysis/NMsim/wdirs"
## setwd("/data/sandbox/trunk/analysis/NMsim/wdirs")
wdirs <- "~/wdirs"
load_all(file.path(wdirs,"NMdata"))
load_all(file.path(wdirs,"NMsim"))

NMdataConf(path.nonmem="/opt/NONMEM/nm75/run/nmfe75",
           as.fun="data.table")


printSection <- function(section) {
    res <- lapply(section,function(x)cat(paste(paste(x,collapse="\n"),"\n\n")))
}

##### Choose a model

### The model shown in the NMsim-ParamUncertain vignette
file.project <- function(...)file.path(system.file("examples",package="NMsim"),...)
file.mod <- file.project("nonmem/xgxr032.mod")
## dat.sim <- read_fst(here::here("wdirs/NMsim/vignettes/simulate-results/dat_sim.fst"))
data.sim <- read_fst(file.path(wdirs,"NMsim/vignettes/simulate-results/dat_sim.fst"))

### lorlatinib model
file.mod <- file.path(wdirs,"NMsim/devel/example_nonmem_models/lorlatinib_sim_est/mod_lorlatinib_estimate.mod")
data.sim <- fread(file.path(wdirs,"NMsim/devel/example_nonmem_models/derived_data/simulated_nonmem_dataset_mod_lorlatinib.csv"))


### User input done

## pars = NMreadExt(file.mod,return="pars",as.fun="data.table")[,.(parameter,par.name,i,j,iblock,blocksize,value)]
pars <- NMreadExt(file.mod,return="pars",as.fun="data.table")
pars[value!=0]
pars[, parlab := stringr::str_remove_all(string = parameter, pattern = "\\(|\\)") %>% stringr::str_replace(",", "\\_")]

simres <- NMsim(
    file.mod=file.mod,              ## Path to estimation input control stream
    data=data.sim                    ## simulation input data
   ,dir.sims="simtmp_NWPRI" ## where to store temporary simulation files
    ##   ,dir.res="simulate-results"      ## where to store simulation results files
   ,modify.model = list(
        ## name the THETAS/OMEGAS/SIGMAS in $ERROR so we can compare
        ## the distributions to other methods.
        ERROR = add( paste0(pars$parlab, " = ", pars$par.name) )
    )
   ,table.vars = paste0( "PRED IPRED Y ", paste0(pars$parlab, collapse = " ") )   
   ,method.sim=NMsim_NWPRI         ## Var-Cov parameter sampling
   ,name.sim="NWPRI"               ## a recognizable directory name
   ,subproblems=250                 ## sampling multiple models
   ,sge=FALSE                      ## run simulations in parallel please
   ,nmquiet=T
)



#### Notice dist of OMEGA(2,2). It doesn't match the estimate. The
#### estimate is 0.17. But mean of sim is ~0. I can't tell from the
#### sim control stream that something should be wrong.
##sim.lst <- "simtmp_NWPRI/xgxr032_NWPRI/xgxr032_NWPRI.lst"
sim.lst <- attributes( simres)$NMsimModTab$path.sim.lst


pars[,parlab2:=paste(parlab,"Est =",signif(est,3),"SE =",signif(se,3))]
pars[FIX==1,parlab2:=paste(parlab,"Est =",signif(est,3), "(Fixed)")]
pars[par.type=="OMEGA"&FIX!=1]

## $OMEGA and $OMEGAP aligns with pars$value
NMreadSection(sim.lst,section=c("omega")) |> printSection()
NMreadSection(sim.lst,section=c("omegap")) |> printSection()
## OMEGAPD aligns wit DF2
NMsim:::NWPRI_df(pars)
NMreadSection(sim.lst,section=c("omegapd")) |> printSection()

NMreadSection(sim.lst,section=c("sigmapd")) |> printSection()

vars <- intersect(colnames(simres),pars$parlab)

findCovs(simres,by="NMREP")[,c("NMREP",vars),with=F] |>
    melt(measure.vars=vars) |>
    mergeCheck(unique(pars[,.(parlab,parlab2)]),by.x="variable",by.y="parlab") |>
        ggplot(aes(x = value)) + 
    geom_histogram(bins=25)+
    facet_wrap(~ parlab2, scales = "free") +
    theme_bw()

## table with estimates, se and sd. not sure this is needed
simpars.l <- findCovs(simres,by="NMREP") |>
    melt(measure.vars=pars$parlab)
sumpars <- simpars.l[,.(mean=mean(value),sd=sd(value),median=median(value)),by=.(parlab=variable)]
dt.pars <- mergeCheck(sumpars,pars[,.(parlab,val.ext=value,se.ext=se,FIX)],by="parlab")[
   ,diff.median:=percent(abs((median-val.ext)/val.ext))][
   ,diff.mean:=percent(abs((mean-val.ext)/val.ext))]
dt.pars[FIX!=1,!("FIX")]


## manual edits of the sim control stream
NMexec("simtmp_NWPRI/mod_lorlatinib_estimate_NWPRI/mod_lorlatinib_estimate_NWPRI_2.mod",sge=F)
simres <- NMscanData("simtmp_NWPRI/mod_lorlatinib_estimate_NWPRI/mod_lorlatinib_estimate_NWPRI_2.lst")
