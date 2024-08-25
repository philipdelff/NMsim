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

file.mod <- file.path(wdirs,"NMsim/devel/example_nonmem_models/lorlatinib_sim_est/mod_lorlatinib_estimate.mod")
data.sim <- fread(file.path(wdirs,"NMsim/devel/example_nonmem_models/derived_data/simulated_nonmem_dataset_mod_lorlatinib.csv"))

NMreadSection(file.mod,section="OMEGA")

quiet <- NMreadSection(file.mod,keep.empty=F)[c("THETA","OMEGA","SIGMA")] |> lapply(function(x)cat(paste(paste(x,collapse="\n"),"\n\n")))

pars <- NMreadExt(file.mod,return="pars",as.fun="data.table")
pars[, parlab := stringr::str_remove_all(string = parameter, pattern = "\\(|\\)") %>% stringr::str_replace(",", "\\_")]

### NMreadExt() test end

### debug typicalize()
file.mod <- file.path(wdirs,"NMsim/devel/example_nonmem_models/lorlatinib_sim_est/mod_lorlatinib_estimate.mod")
### this messes up the example control stream - dont do it
## NMsim:::typicalize(file.mod=file.mod,file.sim=file.mod)

#### NMsim_NWPRI - not typical
## need a relevant simulation data set
data.sim <- fread(file.path(wdirs,"NMsim/devel/example_nonmem_models/derived_data/simulated_nonmem_dataset_mod_lorlatinib.csv"))


simres <- NMsim(file.mod,
                data=data.sim,
                method.sim=NMsim_NWPRI,
                method.execute = "nmsim",
                subproblems=1000,
                name.sim="nwpri",
                modify.model = list(
                                        # name the THETAS/OMEGAS/SIGMAS in $ERROR so we can compare the distributions to other methods. 
                    ERROR = add(paste0(pars$parlab, " = ", pars$par.name))
                ),
                table.vars = paste0(
                    "PRED IPRED Y ",
                    paste0(pars$parlab, collapse = " ")
                ),
                nmquiet=FALSE
                )


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

simpars.l <- findCovs(simres,by="NMREP") |>
    melt(measure.vars=pars$parlab)
sumpars <- simpars.l[,.(mean=mean(value),sd=sd(value),median=median(value)),by=.(parlab=variable)]
dt.pars <- mergeCheck(sumpars,pars[,.(parlab,val.ext=value,se.ext=se,iblock,FIX)],by=cc(parlab))[
   ,diff.median:=percent(abs((median-val.ext)/val.ext))][
   ,diff.mean:=percent(abs((mean-val.ext)/val.ext))]
dt.pars[FIX!=1,!("FIX")]


### "typical" sim with variabiliity

simres.typ <- NMsim(file.mod,
                    data=data.sim,
                    method.sim=NMsim_NWPRI,
                    method.execute = "nmsim",
                    name.sim="nwpri_typ",
                    subproblems=1000,
                    typical=TRUE,
                    modify.model = list(
                        ## name the THETAS/OMEGAS/SIGMAS in $ERROR so we can compare the distributions to other methods. 
                        ERROR = add(paste0(pars$parlab, " = ", pars$par.name))
                    ),
                    table.vars = paste0(
                        "PRED IPRED Y ",
                        paste0(pars$parlab, collapse = " ")
                    )
                   ,nmquiet=FALSE
                    )


dplyr::select(simres.typ, THETA1:NMREP ) %>% 
    distinct() %>% 
    pivot_longer(!NMREP) %>%
                                        # mutate(keep = ifelse(length(unique(value))==1, 0, 1), .by = name) %>% 
                                        # filter(keep==1) %>% 
    ggplot(aes(x = value)) + 
    geom_histogram(bins=25)+
                                        # geom_density() + 
    facet_wrap(~ name, scales = "free") +
    theme_bw()

### Perfect
simpars.l <- findCovs(simres.typ,by="NMREP") |>
    melt(measure.vars=pars$parlab)
sumpars <- simpars.l[,.(mean=mean(value),sd=sd(value),median=median(value)),by=.(parlab=variable)]
dt.pars <- mergeCheck(sumpars,pars[,.(parlab,val.ext=value,se.ext=se,FIX)],by="parlab")[
   ,diff.median:=percent(abs((median-val.ext)/val.ext))][
   ,diff.mean:=percent(abs((mean-val.ext)/val.ext))]
dt.pars[FIX!=1,!("FIX")]




#### The model shown in the NMsim-ParamUncertain vignette

file.project <- function(...)file.path(system.file("examples",package="NMsim"),...)
## file.project <- function(...)file.path("~/wdirs/NMsim/inst/examples",...)
file.mod <- file.project("nonmem/xgxr032.mod")
## dat.sim <- read_fst(here::here("wdirs/NMsim/vignettes/simulate-results/dat_sim.fst"))
dat.sim <- read_fst(file.path(wdirs,"NMsim/vignettes/simulate-results/dat_sim.fst"))
## pars = NMreadExt(file.mod,return="pars",as.fun="data.table")[,.(parameter,par.name,i,j,iblock,blocksize,value)]
pars = NMreadExt(file.mod,return="pars",as.fun="data.table")
pars[value!=0]
pars[, parlab := stringr::str_remove_all(string = parameter, pattern = "\\(|\\)") %>% stringr::str_replace(",", "\\_")]

simres.2 <- NMsim(
    file.mod=file.mod,              ## Path to estimation input control stream
    data=dat.sim                    ## simulation input data
   ,dir.sims=file.path("~/NMsim_vignette/tmp") ## where to store temporary simulation files
   ,dir.res="simulate-results"      ## where to store simulation results files
   ,modify.model = list(
                                        # name the THETAS/OMEGAS/SIGMAS in $ERROR so we can compare the distributions to other methods. 
        ## ERROR = function(.x) c(  .x,   paste0(pars$parlab, " = ", pars$par.name) )
        ERROR = add( paste0(pars$parlab, " = ", pars$par.name) )
    )
   ,table.vars = paste0( "PRED IPRED Y ", paste0(pars$parlab, collapse = " ") )   
   ,method.sim=NMsim_NWPRI         ## Var-Cov parameter sampling
   ,name.sim="NWPRI"               ## a recognizable directory name
   ,subproblems=1000                 ## sampling 500 models
   ,sge=FALSE                      ## run simulations in parallel please
   ,nmquiet=T
)


#### Notice dist of OMEGA(2,2). It doesn't match the estimate. The
#### estimate is 0.17. But mean of sim is ~0. I can't tell from the
#### sim control stream that something should be wrong.
sim.lst <- "~/NMsim_vignette/tmp/xgxr032_NWPRI/xgxr032_NWPRI.lst"
NMreadSection(sim.lst,section=c("omega"))
NMreadSection(sim.lst,section=c("omegap"))
NMreadSection(sim.lst,section=c("omegapd"))


pars[value!=0]
library(tidyverse)
dplyr::select(simres.2, THETA1:NMREP ) %>% 
    distinct() %>% 
    pivot_longer(!NMREP) %>%
                                        # mutate(keep = ifelse(length(unique(value))==1, 0, 1), .by = name) %>% 
                                        # filter(keep==1) %>%    
    ggplot(aes(x = value)) + 
    geom_histogram(bins=25)+
                                        # geom_density() + 
    facet_wrap(~ name, scales = "free") +
    theme_bw()


simpars.l <- findCovs(simres.2,by="NMREP") |>
    melt(measure.vars=pars$parlab)
sumpars <- simpars.l[,.(mean=mean(value),sd=sd(value),median=median(value)),by=.(parlab=variable)]
dt.pars <- mergeCheck(sumpars,pars[,.(parlab,val.ext=value,se.ext=se,FIX)],by="parlab")[
   ,diff.median:=percent(abs((median-val.ext)/val.ext))][
   ,diff.mean:=percent(abs((mean-val.ext)/val.ext))]
dt.pars[FIX!=1,!("FIX")]


### vignette example, typical=TRUE

simres.2.typ <- NMsim(
    file.mod=file.mod,              ## Path to estimation input control stream
    data=dat.sim                    ## simulation input data
   ,dir.sims=file.path("~/NMsim_vignette/tmp") ## where to store temporary simulation files
   ,dir.res="simulate-results"      ## where to store simulation results files
   ,modify.model = list(
                                        # name the THETAS/OMEGAS/SIGMAS in $ERROR so we can compare the distributions to other methods. 
        ## ERROR = function(.x) c(  .x,   paste0(pars$parlab, " = ", pars$par.name) )
        ERROR = add( paste0(pars$parlab, " = ", pars$par.name) )
    )
   ,table.vars = paste0( "PRED IPRED Y ", paste0(pars$parlab, collapse = " ") )   
   ,method.sim=NMsim_NWPRI         ## Var-Cov parameter sampling
   ,name.sim="NWPRI"               ## a recognizable directory name
   ,subproblems=1000                 ## sampling 500 models
   ,sge=FALSE                      ## run simulations in parallel please
   ,typical=TRUE
   ,nmquiet=T
)


### Perfect
simpars.l <- findCovs(simres.2.typ,by="NMREP") |>
    melt(measure.vars=pars$parlab)
sumpars <- simpars.l[,.(mean=mean(value),sd=sd(value),median=median(value)),by=.(parlab=variable)]
dt.pars <- mergeCheck(sumpars,pars[,.(parlab,val.ext=value,se.ext=se,FIX)],by="parlab")[
   ,diff.median:=percent(abs((median-val.ext)/val.ext))][
   ,diff.mean:=percent(abs((mean-val.ext)/val.ext))]
dt.pars[FIX!=1,!("FIX")]
