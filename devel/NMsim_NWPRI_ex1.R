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

## reading estimated parameters to generate TABLE statements with all parameters - generally not necessary.
pars <- NMreadExt(file.mod,return="pars",as.fun="data.table")
pars[, parlab := stringr::str_remove_all(string = parameter, pattern = "\\(|\\)") %>% stringr::str_replace(",", "\\_")]

#### NMsim_NWPRI - not typical
## need a relevant simulation data set
data.sim <- fread(file.path(wdirs,"NMsim/devel/example_nonmem_models/derived_data/simulated_nonmem_dataset_mod_lorlatinib.csv"))


simres <- NMsim(file.mod,
                data=data.sim,
                method.sim=NMsim_NWPRI,
                method.execute = "nmsim",
                subproblems=500,
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

final_ests = pars %>% 
  transmute(name=parlab, value=est)
dplyr::select(simres, NMREP, THETA1:SIGMA1_1 ) %>% 
  distinct() %>% 
  pivot_longer(!NMREP) %>%
  ggplot(aes(x=value))+
  geom_histogram(bins=100)+
  geom_vline(data = final_ests, aes(xintercept=value),color="red")+
  facet_wrap(~name,scales="free")

# changed the simulation file to include OMEGADF for each OMEGA separately, even if they are within a block --> error, did not run
# changed simulation file to put all OMEGAS in block, even if just diagonal. --> ran, but same problem as before, all omegas after 2,2 are wrong. 
# added second random source to $SIMULATION --> did not help. 
# added a OMEGAPD for every estimated element of OMEGA (including diagonals and off-diagonals, separately. we have two 2x2 blocks and one diagonal --> 7 elements total) --> did not run - error.
# moved $PRIOR to before THETA. previously it was in between THETA/OMEGA/SIGMA and THETAP/OMEGAP/etc. --> did not help
# moved FIX to after the first element of block OMEGAS  
# added ITYP=2 and NSAM=1000 to  $PRIOR NWPRI --> ITYP2 produces completely inaccurate population estimates
# ITYP=1, NSAM=1000 --> population estimates incorrect as with ITYP=2
# Provide NTHETA,NETA,NEPS / NTHP,NETP,NEPP arguments to NWPRI
# set PLEV=0 instead of 0.999
# putting all omegas in a block instead of in separate blocks, set all non-estimated omegas to 1E-30. --> this gets the center of the distributions correct, but it lets the non-estimated omegas take on non-zero values which is not desired.   Additionally, this does not let us use separate degrees of freedom for each omega block, which is not desired.
NMexec(files = "/data/sandbox/trunk/analysis/NMsim/wdirs/NMsim/devel/example_nonmem_models/lorlatinib_sim_est/NMsim/mod_lorlatinib_estimate_nwpri/mod_lorlatinib_estimate_nwpri.mod", sge=F,method.execute = "nmsim")
simres4 = NMscanData("/data/sandbox/trunk/analysis/NMsim/wdirs/NMsim/devel/example_nonmem_models/lorlatinib_sim_est/NMsim/mod_lorlatinib_estimate_nwpri/mod_lorlatinib_estimate_nwpri.mod")

dplyr::select(simres4, NMREP, THETA1:SIGMA1_1 ) %>% 
  distinct() %>% 
  pivot_longer(!NMREP) %>%
  ggplot(aes(x=value))+
  geom_histogram(bins=100)+
  geom_vline(data = final_ests, aes(xintercept=value),color="red")+
  facet_wrap(~name,scales="free")

simres2 <- NMsim(file.mod,
                data=data.sim,
                method.sim=NMsim_NWPRI,
                PLEV = 0.90,
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

simres3 <- NMsim(file.mod,
                 data=data.sim,
                 method.sim=NMsim_NWPRI,
                 PLEV = 0.99999,
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







### Lets compare to Simpar ----------------------
## Clear the environment
rm(list = ls())
graphics.off()
options(stringsAsFactors = FALSE)

library(dplyr)
library(ggplot2)
library(magrittr)
library(metrumrg)
library(mrgsolve)
library(simpar)
library(dplyr)
library(tidyr)

setwd("/data/sandbox/trunk/analysis/NMsim/wdirs")
devtools::load_all("./NMdata")
devtools::load_all("./NMsim")
set.seed(-1)

NMdataConf(as.fun = "data.table")

## load the final parameters
tbl.par = NMreadExt("./NMsim/devel/example_nonmem_models/lorlatinib_sim_est/mod_lorlatinib_estimate.ext")
tbl.par[par.type %in% c("OMEGA","SIGMA"), DF:= floor(2*(est/se)^2 +1)]
tbl.par[par.type %in% c("OMEGA","SIGMA"), DF := ifelse(FIX==1, blocksize, DF)]

## read in the covariance matrix, which provides uncertainty for the THETAs
tbl.cov = NMreadCov("./NMsim/devel/example_nonmem_models/lorlatinib_sim_est/mod_lorlatinib_estimate.cov")

thetas <- c(as.numeric(tbl.par[par.type=="THETA"]$est))
covars <- data.matrix(tbl.cov[stringr::str_detect(colnames(tbl.cov), "THETA"),stringr::str_detect(rownames(tbl.cov), "THETA")])

## diagonal OMEGA matrix and its degrees of freedom (odfs)
# omegas <- as.list(diag(bmat(tbl.p[1,grep("OMEGA",colnames(tbl.p))])))
## for a block OMEGA matrix, select the min of the diagonal entries
#odfs <- min(diag(bmat(tbl.n[1,grep("OMEGA",colnames(tbl.n))])))
tbl.par[par.type=="OMEGA" & i==j, DF := min(DF), by = iblock]
omegas = tbl.par[par.type=="OMEGA"& !is.na(iblock)] %>% split(by="iblock")
omegamats = lapply(omegas, function(.x) mrgsolve::bmat(.x$est))
odfs = lapply(omegas, function(.x) .x[i==j,min(DF)])

## for a diagonal SIGMA matrix, generate a list with each diagonal element and its degrees of freedom (sdfs)
tbl.par[par.type=="SIGMA" & i==j, DF := min(DF), by = iblock]
sigmas = tbl.par[par.type=="SIGMA"& !is.na(iblock)] %>% split(by="iblock")
sigmamats = lapply(sigmas, function(.x) mrgsolve::bmat(.x$est))
sdfs = lapply(sigmas, function(.x) .x[i==j,min(DF)])


# sigmas <- as.list(diag(bmat(tbl.p[1,grep("SIGMA",colnames(tbl.p))])))
# sdfs <- as.list(diag(bmat(tbl.n[1,grep("SIGMA",colnames(tbl.n))])))

n.theta <- length(thetas)

tbl.simpar <- simpar(
  nsim = 1000,
  theta = thetas,
  covar = covars,
  omega = omegamats,
  odf = odfs,
  sigma = sigmamats,
  sdf = sdfs) %>%
  as.data.frame

write.csv(tbl.simpar,file="sample_cov.csv",row.names=F,quote=F)

#TODO: check what happens when we change ODF - does omega distribution widen or shrink to zero?
final_ests = tbl.par %>% 
  mutate(name = stringr::str_extract(parameter, "^TH|^OM|^SIG")) %>% 
  mutate(name = stringr::str_replace(name, "SIG", "SG")) %>% 
  mutate(name = ifelse(is.na(j), paste0(name, ".",i), paste0(name, i, ".", j))) %>% 
  transmute(name, value=est)
tbl.simpar %>% 
  pivot_longer(cols = everything()) %>% 
  ggplot(aes(x=value))+
  geom_histogram(bins=100)+
  geom_vline(data = final_ests, aes(xintercept=value),color="red")+
  facet_wrap(~name,scales="free")
