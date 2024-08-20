
library(devtools)
                                        # library(NMdata)
library(here)

## devtools::load_all(here::here("wdirs/NMdata"))
## devtools::load_all(here::here("NMsim"))

if(F){
    devtools::load_all("~/wdirs/NMdata")
    devtools::load_all("~/wdirs/NMsim")
}

devtools::load_all(here::here("wdirs/NMdata"))
devtools::load_all(here::here("wdirs/NMsim"))


NMdataConf(as.fun="data.table"
          ,col.row="REC"
          ,path.nonmem="/opt/NONMEM/nm75/run/nmfe75"
          ,dir.sims="simtmp"
          ,dir.res="simres"
           ##    ,file.data=NMsim::inputArchiveDefault
           )
                                        # file.mod <- here::here("wdirs/NMsim/inst/examples/nonmem/xgxr033.mod")
file.mod <- here::here("wdirs/NMsim/devel/example_nonmem_models/lorlatinib_sim_est/mod_lorlatinib_estimate.mod")

cov <- NMdata::NMreadCov(file.mod |> fnExtension(".cov"))

ext <- NMreadExt(file.mod)
ext

if(is.null(ext$est) & !is.null(ext$value)) {
  ext[, est:=value]
  }
setnames(ext,"est","value")

## create THETAP section
thetas <- ext[par.type=="THETA"]
setorder(thetas,i)
str.fix <- "FIX"

lines.thetap <- c("$THETAP",
                  paste(thetas[,value],str.fix)
                  )


lines.sigmap <- NMsim:::NMcreateMatLines(ext[par.type=="SIGMA"],type="SIGMA")
lines.sigmap <- sub("\\$SIGMA","\\$SIGMAP",lines.sigmap)
                                        # doesn't add "FIX" after non-block SIGMAS, need to add it.
lines.sigmap  = sapply(lines.sigmap, FUN = function(.x) ifelse((!grepl("BLOCK",.x)&!grepl("FIX",.x)), paste0(.x, " FIX"), .x), USE.NAMES = FALSE)


lines.omegap <- NMsim:::NMcreateMatLines(ext[par.type=="OMEGA"],type="OMEGA")
                                        # doesn't add "FIX" after non-block OMEGAS, need to add it.
lines.omegap = sapply(lines.omegap, FUN = function(.x) ifelse((!grepl("BLOCK",.x)&!grepl("FIX",.x)), paste0(.x, " FIX"), .x), USE.NAMES = FALSE)
lines.omegap <- sub("\\$OMEGA","\\$OMEGAP",lines.omegap)

cov.l <- mat2dt(cov)

cov.l <- NMdata:::addParType(cov.l,suffix="i")
cov.l <- NMdata:::addParType(cov.l,suffix="j")
## head(cov.l,100)

cov.l2 <- copy(cov.l)
cov.l2[,i2:=j]
cov.l2[,j:=i]
cov.l2[,i:=i2]
lines.thetapv <-
    NMsim:::NMcreateMatLines(cov.l2[par.type.i=="THETA"&par.type.j=="THETA"],type="OMEGA")
lines.thetapv <- sub("\\$OMEGA","\\$THETAPV",lines.thetapv)
                                        # format thetapv string into matrix format for readability
                                        # lines.thetapv =
                                        #   lapply(1:max(cov.l[par.type.i=="THETA"&par.type.j=="THETA"]$i), function(.x) {
                                        #   vals = cov.l[par.type.i=="THETA"&par.type.j=="THETA"][i==.x]$value
                                        #   vals = sapply(vals, function(.y) ifelse(.y<1e-9, 1.0000e-30,.y))
                                        #   vals = sapply(vals, function(.y) sprintf(.y, fmt = "%.4e"))
                                        #   paste(vals, collapse = " ")
                                        # }) %>% unlist
                                        # lines.thetapv = c("$THETAPV", lines.thetapv)

## here

load_all("~/wdirs/NMdata")
NMdataConf(as.fun="data.table")

### degrees of freedom for each block
## OMEGAPD

                                        # identify omega blocks
ext[par.type=="OMEGA",iblock:=i]

omegas = ext[par.type=="OMEGA"]

                                        #TODO: not sure if better to use !(value==0 & FIX==1) or !(value==0)   in the piece below. with FIX it will keep any estimated (but zero) off diagonals.
                                        # omega_offdiag = omegas[ i!=j & !(value==0 & FIX==1)]
omega_offdiag = omegas[ i!=j & !(value==0)]
                                        # omega_blocks = omegas[ !(value==0 &FIX==1) & i %in% unique(c(omega_offdiag$i, omega_offdiag$j))]
omega_blocks = omegas[ !(value==0 ) & i %in% unique(c(omega_offdiag$i, omega_offdiag$j))]

                                        # get non-block omegas and separate
omega_nonblocks = 
    omegas[i==j & !(i %in% omega_blocks$i)][
      , omega_type := paste0("nonblock", i)
    ][
                                        # m = 1 for non-block omegas
      , m := 1
    ] |> 
    data.table:::split.data.table(by = "i") 

if(nrow(omega_blocks)>0) {
                                        # associate individual omegas within blocks
    omega_blocklist_indices =
        lapply(
            X = omega_blocks$i,
            FUN = function(.x) {
                sort(unique(c(omega_blocks$i[which(omega_blocks$j == .x)],
                              omega_blocks$j[which(omega_blocks$i == .x)])))
            }
        ) %>% unique()
                                        #separate multiple blocks into separate matrices
    omega_block_sep_list =
        lapply(1:length(omega_blocklist_indices), function(.x) {
            dt = data.table(i = omega_blocklist_indices[[.x]], omega_type = paste0("block", .x))
            dt[, c("par.type", "m") := list("OMEGA", length(unique(i)))]
            dt.block = merge.data.table(
                omegas,
                dt,
                by = cc(i, par.type),
                all.y = TRUE,
                all.x = FALSE
            )
            dt.block = dt.block[!(value==0 & FIX==1)] # drop off-diagonals that are not estimated
            return(dt.block)
        })
    
                                        # take all omega groups into list
    omega_matrices = unlist(list(omega_nonblocks, omega_block_sep_list), recursive =
                                                                             FALSE)
    omegas_grouped = rbindlist(omega_matrices, use.names = TRUE)[order(i,j)]
    
} else {
    omega_matrices = unlist(omega_nonblocks, recursive=FALSE)
    omegas_grouped = rbindlist(omega_matrices, use.names = TRUE)[order(i,j)]
}

                                        # calculate degrees of freedom for each block/nonblock group
                                        # calculate N (can be thought of as an effective sample size, estimated from standard error of parameter estimate) and DF = degrees of freedom, based on 
                                        #  NONMEM tutorial part II, supplement 1, part C:
                                        # https://ascpt.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fpsp4.12422&file=psp412422-sup-0001-Supinfo1.pdf
                                        # only calculate for diagonal elements
omegas_grouped[i==j, N := 2*((value**2)/(se**2)) + 1]
omegas_grouped[i==j, DF := N-m-1]
                                        # DF cannot be smaller than the number of parameters in the block
omegas_grouped[, DF := ifelse(DF<m, m, DF)]
                                        # take the minimum DF per omega matrix:
omegas_grouped[, DF2 := min(DF, na.rm = TRUE), by = omega_type]

lines.omegadf = unique(omegas_grouped[,.(omega_type, DF2)])[,.(line = paste0("$OMEGAPD ", DF2, " FIXED"))]$line


## SIGMAPD
                                        # Calculate SIGMA degrees of freedom for inverse wishart. Same as for OMEGA, but the DF should not be greater than the number of observations in the dataset. Should come out correctly using the N = 2*((estimate**2))/(SE**2))+1, and DF = N-m-1 estimation equations. 
sigmas = ext[par.type %in% c("SIGMA")]
sigma_offdiag = sigmas[ i!=j & !(value==0 & FIX==1)]
sigma_blocks = sigmas[ !(value==0 & FIX==1) & i %in% unique(c(sigma_offdiag$i, sigma_offdiag$j)) ]

                                        # get non-block omegas and separate
sigma_nonblocks = 
    sigmas[i==j & !(i %in% sigma_blocks$i)][
      , omega_type := paste0("nonblock", i)
    ][
                                        # m = 1 for non-block sigmas
      , m := 1
    ] |> 
    data.table:::split.data.table(by = "i") 

if(nrow(sigma_blocks)>0) {
                                        # associate individual sigmas within blocks
    sigma_blocklist_indices =
        lapply(
            X = sigma_blocks$i,
            FUN = function(.x) {
                sort(unique(c(sigma_blocks$i[which(sigma_blocks$j == .x)],
                              sigma_blocks$j[which(sigma_blocks$i == .x)])))
            }
        ) %>% unique()
                                        #separate multiple blocks into separate matrices
    sigma_block_sep_list =
        lapply(1:length(sigma_blocklist_indices), function(.x) {
            dt = data.table(i = sigma_blocklist_indices[[.x]], omega_type = paste0("block", .x))
            dt[, c("par.type", "m") := list("SIGMA", length(unique(i)))]
            dt.block = merge.data.table(
                sigmas,
                dt,
                by = cc(i, par.type),
                all.y = TRUE,
                all.x = FALSE
            )
            dt.block = dt.block[!(value==0 & FIX==1)] # drop off-diagonals that are not estimated
            return(dt.block)
        })
                                        # take all sigma groups into list
    sigma_matrices = unlist(list(sigma_nonblocks, sigma_block_sep_list), recursive =
                                                                             FALSE)
    sigmas_grouped = data.table::rbindlist(sigma_matrices, use.names = TRUE)[order(i,j)]
    
} else {
    sigma_matrices = sigma_nonblocks
    sigmas_grouped = data.table::rbindlist(sigma_matrices, use.names = TRUE)[order(i,j)]
}

                                        # calculate degrees of freedom for each block/nonblock group
                                        # calculate N (can be thought of as an effective sample size, estimated from standard error of parameter estimate) and DF = degrees of freedom, based on 
                                        #  NONMEM tutorial part II, supplement 1, part C:
                                        # https://ascpt.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fpsp4.12422&file=psp412422-sup-0001-Supinfo1.pdf
                                        # only calculate for diagonal elements
sigmas_grouped[i==j, N := 2*((value**2)/(se**2)) + 1]
sigmas_grouped[i==j, DF := N-m-1]
                                        # DF cannot be smaller than the number of parameters in the block
                                        # for SIGMA, DF should not be smaller than total number of observations, however this should be accounted for in estimate of N above.
sigmas_grouped[, DF := ifelse(DF<m, m, DF)]
                                        # If sigma is fixed, set DF=dimension of sigma block for uninformative distribution
sigmas_grouped[, DF := ifelse(FIX==1, m, DF)]
                                        # take the minimum DF per omega matrix:
sigmas_grouped[, DF := min(DF, na.rm = TRUE), by = omega_type]

lines.sigmadf = unique(sigmas_grouped[,.(omega_type, DF)])[,.(line = paste0("$SIGMAPD ", DF, " FIXED"))]$line


lines.prior = "$PRIOR NWPRI PLEV=0.999"

                                        # this contains all of the lines we need to add to simulate with uncertainty within NONMEM.
all.lines = c(lines.prior, lines.thetap, lines.thetapv, lines.omegap, lines.omegadf, lines.sigmap, lines.sigmadf)


## Simulate
NMsim(
    file.mod = file.mod,
    name.sim = "sim_varcov_nwpri",
    dir.sims = here("simtmp"),
    method.sim = NMsim_default,
    seed.R = 1234,
    seed.nm = 1234,
    subproblems = 1000,
    nc = 16,
    modify.model = list(
                                        # name the THETAS/OMEGAS/SIGMAS in $ERROR so we can compare the distributions to other methods. 
        ERROR = function(.x)
            c(
                .x,
                paste0(thetas$parameter, " = ", thetas$par.type, "(", thetas$i, ")"),
                paste0(gsub("\\)|\\(|,", "", omegas$parameter), " = ", omegas$parameter),
                paste0(gsub("\\)|\\(|,", "", sigmas$parameter), " = ", sigmas$parameter)
            ),
                                        #TODO: ideally prepend to $SIMULATION instead of append to $SIGMA.
        SIGMA = function(.x)
            c(.x, all.lines)
    ),
                                        # SIMULATION=function(.x) c(all.lines, .x)),
                                        # add additional parameters/arguments to $SIMULATION using text.sim; TRUE=PRIOR is required
    text.sim = "TRUE=PRIOR",
    table.vars = paste0(
        "PRED IPRED Y TVCL CL TVF1 F1 ",
        paste0(thetas$parameter, collapse = " "),
        " ",
        paste0(gsub("\\)|\\(|,", "", omegas$parameter), collapse = " "),
        " ",
        paste0(gsub("\\)|\\(|,", "", sigmas$parameter), collapse = " ")
    ),
    file.res = here::here("simres/sim_varcov_nwpri.rds")
)

d = NMreadSim(here("simres/sim_varcov_nwpri.rds"))


library(tidyverse)
dplyr::select(d, NMREP, THETA1:SIGMA11) %>% 
    distinct() %>% 
    pivot_longer(!NMREP) %>%
    mutate(keep = ifelse(length(unique(value))==1, 0, 1), .by = name) %>% 
    filter(keep==1) %>% 
    
    ggplot(aes(x = value)) + 
    geom_histogram(bins=25)+
                                        # geom_density() + 
    facet_wrap(~ name, scales = "free") +
    theme_bw()

# are there different parameter values for each NMREP?
d %>% filter(ID==1,CMT==2,TIME<50) %>% ggplot(aes(x=TIME,y=IPRED,color=NMREP,group=NMREP))+geom_line()

# Is the mean of the IPREDs different across NMREP? i.e. are the ETAS distributed around different PRED?
d %>% 
  filter(EVID==0) %>% 
  summarise(PRED = median(IPRED), .by = c(NMREP, TIME,CMT)) %>% 
  ggplot(aes(x=TIME,y=PRED,color=NMREP,group=NMREP))+
  geom_line()


d %>% 
  distinct(NMREP,ID,TVF1,F1) %>% 
  mutate( mf1 = mean(F1), .by = NMREP) %>% 
  distinct(TVF1, mf1,NMREP)
