### To read development versions of NMsim and NMdata, do this in a
### linux terminal:
## cd ~ ; mkdir wdirs
## git clone https://github.com/philipdelff/NMdata.git
## git clone https://github.com/philipdelff/NMsim.git

library(ggplot2)
library(XML)


library(devtools)
load_all("~/wdirs/NMdata")
load_all("~/wdirs/NMsim")

NMdataConf(path.nonmem="/opt/NONMEM/nm75/run/nmfe75")

file.mod <- "~/wdirs/NMsim/inst/examples/nonmem/xgxr021.mod"
file.ext <- fnExtension(file.mod,"ext")

NMreadSection(file.mod,section="THETA")
## requires NMdata >=0.1.2
## est is 2.17
NMdata:::NMreadExt(file.ext)

seq.ka <- seq(.1,10,by=.1)


for (i in seq_along(seq.ka)){
## for (i in 1:3){

    NMsim(file.mod=file.mod,
          dir.sims="~/NMsim_loglikprof",
          name.sim=paste0("llp_",sprintf(fmt="%03d",i)),
          method.sim=NMsim_asis,
          list.sections=list(THETA=function(x)sub(".*POPKA",paste("$THETA",seq.ka[i],"FIX ; LLP KA"),x)),
          sge=TRUE
         ## ,method.update.inits="nmsim"
          )
}

### get objective function values for each run
lsts <- list.files("~/NMsim_loglikprof",pattern=".*lst",recursive=TRUE,full.names=TRUE)
## avoid reading from temporary directories created by NMsim
lsts <- lsts[!grepl(".*_dir[0-9]+/",lsts)]
xmls <- fnExtension(lsts,"xml")


xmlres <- lapply(xmls,function(xml){
    XML::xmlParse(xml) |> xmlToList()
})

objv <- sapply(xmlres,function(x)x$nonmem$problem$estimation$final_objective_function |>
                                 as.numeric())

## collect KA and conditional likelihood
dt.res <- data.table(ka=seq.ka,objv=objv)

## 2.17 is quick and dirty the maxlik estimate
ggplot(dt.res,aes(ka,objv))+
    geom_point()+
    geom_vline(xintercept=2.17)
