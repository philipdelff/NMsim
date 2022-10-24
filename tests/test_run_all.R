library(devtools)
load_all("~/wdirs/NMexec",export_all=F)


## lsts.all <- fun.find.models()
lsts.all <- list.files("~/wdirs/NMdata/inst/examples/nonmem/",pattern=".+\\.mod",full.names=T)

load_all("~/wdirs/NMexec",export_all=F)
findUpdated(lsts.all)

load_all("~/wdirs/NMexec",export_all=F)
NMexec(dir="~/wdirs/NMdata/inst/examples/nonmem/")
