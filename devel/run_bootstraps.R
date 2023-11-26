setwd("~/wdirs/NMsim/inst/examples/nonmem")
NMdataConf(as.fun="data.table")

script <- "run_bootstraps.R" 

mod.bs <- "bs1_021.mod"

##### filter according to nonmem filters
inp.bs <- NMscanInput("xgxr021.mod",apply.filters=TRUE,translate=TRUE)
inp.bs
## save that 
nmtext <- NMwriteData(inp.bs , file="../data/xgxr021_bs.csv",script=script,
                      args.NMgenText=list(dir.data="../data"))
#### copy .mod to a bootstrap version with updated initial values
system('update_inits xgxr021.mod  --output_model="bs1_021.mod"')

NMwriteSection(mod.bs,list.sections=nmtext)
#### If removing IGN/ACCEPT statements (not necessary), remember to keep IGN=@
#### remove $COV and $TABLE
NMwriteSection(mod.bs,list.sections=list(COV="",TABLE=""))
#### test .mod and data before submitting to queue system
NMexec(mod.bs,sge=FALSE)
#### Preferably run the model before doing the bootstrap

system("bootstrap -run_on_sge -samples=1000 -threads=250 -dir=bs1_021_N1000 -seed=99521 bs1_021.mod" )

