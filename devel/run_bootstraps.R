setwd("~/wdirs/NMsim/inst/examples/nonmem")
NMdataConf(as.fun="data.table")

script <- "run_bootstraps.R" 

######## 021 bootstrap
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

######## end 021 bootstrap


###### 032 bootstrap
mod.bs <- "bs1_032.mod"
file.mod <- "xgxr032.mod"
file.csv.bs <- "../data/xgxr032_bs.csv"
dir.bs <- "bs1_032_N1000"

##### filter according to nonmem filters
inp.bs <- NMscanInput(file.mod,apply.filters=TRUE,translate=TRUE)
inp.bs
## save that 
nmtext <- NMwriteData(inp.bs , file=file.csv.bs,script=script,
                      args.NMgenText=list(dir.data="../data"))
#### copy .mod to a bootstrap version with updated initial values
str.cmd <- sprintf("update_inits %s --output_model='%s'",file.mod,mod.bs)
system(str.cmd)

NMwriteSection(mod.bs,list.sections=nmtext)
#### If removing IGN/ACCEPT statements (not necessary), remember to keep IGN=@
#### remove $COV and $TABLE
NMwriteSection(mod.bs,list.sections=list(COV="",TABLE=""))
#### test .mod and data before submitting to queue system
NMexec(mod.bs,sge=FALSE)
#### Preferably run the model before doing the bootstrap

## system("bootstrap -run_on_sge -samples=1000 -threads=250 -dir=bs1_021_N1000 -seed=99521 bs1_021.mod" )
str.cmd.bs <- sprintf("bootstrap -run_on_sge -samples=1000 -threads=250 -dir=%s -seed=99521 %s" ,dir.bs,mod.bs)
system(str.cmd.bs)
####### end 032 bootstrap
