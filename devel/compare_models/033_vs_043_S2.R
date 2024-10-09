### compare 033 and 043
library(data.table)
library(devtools)
load_all("~/wdirs/NMdata")
NMdataConf(path.nonmem="/opt/NONMEM/nm75/run/nmfe75"
           ,as.fun="data.table")

files <- c("../nonmem/xgxr033.mod"
          ,"../nonmem/xgxr043.mod"
          ## ,"../nonmem/xgxr044.mod"
          ## ,"../nonmem/xgxr045.mod"
          ,"../nonmem/xgxr046.mod"
           )

exts <- NMreadExt(file=files)

dcast(exts,parameter~model,value.var="value")
NMreadExt(file=files,return="OBJ")


res <- NMscanMultiple(files=files)

### Predictions are identical
load_all("~/wdirs/NMgof")
NMplotIndProfs(res[ID>170],par.prof="model")

dcast(res[ID==180],TIME~model,value.var="IPRED")


##
pars <- findCovs(res,by="model")
cnames <- colnames(pars)
pnames <- cnames[grepl("^LTV",cnames)]
p.l <- melt(pars,measure.vars=c(pnames),id.vars="model")
p.l[,tvval.lin:=exp(value)]
p.l[,par.lin:=sub("LTV","",variable)]
dcast(p.l,par.lin~model,value.var="tvval.lin")
