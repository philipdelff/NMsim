### compare 033 and 043
library(data.table)
library(devtools)
load_all("~/wdirs/NMdata")
NMdataConf(path.nonmem="/opt/NONMEM/nm75/run/nmfe75"
           ,as.fun="data.table")

files <- c("../nonmem/xgxr033.mod","../nonmem/xgxr043.mod")
exts <- NMreadExt(file=files)

dcast(exts,parameter~model,value.var="value")

res <- NMscanMultiple(files=files)

### Predictions are identical
load_all("~/wdirs/NMgof")
NMplotIndProfs(res,par.prof="model")

dcast(res[ID==180],TIME~model,value.var="IPRED")
