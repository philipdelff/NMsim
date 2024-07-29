library(devtools)
load_all("~/wdirs/NMdata")
NMdataConf(as.fun="data.table")

file.mod <- "~/wdirs/NMsim/inst/examples/nonmem/xgxr033.mod"

cov <- NMreadCov(file.mod)
ext <- NMreadExt(file.mod)
ext

setnames(ext,"est","value")

## create THETAP section
thetas <- ext[par.type=="THETA"]
setorder(thetas,i)
str.fix <- "FIX"

lines.thetap <- c("$THETAP",
                 paste(thetas[,value],str.fix)
                 )


lines.sigmap <- NMcreateMatLines(ext[par.type=="SIGMA"],type="SIGMA")
lines.sigmap <- sub("\\$SIGMA","\\$SIGMAP",lines.sigmap)

lines.omegap <- NMcreateMatLines(ext[par.type=="OMEGA"],type="OMEGA")
lines.omegap <- sub("\\$OMEGA","\\$OMEGAP",lines.omegap)

cov.l <- mat2dt(cov)
cov.l <- addParType(cov.l,suffix="i")
cov.l <- addParType(cov.l,suffix="j")
## head(cov.l,100)

cov.l2 <- copy(cov.l)
cov.l2[,i2:=j]
cov.l2[,j:=i]
cov.l2[,i:=i2]
lines.thetapv <-
    NMcreateMatLines(cov.l2[par.type.i=="THETA"&par.type.j=="THETA"],type="OMEGA")
lines.thetapv <- sub("\\$OMEGA","\\$THETAPV",lines.omegap)

## here

load_all("~/wdirs/NMdata")
NMdataConf(as.fun="data.table")
### degrees of freedom for each block
## OMEGAPD
## SIGMAPD

## $SIGMAPD 58.4501338947969 FIXED

## $SIGMAPD 341.676016955025 FIXED
