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
OMEGAPD
SIGMAPD

findBlocks(ext[par.type=="OMEGA"])

modify.model=list(SIGMA=add("$PRIOR NWPRI PLEV=0.999"

                            
$THETAP
 ( 3.94318E-01 FIXED)
 ( 4.10202E+00 FIXED)
 ( 5.27965E+00 FIXED)
 ( 8.33984E+01 FIXED)
 ( 1.61527E+01 FIXED)
 ( 5.95051E+01 FIXED)
 (-3.85801E-01 FIXED)
 ( 0.00000E+00 FIXED)
 ( 1.00000E+00 FIXED)
 ( 0.00000E+00 FIXED)
 (-1.99663E-02 FIXED)

$SIGMAP BLOCK(1)
  2.01863E-01 FIXED

$SIGMAP BLOCK(1)
  1.04306E-02 FIXED


$THETAPV BLOCK(11)
  1.02414E-03 FIXED
 -2.73904E-03  1.15916E-01
  2.27301E-04 -1.47761E-03  1.48532E-02
  2.03512E-02 -2.47167E-02  1.18906E-01  7.40825E+00
 -8.05802E-05 -8.96352E-02 -2.37892E-02 -1.32688E+00  1.24652E+00
 -1.05210E-02  3.26323E-02 -3.77844E-02 -2.65132E+00  2.09475E+00  8.42889E+00
  4.83576E-04 -9.02173E-03  1.38915E-03  5.05677E-02 -3.56967E-03 -2.57899E-02  1.54981E-03
  1.00000E-30  1.00000E-30  1.00000E-30  1.00000E-30  1.00000E-30  1.00000E-30  1.00000E-30  1.00000E-29
  1.00000E-30  1.00000E-30  1.00000E-30  1.00000E-30  1.00000E-30  1.00000E-30  1.00000E-30  1.00000E-30  1.00000E-29
  1.00000E-30  1.00000E-30  1.00000E-30  1.00000E-30  1.00000E-30  1.00000E-30  1.00000E-30  1.00000E-30  1.00000E-30  1.00000E-29
  2.26009E-06 -2.15627E-06  1.84116E-06  6.34124E-05 -1.16694E-05 -3.37413E-05  1.11806E-06  1.00000E-30  1.00000E-30  1.00000E-30  2.15663E-08

$OMEGAPD 92.8996749427102 FIXED

$OMEGAPD 65.2880264622316 FIXED

;DF from OMEGA(3,3): 116.924694600055
;DF from OMEGA(4,4): 79.3782626257959
$OMEGAPD 79.3782626257959 FIXED

$OMEGAPD 1 FIXED

;DF from OMEGA(6,6): 36.9583435287946
$OMEGAPD 36.9583435287946 FIXED

$OMEGAPD 1 FIXED

$SIGMAPD 58.4501338947969 FIXED

$SIGMAPD 341.676016955025 FIXED
