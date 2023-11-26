library(xgxr)
pkpd <- as.data.table(case1_pkpd)
pd <- pkpd[NAME=="PD - Continuous"]
dim(pd)
NMwriteData(pd,file="~/wdirs/NMsim/inst/examples/data/xgxr_pd.rds",formats="rds")
