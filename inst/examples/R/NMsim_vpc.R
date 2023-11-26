file.lst <- fnExtension(file.mod,"lst")


### function to transform predictions to linear scale in case model is on log scale
exp10 <- function(x)10^x
## assuming model is on linear scale - identity means no transformation
fun.trans <- function(x)x


### make sims for VPC.We need to simulate with the input dataset used
### in the estimation.
## Reading the input data set is needed until NMsim 0.0.5. will be
## simpler in future versions of NMsim/NMdata.  data.inp <-
## NMscanInput(file.mod,translate=TRUE,recover.cols=FALSE,
## apply.filters=TRUE)

## dims(data.inp)
## NMcheckData(data.inp)

if(F){
## using NMsim for simulation

    set.seed(9857)    
    simpaths <- NMsim(file.mod,
                    ## data=data.inp[EVID!=2],
                    ## we don't need much for the output table. Notice
                    ## Y is sim with residual variability
                    text.table="ID TIME EVID PRED IPRED Y NOAPPEND NOPRINT",
                    dir.sims="simulations",
                    name.sim="vpc3",
                    ## We do 1000 reps but its slow. So just 10 on
                    ## each run and 100 runs.
                    subproblems=10,
                    nsims=100
                    ## run on the cluster
                    ,sge=TRUE
                    )
}

simdir <- file.path("simulations",paste(fnExtension(basename(file.mod),""),"vpc3",sep="_"))

simres <- NMscanMultiple(dir=simdir,quiet=TRUE,use.input=FALSE)
cols.trans <- cc(Y,IPRED,PRED)
simres[,(cols.trans):=lapply(.SD,fun.trans),.SDcols=cols.trans]
dims(simres)

### Prepare obs data. The obs data should contain PRED for predcorr
### VPCs. So we use NMscanData to combine input and output data.
res <- NMscanData(file.lst)
data.obs <- res[EVID==0]

## ggplot(data.obs,aes(TIME,DV))+
##     geom_point()+
##     facet_wrap(~ID)

### for stratification, we need to make sure to add those variables to the sim data. 
## in case we need to create a new grouping
data.obs[,groupDose:=paste(STUDY,"-",DOSE,"mg")]
data.obs[,.N,by=.(groupDose)]

## get group into sim data
simres <- mergeCheck(simres,
                     findCovs(data.obs,by="ID")[,.(ID,groupDose,STUDY,DOSE)]
                    ,by="ID")


## run vpc
vpc1 <-
    observed(data.obs[EVID==0], x = TIME, y = DV) |>
    simulated(simres[EVID==0], y = Y) |>
    stratify(~groupDose+STUDY+DOSE) |>
    binning(bin = "ntile", nbins = 9) |>
    vpcstats()



p.vpc1 <- plot(vpc1) +
    facet_grid(STUDY~DOSE)
