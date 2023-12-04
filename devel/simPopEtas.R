
if(F){
### prepare with something like this
    seed <- 35761
    Nsubjs <- 1e4
    suffix.sim <- paste0("NMsimPop")
    
    file.phi.out <- fnAppend(fnExtension(file.mod,".phi"),suffix.sim)
}

simPopEtas <- function(file.mod,N,seed){
    
    ## dt.sim <- NMcreateDoses(AMT=100,TIME=1,RATE=-2,CMT=1)
    dt.sim <- NMcreateDoses(AMT=100,TIME=1,CMT=1)
    dt.sim[,`:=`(DV=NA)]
    dt.sim <- addTAPD(dt.sim)

    ## NMcheckData(dt.sim)

    pars <- NMdata:::NMreadExt(fnExtension(file.mod,"ext"),return="pars")
    Netas <- pars[par.type=="OMEGA",max(i)]
    lines.pk <- c("$PK",sprintf("var%s=ETA(%s)",1:Netas,1:Netas))
    
    
    simres <- NMsim(file.mod=file.mod,
                    data=dt.sim,
                    dir.sims="simulations",
                    name.sim="sim_population",
                    subproblems=N,
                    ## text.table="ETAS(1:LAST) NOPRINT NOAPPEND",
                    table.vars="ETAS(1:LAST)",
                    table.options="NOPRINT NOAPPEND",
                    nmquiet=TRUE,
                    list.sections=list(PK=lines.pk,
                                       DES=function(x) "$DES DADT(1)=0",
                                       ERROR=function(x) "$ERROR Y=1",
                                       MODEL="$MODEL COMP=(DUMMY)"),
                    seed=seed
                    )

    simres[,ID:=.GRP,by=.(ID,NMREP)]

    simres[]
}
