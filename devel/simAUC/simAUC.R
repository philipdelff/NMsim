load_all("~/wdirs/NMsim")
file.mod <- "~/wdirs/NMsim/inst/examples/nonmem/xgxr046.mod"

## NMreadSection(file.mod,section="MODEL")

Ncomps <- function(file.mod){
    des <- NMreadSection(file.mod,section="DES",keep.comments = FALSE)
    pattern <- "DADT *\\( *[0-9]+ *\\)"
    m <- regexpr(pattern, des)
    strs <- regmatches(des, m)
    Ndes <- max(as.numeric(sub("DADT\\(([0-9]*)\\)","\\1",strs)))
    Ndes
}

dat.sim <- readRDS(system.file("examples/derived/dat_sim1.rds",
                                package="NMsim")) 


dat.sim[,AMT:=AMT*1000]

sres1 <- NMsim(file.mod=file.mod,
      data=dat.sim,
      modify.model=list(MODEL=add("COMP=(AUC)"),
                        DES=add("DADT(3)=A(2)/V2"),
                        ERROR=add("AUCCUM=A(3)",
                                  "IF(NEWIND.NE.2) OLDAUCCUM=0",
                                  "AUC = AUCCUM-OLDAUCCUM",
                                  "OLDAUCCUM = AUCCUM"
                                  )),
      table.vars=cc(PRED,IPRED,AUC)
      ,typical=TRUE
      )

melt(sres1,measure.vars=cc(IPRED,AUC))|>
    ggplot(aes(TIME,value))+geom_point()+
    facet_wrap(~variable)

dat.sim2 <- addEVID2(dat.sim[EVID==1],CMT=2,time.sim=seq(0,by=24,length.out=9))

sres2 <- NMsim(file.mod=file.mod,
      data=dat.sim2,
      modify.model=list(MODEL=add("COMP=(AUC)"),
                        DES=add("DADT(3)=A(2)/V2"),
                        ERROR=add("AUCCUM=A(3)",
                                  "IF(NEWIND.NE.2) OLDAUCCUM=0",
                                  "AUC = AUCCUM-OLDAUCCUM",
                                  "OLDAUCCUM = AUCCUM"
                                  )),
      table.vars=cc(PRED,IPRED,AUC)
     ,typical=TRUE
      )

melt(sres2,measure.vars=cc(IPRED,AUC))|>
    ggplot(aes(TIME,value))+geom_point()+
    facet_wrap(~variable)

###### Cmax

sres3 <- NMsim(file.mod=file.mod,
      data=dat.sim2,
      modify.model=list(MODEL=add("$ABB COMRES=2"),
                        PK=add(##"IF(NEWIND.LE.1) THEN",
                               "COM(1)=0",
                               "COM(2)=0"
                            ##,"ENDIF"
                            ),
                        DES=add("CONC=A(2)/V2",
                                "IF(CONC.GT.COM(1)) THEN",
                                "COM(1)=CONC",
                                "COM(2)=T",
                                "ENDIF"),
                        ERROR=add("CMAX=COM(1)",
                                  "TMAX=COM(2)"
                                  )),
      table.vars=cc(PRED,IPRED,CMAX,TMAX)
     ,typical=TRUE
      )

melt(sres3,measure.vars=cc(IPRED,CMAX,TMAX))|>
    ggplot(aes(TIME,value))+geom_point()+
    facet_wrap(~variable,scales="free")
