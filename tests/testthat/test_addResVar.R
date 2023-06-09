if(FALSE){

    data <- NMscanData(system.file("examples/simulations/xgxr014_singlesubj1.lst",package="NMsim"),merge.by.row=FALSE,as.fun="data.table")
    path.ext <- system.file("examples/nonmem/xgxr014.ext",package="NMsim")
    prop <- 1
    add <- 2
    par.type <- "SIGMA"
    log <- FALSE

    ## additional args
    scale.par <- "var"

    data <- NMscanData(system.file("examples/simulations/xgxr014_singlesubj1.lst",package="NMsim"),merge.by.row=FALSE,as.fun="data.table")
    data1 <- addResVar(data=data
                      ,path.ext = system.file("examples/nonmem/xgxr014.ext",package="NMsim")
                      ,prop = 1
                      ,add = 2
                      ,par.type = "SIGMA"
                      ,log = FALSE)

    ggplot(data1,aes(TIME,IPRED,colour=dose)) + geom_line() +
        geom_point(aes(TIME,IPREDVAR))+
        facet_wrap(~regimen,scales="free")

}
