##' Easily generate dosing records
##'
##' Combinations of different columns can be generated. Columns will
##' be extended by repeating last value of the column if needed in
##' order to match length of other columns.
##' @param TIME The time of the dosing events
##' @param AMT vector or dataa.frame with amounts amount
##' @param RATE Optional infusion rate
##' @param SS Optional steady-state flag
##' @param CMT Compartment number. Default is to dose into CMT=1.
##' @param EVID The event ID to use for doses. Default is to use
##'     EVID=1, but EVID might also be wanted.
##' @param addl Optinal. A list of ADDL and II that will be applied to
##'     last dose
##' @param as.fun The default is to return data as a data.frame. Pass
##'     a function (say `tibble::as_tibble`) in as.fun to convert to
##'     something else. If data.tables are wanted, use
##'     as.fun="data.table". The default can be configured using
##'     NMdataConf.
##' @details Experimental. Please check output before use. AMT, RATE,
##'     SS, II, CMT are vectors of length 1 or longer. Those not of
##'     max length 1 are repeated.  If TIME is longer than those, they
##'     are extended to match length of TIME. Allowed combinations of
##'     AMT, RATE, SS, II here:
##'     \url{https://ascpt.onlinelibrary.wiley.com/doi/10.1002/psp4.12404}
##' @return A data.frame with dosing events
##' @examples
##' library(data.table)
##' ## Users should not use setDTthreads. This is for CRAN to only use 1 core.
##' data.table::setDTthreads(1) 
##' ## arguments are expanded - makes loading easy
##' NMcreateDoses(TIME=c(0,12,24,36),AMT=c(2,1))
##' ## Different doses by covariate
##' NMcreateDoses(TIME=c(0,12,24),AMT=data.table(AMT=c(2,1,4,2),DOSE=c(1,2)))
##' ## Make Nonmem repeat the last dose. This is a total of 20 dosing events.
##' NMcreateDoses(TIME=c(0,12),AMT=c(2,1),addl=list(ADDL=c(NA,9*2),II=c(NA,12)))
##' dt.amt <- data.table(DOSE=c(100,400))
##' dt.amt[,AMT:=DOSE*1000]
##' dt.amt
##' doses.sd <- NMcreateDoses(TIME=0,AMT=dt.amt)
##' doses.sd$dose <- paste(doses.sd$DOSE,"mg")
##' doses.sd$regimen <- "SD"
##' doses.sd
##' 
##' ### multiple dose regimens with loading are easily created with NMcreateDoses too
##' ## Specifying the time points explicitly
##' dt.amt <- data.table(AMT=c(200,100,800,400)*1000,DOSE=c(100,100,400,400))
##' doses.md.1 <- NMcreateDoses(TIME=seq(0,by=24,length.out=7),AMT=dt.amt)
##' doses.md.1$dose <- paste(doses.md.1$DOSE,"mg")
##' doses.md.1$regimen <- "QD"
##' doses.md.1
##' ## or using ADDL+II
##' dt.amt <- data.table(AMT=c(200,100,800,400)*1000,DOSE=c(100,100,400,400))
##' doses.md.2 <- NMcreateDoses(TIME=c(0,24),AMT=dt.amt,addl=data.table(ADDL=c(0,5),II=c(0,24)))
##' doses.md.2$dose <- paste(doses.md.2$DOSE,"mg")
##' doses.md.2$regimen <- "QD"
##' doses.md.2
##' @import data.table
##' @import NMdata
##' @export





NMcreateDoses <- function(TIME, AMT=NULL, RATE=NULL, SS=NULL, CMT=1, EVID=1, addl=NULL, as.fun){
    
    ## if(debug) browser()

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    . <- NULL
    ID <- NULL
    Nna <- NULL
    max.length <- NULL
    variable <- NULL
    ROW <- NULL
    as.formula <- NULL
    MDV <- NULL
    
### Section end: Dummy variables, only not to get NOTE's in pacakge checks

    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdata:::NMdataDecideOption("as.fun",as.fun)
    
    ## list.doses <- list(TIME=TIME, EVID=EVID, CMT=CMT, AMT=AMT, RATE=RATE, SS=SS, II=addl$II,ADDL=addl$ADDL)
    ## list.doses <- list(TIME=TIME, EVID=EVID, CMT=CMT, AMT=AMT, RATE=RATE, SS=SS, ADDL=addl)
    list.doses <- list(TIME=TIME, EVID=EVID, CMT=CMT, AMT=AMT, RATE=RATE, SS=SS 
                       )
    if(!is.null(addl)){
        addl <- as.data.table(addl)
        list.doses$ADDL <- addl[,setdiff(colnames(addl),"II"),with=FALSE]
        list.doses$II <- addl[,setdiff(colnames(addl),"ADDL"),with=FALSE]
    }
    
    ## disregard the ones that were not supplied
    list.doses <- list.doses[!sapply(list.doses,is.null)]

    ## convert to dt's
    
    names.doses <- names(list.doses)
    
    list.doses <- lapply(names.doses,function(x){
        dt <- list.doses[[x]]
        if(is.data.frame(dt)){
            if(!is.data.table(dt)){
                dt <- as.data.table(dt)
            }
            return(dt)
        } else {
            DT <- data.table(x1=dt)
            setnames(DT,"x1",x)
            return(DT)
        }
    })

#### check that TIME is long enough
    dt.lengths <- data.table(name=names.doses,
                             length=sapply(list.doses,nrow))
    ## if(dt.lengths[name=="TIME",length]!=dt.lengths[,max(length)]){
    ##     stop("Column(s) has/have been specified beyond TIME. This is not allowed - TIME has to be at least as long as other arguments.")
    ## }

    ## list.doses[[6]][,TIME:=12]
    ## list.doses[[5]][,TIME:=12]
    
### make use of merge.data.frame to get outer merges where if no
### common columns found.
    df.doses <- lapply(list.doses,as.data.frame)
    res <- Reduce(merge,df.doses)
    dt.doses1 <- as.data.table(res)
    
    names(list.doses) <- names.doses
    
    ## stack all dt's, fill=T
    ##  dt.doses1 <- rbindlist(list.doses,fill=T)

    ## identify covs
    ##     covs <- setdiff(colnames(dt.doses1),c(names.doses,"II","ADDL"))
    covs <- setdiff(colnames(dt.doses1),c(names.doses))
    if("ID" %in% covs) stop("ID is currently not allowed as a covariate. Please use a different name and adjust the result accordingly.")
    combs <- unique(dt.doses1[,covs,with=F])

    ## get rid of all combs that contain NA
    
    col.row <- tmpcol(combs)
    combs[,(col.row):=.I]
    combs[,Nna:=sum(is.na(.SD)),by=col.row]
    
### This used to drop rows. That seems too risky. Give warning if there are any.
    nrows.na <- combs[Nna>0][,.N]
    if(nrows.na){
        warning("NA values among covariates. This may give unintended results.")
    }
    ## combs <- combs[Nna==0]
    ## combs <- combs[Nna<length(covs)]

    combs[,(col.row):=NULL]
    combs[,Nna:=NULL]
    ## expand all to include all combs
### the name (EVID, AMT, etc) col must be renamed to value. For dcast.
    list.doses.exp <- lapply(names.doses,function(name){
###### egdt with the unique combs of those we dont have already.
        ## name <- names.doses[1]
        elem <- list.doses[[name]]
        ## egdt(transform(elem,elem=name),combs[,setdiff(names(combs),names(elem)),with=FALSE] ,quiet=T)
        if("variable"%in%colnames(elem)) stop("a column called variable is not allowed in provided data.")
        if(!name%in%colnames(elem)) stop("If data is given as a data.table, the argument name has to be a column in data too.")
        egdt(
            melt(elem,measure.vars=name)
           ,
            combs[,setdiff(names(combs),names(elem)),with=FALSE]
           ,quiet=T)
    })
    ## dt.doses1 <- unique(rbindlist(list.doses.exp,fill=T))
    dt.doses1 <- rbindlist(list.doses.exp,fill=T)
    ## assign ID counter
    col.id <- "ID"
    if(!col.id%in%covs){
        dt.doses1[,ID:=.GRP,by=covs]
    }
    ## calc max length within ID
    dt.doses1[,max.length:=.N,by=.(ID,variable)]
    dt.doses1[,max.length:=max(max.length),by=.(ID)]
    ## expand to max length for each elem within each comb of covs
    dt.doses2 <-
        rbindlist(lapply(split(dt.doses1,by=c("variable","ID")),function(sub) {
            if(nrow(sub)==0) return(NULL)
            sub[c(1:.N,rep(.N,unique(sub$max.length)-.N))]}))

    ## assign row counter within comb
    dt.doses2[,ROW:=1:.N,by=c("ID","variable")]
    
    ## dcast
    res <- dcast(dt.doses2,as.formula(paste(paste0(c("ID","ROW",covs),collapse="+"),"~variable")),value.var="value")
    res[,ROW:=NULL]
    res[,MDV:=1]

    ## because covariates can be a mix of types/classes, at least the
    ## value column may be a list at this point. Recoding that.
    res <- lapply(res,unlist)
    res <- as.data.table(res)
    ## order rows and columns. 
    
    setorderv(res,c("ID","TIME","CMT"))
    res <- NMorderColumns(res)
    
    ## done
    as.fun(res)
}



