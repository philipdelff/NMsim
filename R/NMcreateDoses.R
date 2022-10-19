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
##' @import data.table
##' @examples
##' NMcreateDoses(TIME=c(0,1,4),AMT=c(2,1))
##' ## Make Nonmem repeat the last dose. This is a total of 20 dosing events.
##' NMcreateDoses(TIME=c(0,12),AMT=c(2,1),addl=list(ADDL=9*2,II=12))
##' dt.amt <- data.table(DOSE=c(100,400))
##' dt.amt[,AMT:=DOSE*1000]
##' dt.amt
##' doses.sd <- NMcreateDoses(TIME=0,AMT=dt.amt)
##' doses.sd[,dose:=paste(DOSE,"mg")]
##' doses.sd[,regimen:="SD"]
##' doses.sd
##' 
##' ### multiple dose regimens with loading are easily created with NMcreateDoses too
##' ## Specifying the time points explicitly
##' dt.amt <- data.table(AMT=c(200,100,800,400)*1000,DOSE=c(100,100,400,400))
##' doses.md.1 <- NMcreateDoses(TIME=seq(0,by=24,length.out=7),AMT=dt.amt)
##' doses.md.1[,dose:=paste(DOSE,"mg")]
##' doses.md.1[,regimen:="QD"]
##' doses.md.1
##' ## or using ADDL+II
##' dt.amt <- data.table(AMT=c(200,100,800,400)*1000,DOSE=c(100,100,400,400))
##' doses.md.2 <- NMcreateDoses(TIME=c(0,24),AMT=dt.amt,addl=data.table(ADDL=c(0,5),II=c(0,24)))
##' doses.md.2[,dose:=paste(DOSE,"mg")]
##' doses.md.2[,regimen:="QD"]
##' doses.md.2
##'
##' @export

## AMT, RATE, SS, II, CMT are vectors of length 1 or longer. Those not of max
## length 1 are repeated.  If TIME is longer than those, they are
## extended to match length of TIME.

### allowed combinations of AMT, RATE, SS, II here:
## https://ascpt.onlinelibrary.wiley.com/doi/10.1002/psp4.12404

##### examples
## library(data.table)
## library(NMdata)

## NMcreateDoses(TIME=data.table(ID=c(1,1,2,2,2),TIME=c(0,1,0,1,4)),AMT=data.table(ID=c(1,1,2,2),AMT=c(2,1,4,2)))

## NMcreateDoses(TIME=c(0,1,4),AMT=data.table(ID=c(1,1,2,2),AMT=c(2,1,4,2)))
## NMcreateDoses(TIME=c(0,1,4),AMT=data.table(DOSELEVEL=c(1,1,2,2),AMT=c(2,1,4,2)))

## NMcreateDoses(TIME=c(0,1,4),AMT=c(2,1,4,2))
## NMcreateDoses(TIME=c(0,1,4),AMT=c(2,1,4,2),CMT=1)

NMcreateDoses <- function(TIME, AMT=NULL, RATE=NULL, SS=NULL, CMT=1, EVID=1, addl=NULL){
    
    ## if(debug) browser()
    
    list.doses <- list(TIME=TIME, EVID=EVID, CMT=CMT, AMT=AMT, RATE=RATE, SS=SS, II=addl$II,ADDL=addl$ADDL)
    ## disregard the ones that were not supplied
    list.doses <- list.doses[!sapply(list.doses,is.null)]

    ## convert to dt's
    
    names.doses <- names(list.doses)
    list.doses <- lapply(names.doses,function(x){
        if(is.data.table(list.doses[[x]])){
            return(list.doses[[x]])
        } else {
            DT <- data.table(x1=list.doses[[x]])
            setnames(DT,"x1",x)
            return(DT)
        }
    })

### is it this simple?
    df.doses <- lapply(list.doses,as.data.frame)
    res <- Reduce(merge,df.doses)
    dt.doses1 <- as.data.table(res)
    
    names(list.doses) <- names.doses
    
    ## stack all dt's, fill=T
    ##  dt.doses1 <- rbindlist(list.doses,fill=T)

    ## identify covs
    covs <- setdiff(colnames(dt.doses1),names.doses)
    combs <- unique(dt.doses1[,covs,with=F])
    ## get rid of all combs that contain NA
    combs[,ID:=.I]
    combs[,Nna:=sum(is.na(.SD)),by=ID]

### trying to fix. Is this a bug?
    combs <- combs[Nna==0]
    ## combs <- combs[Nna<length(covs)]

    combs[,ID:=NULL]
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
    dt.doses1 <- unique(rbindlist(list.doses.exp,fill=T))
    ## assign ID counter
    dt.doses1[,ID:=.GRP,by=covs]
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
    ## order rows and columns. We should order rows by CMT too.
    setorderv(res,c("ID","TIME"))
    res <- NMorderColumns(res)
    
    ## done
    res
}



