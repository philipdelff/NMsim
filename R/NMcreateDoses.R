## AMT, RATE, SS, II, CMT are vectors of length 1 or longer. Those not of max
## length 1 are repeated.  If TIME is longer than those, they are
## extended to match length of TIME.

##' @param TIME
##' @param AMT
##' @param addl is a list of ADDL and II that will be applied to last dose
##' @param ID ID's to replicate for. Default is 1. Use NULL to omit.
##' @import data.table
##' @examples
##' NMcreateDoses(TIME=c(0,1,4),AMT=c(2,1))
##' ## Make Nonmem repeat the last dose. This is a total of 20 dosing events.
##' NMcreateDoses(TIME=c(0,12),AMT=c(2,1),addl=list(ADDL=9*2,II=12))
##' ## using cyclicDose to create a complex dosing regimen with rest days and rest weeks
##' dos1=cyclicDoses(c(0,12),cycles.days=list(c(4,3),c(21,7)),days.total=56,dose=400)
##' NMcreateDoses(TIME=dos1$TIME,AMT=dos1$AMT,CMT=2)
##'
##' @export



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




## TODO: TIME must be full length

## No NA's anywhere

## MDV

## addl

## if a cov is found in multiple arguments, it must span same values

## Should CMT have a default? Or be required?

## avoid hard coding variable names

## N is another arg. If ID not in covs, everything is replicated. It
## can be a data.table too, meaning that we replicate within
## covariates. Maybe we have to be able to use a known set of ID's and covs?



NMcreateDoses <- function(TIME, AMT=NULL, RATE=NULL, SS=NULL, II=NULL, CMT=1, EVID=1, addl=NULL,debug=FALSE){
    
    if(debug) browser()
    
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



##### based on merge.data.frame. Much simpler.
NMcreateDoses.df <- function(TIME, AMT=NULL, RATE=NULL, SS=NULL, II=NULL, CMT=1, EVID=1, addl,debug=FALSE){
    
    if(debug) browser()
    
    list.doses <- list(TIME=TIME, EVID=EVID, CMT=CMT, AMT=AMT, RATE=RATE, SS=SS, II=II)
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

    
    if(F){
        Reduce(function(x,y){
            bys <- intersect(colnames(x),colnames(y))
            if(length(bys)==0) return(egdt(x,y))
            merge(x,y,by=bys)
        },list.doses)
    }
    
### is it this simple?
    df.doses <- lapply(list.doses,as.data.frame)
    res <- Reduce(merge,df.doses)
    as.data.table(res)

}
