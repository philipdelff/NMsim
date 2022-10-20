##' Add simulation records to dosing records
##'
##' @param doses dosing records Nonmem style
##' @param time.sim A numerical vector with simulation times
##' @param CMT The compartment in which to insert the EVID=2 records
##' @examples
##' (doses1 <- NMcreateDoses(TIME=c(0,12,24,36),AMT=c(2,1)))
##' addEVID2(doses1,time.sim=seq(0,48,by=1),CMT=2)
##' @import data.table
##' @import NMdata
##' @export 


addEVID2 <- function(doses,time.sim,CMT){
    if(is.data.table(doses)) {
        doses <- copy(doses)
    } else {
        doses <- as.data.table(doses)
    }
    

    to.drop <- intersect(c("TIME","EVID","CMT","AMT","RATE","MDV","SS","II","ADDL"),colnames(doses))
    covs.doses <- findCovs(doses[,setdiff(colnames(doses),to.drop),with=FALSE],by="ID")

    dt.obs <- data.table(TIME=time.sim)
    dt.obs <- egdt(dt.obs,covs.doses,quiet=TRUE)
    dt.obs[
       ,EVID:=2][
       ,DV:=NA_real_][
       ,MDV:=1]
                     

### add CMT
    if(!is.list(CMT) && length(CMT)== 1){
        dt.obs[,CMT:=CMT]
    } else if (is.data.frame(CMT)){
        CMT <- as.data.table(CMT)
        dt.obs <- egdt(dt.obs,CMT)
    }
    
    ## dat.sim <- egdt(typsubj[,!(c("ID"))],doses.ref)
    dat.sim <- rbind(doses,dt.obs,fill=T)

#### not sure how to allow flexible sorting. For now, NB order is naive.
    setorder(dat.sim,ID,TIME,EVID)
    ## dat.sim[,REC:=.I]
    dat.sim[]
}

