##' Add simulation records to dosing records
##'
##' Performs the simple job of adding simulation events to all
##' subjects in a data set. Copies over columns that are not varying
##' at subject level (i.e. non-variying covariates).
##' 
##' @param doses dosing records Nonmem style (EVID==1 records from a
##'     data set)
##' @param time.sim A numerical vector with simulation times. Can also
##'     be a data.frame in which case it must contain a `TIME` column
##'     and is merged with subjects found in `doses`. The latter
##'     feature is experimental.
##' @param CMT The compartment in which to insert the EVID=2
##'     records. If longer than one, the records will be repeated in
##'     all the specified compartments. If a data.frame, covariates
##'     can be specified.
##' @param EVID The value to put in the EVID column for the created
##'     rows. Default is 2 but 0 may be prefered even for simulation.
##' @param as.fun The default is to return data as a data.frame. Pass
##'     a function (say `tibble::as_tibble`) in as.fun to convert to
##'     something else. If data.tables are wanted, use
##'     as.fun="data.table". The default can be configured using
##'     NMdataConf.
##' @details The resulting data set is ordered by ID, TIME, and
##'     EVID. You may have to reorder for your specific needs.
##' @examples
##' library(data.table)
##' ## Users should not use setDTthreads. This is for CRAN to only use 1 core.
##' data.table::setDTthreads(1) 
##' (doses1 <- NMcreateDoses(TIME=c(0,12,24,36),AMT=c(2,1)))
##' addEVID2(doses1,time.sim=seq(0,28,by=4),CMT=2)
##'
##' ## two named compartments
##' dt.doses <- NMcreateDoses(TIME=c(0,12),AMT=10,CMT=1)
##' seq.time <- c(0,4,12,24)
##' dt.cmt <- data.table(CMT=c(2,3),analyte=c("parent","metabolite"))
##' res <- addEVID2(dt.doses,time.sim=seq.time,CMT=dt.cmt)
##' 
##' ## Separate sampling schemes depending on covariate values
##' dt.doses <- NMcreateDoses(TIME=data.table(regimen=c("SD","MD","MD"),TIME=c(0,0,12)),AMT=10,CMT=1)
##'
##' seq.time.sd <- data.table(regimen="SD",TIME=seq(0,6))
##' seq.time.md <- data.table(regimen="MD",TIME=c(0,4,12,24))
##' seq.time <- rbind(seq.time.sd,seq.time.md)
##' 
##' addEVID2(dt.doses,time.sim=seq.time,CMT=2)
##' @import data.table
##' @import NMdata
##' @return A data.frame with dosing records
##' @export 


addEVID2 <- function(doses,time.sim,CMT,EVID=2,as.fun){
    
#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    DV <- NULL
    MDV <- NULL
    ID <- NULL
    TIME <- NULL
    ..EVID <- NULL

### Section end: Dummy variables, only not to get NOTE's in pacakge checks
    
    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdata:::NMdataDecideOption("as.fun",as.fun)


    if(is.data.table(doses)) {
        doses <- copy(doses)
    } else {
        doses <- as.data.table(doses)
    }
    
    
    to.use <- setdiff(colnames(doses),c("TIME","EVID","CMT","AMT","RATE","MDV","SS","II","ADDL","DV"))
    covs.doses <- findCovs(doses[,to.use,with=FALSE],by="ID",as.fun="data.table")

    if(!is.data.frame(time.sim)){
        dt.obs <- data.table(TIME=time.sim)
        dt.obs <- egdt(dt.obs,covs.doses,quiet=TRUE)
    } else {
        if(!"TIME"%in%colnames(time.sim)) stop("When time.sim is a data.frame, it must contain a column called TIME.")
        time.sim <- as.data.table(time.sim)
        cols.by <- intersect(colnames(time.sim),colnames(covs.doses))
        if(length(cols.by) == 0){
            ## message("time.sim is a data,frame but no column names overlap with column names in doses. Only the TIME column from time.sim used.")
            ## return(addEVID2(doses,time.sim$TIME,CMT,as.fun))
            dt.obs <- egdt(time.sim,covs.doses,quiet=TRUE)
        } else {
            dt.obs <- merge(time.sim,covs.doses,all.x=TRUE,allow.cartesian = TRUE)
        }
    }
    dt.obs[
       ,EVID:=..EVID][
       ,DV:=NA_real_][
       ,MDV:=1]
    

### add CMT
    if (!is.data.frame(CMT)){
        CMT <- data.table(CMT=CMT)
        
    } else if (!is.data.table(CMT)){
        CMT <- as.data.table(CMT)
    }
    
    dt.obs <- dt.obs[,setdiff(colnames(dt.obs),colnames(CMT)),with=FALSE]
    dt.obs <- egdt(dt.obs,CMT,quiet=TRUE)
    
    
    ## dat.sim <- egdt(typsubj[,!(c("ID"))],doses.ref)
    dat.sim <- rbind(doses,dt.obs,fill=T)

#### not sure how to allow flexible sorting. For now, NB order is naive.
    setorder(dat.sim,ID,TIME,EVID)
    ## dat.sim[,REC:=.I]
    as.fun(dat.sim)
    
}

