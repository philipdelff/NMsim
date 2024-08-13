##' Parameter data from csv
##' 
##' Reads output table from simpar and returns a long format
##' data.table. This is the same format as returned by NMreadExt()
##' which can be used by NMsim. 
##'
##' @param data A data.frame or a path to a delimited file to be read
##'     using `data.table::fread`.
##' @param col.model Name of the model counter, default is "model". If
##'     the provided name is not found in data, it will be created as
##'     a row counter. Why needed? Each row in data represents a set
##'     of parameters, i.e. a model. In the long format result, each
##'     model will have multiple rows. Hence, a model identifier is
##'     needed to distinguish between models in results.
##' @param strings.par.type Defines how column names get associated
##'     with THETA, OMEGA, and SIGMA. Default is to look for "T", "O",
##'     or "S" as starting letter. If customizing, make sure each no
##'     column name will be matched by more than one criterion.
##' @param as.fun The default is to return data as a data.frame. Pass
##'     a function (say \code{tibble::as_tibble}) in as.fun to convert to
##'     something else. If data.tables are wanted, use
##'     as.fun="data.table". The default can be configured using
##'     NMdataConf.
##' @return a long-format data.frame of model parameters
##' @details
##'     The wide data format read by `readParsWide` is not a Nonmem
##'     format. It is used to bridge output from other tools such as
##'     simpar, and potentially PSN. 
##'
##' This function reads a data that is "wide" in parameters - it has a
##'     column for each parameter, and one row per parameter set or
##'     "model". It returns a data set that is "long" in model and
##'     parameters. The long format contains
##' \itemize{
##' \item id.model.par The unique model-parameter identifier. The row-identifier.
##' \item model Model identifier. 
##' \item par.type ("THETA", "OMEGA", "SIGMA")
##' \item i and j indexes for the parameters (j is NA for par.type=="THETA").
##' \item value The parameter value 
##' \item parameter Nonmem-style parameter names. THETA1, OMEGA(1,1) etc. Notice the inconsistent naming of THETA vs others.
##' \item name.wide The column name in the wide data where this value was taken
##' }
##' The columns or "measure variables" from which to read values  are
##'     specified as three regular expressions, called THETA, OMEGA, and SIGMA. The default three regular expressions will associate a column name starting with "T" with THETAs, while "O" or "S" followed by anything means "OMEGA" or "SIGMA".
##'
##' readParsWide extracts i and j indexes from sequences of digits in the column names. TH.1 would be TETA1, SG1.1 is SIGMA(1,1).
##'
##' @import data.table
##' @import NMdata
##' @examples
##' \dontrun{
##' tab.ext <- readParsCsv("simpartab.csv")
##' ## or
##' tab.simpar <- fread("simpartab.csv")
##' tab.ext <- readParsCsv(tab.simpar)
##' NMsim(...,method.sim=NMsim_VarCov,tab.ext=tab.ext)
##' }
##' @export

readParsWide <- function(data,col.model=NULL,strings.par.type=c(THETA="^T.*",OMEGA="^O.*",SIGMA="^S."),as.fun){


#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    id.model.par <- NULL
    name.wide <- NULL
    value <- NULL
    . <- NULL
    par.type <- NULL
    parameter <- NULL
    model <- NULL
    i <- NULL
    j <- NULL


###  Section end: Dummy variables, only not to get NOTE's in pacakge checks

    
    if(!is.data.frame(data)){
        data <- fread(data)
    }

    ## if col.model is null, create one
    if(is.null(col.model)){
        col.model <- "model"
    }
    if(!col.model%in%colnames(data)){
        data[,(col.model):=.I]
    }
    ## as.fun
    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdata:::NMdataDecideOption("as.fun",as.fun)

    
### convert to long format    
    pars.l <- melt(data,id.vars=col.model,variable.name="name.wide",value.name="value")
    
    ## assign par.type
### id.model.par is unique across models x parameter
    pars.l[,id.model.par:=.I]
    ## dt.match is w
    dt.match <- pars.l[,lapply(strings.par.type,grepl,x=name.wide),by=c(col.model,"id.model.par")]
    dt.match.l <- melt(dt.match,id.vars=c(col.model,"id.model.par"),variable.name="par.type")[value==TRUE]

    ## TODO check that only one was matched
    
    ## assign parameter
    pars.l <- mergeCheck(pars.l,dt.match.l[,.(id.model.par,par.type)],by="id.model.par",quiet=TRUE)


    
    ##  extract the index numbers i,j from digits in the string
    deriveCols <- function(x,n){
        found <- regmatches(x,gregexpr("[1-9][0-9]*",x))
        
        found <- as.numeric(found[[1]])
        as.list(c(found,rep(NA,n-length(found))))
    }
    pars.l[,(c("i","j")):=deriveCols(name.wide,n=2),by=.(id.model.par)]

    
    ## Assign Nonmem style name THETA1, OMEGA(1,1) etc. Notice the
    ## inconsistency in naming.
    NMparIdxNames <- function(dt){

        j <- NULL
        par.type <- NULL
        parameter <- NULL
        i <- NULL
        j <- NULL

        
        dt <- copy(dt)
        dt[,par.type:=toupper(par.type)]

        dt[par.type=="THETA",parameter:=paste0(par.type,i)]
        dt[par.type!="THETA",parameter:=sprintf("%s(%d,%d)",par.type,i,j)]

        dt[,parameter]
    }
    pars.l[,parameter:=NMparIdxNames(.SD)]

    setcolorder(pars.l,cc(id.model.par,model,par.type,i,j,value,parameter,name.wide))

    as.fun(pars.l)

}
