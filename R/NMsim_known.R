##' Known subject simulation method
##' 
##' Simulates _known_ subjects, meaning that it reuses ETA values from
##' estimation run. This is what is refered to as emperical Bayes
##' estimates. The .phi file from the estimation run must be found
##' next to the .lst file from the estimation.This means that ID
##' values in the (simulation) input data must be ID values that were
##' used in the estimation too. Runs an \code{$ESTIMATION MAXEVAL=0}
##' but pulls in ETAs for the ID's found in data. No
##' \code{$SIMULATION} step is run which may affect how for instance
##' residual variability is simulated, if at all.
##' 
##' @param file.sim See \code{?NMsim}.
##' @param file.mod See \code{?NMsim}.
##' @param data.sim See \code{?NMsim}.
##' @param return.text If TRUE, just the text will be returned, and
##'     resulting control stream is not written to file.
##' @import NMdata
##' @import data.table
##' @return Path to simulation control stream
##' @export


NMsim_known <- function(file.sim,file.mod,data.sim,return.text=FALSE){

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    . <- NULL
    rowtmp <- NULL
    ID <- NULL
    n <- NULL
    is.data <- NULL
    text <- NULL
    textmod <- NULL    

### Section end: Dummy variables, only not to get NOTE's in pacakge checks
    
    path.phi.sim <- fnAppend(fnExtension(file.sim,".phi"),"input")
    files.needed.def <- NMsim_default(file.sim=file.sim,file.mod=file.mod,data.sim=data.sim)

    lines.sim <- readLines(file.sim)
    
### prepare simulation control stream
    ## get rid of any $ETAS sections
    lines.sim <- NMdata:::NMwriteSectionOne(lines=lines.sim,section="$ETAS",newlines="",backup=FALSE,quiet=TRUE)
    
    lines.new <- sprintf("$ETAS FILE=%s  FORMAT=s1pE15.8 TBLN=1
$ESTIMATION  MAXEVAL=0 NOABORT METHOD=1 INTERACTION FNLETA=2",basename(path.phi.sim))
    
    lines.sim <- NMdata:::NMwriteSectionOne(lines=lines.sim,section="TABLE",location="before",
                                   newlines=lines.new,backup=FALSE,quiet=TRUE)

### $SIM ONLYSIM does not work in combination with $ESTIM, so we have to drop ONLYSIM
    lines.section.sim <- NMreadSection(lines=lines.sim,section="SIM")
    lines.section.sim <- sub("ONLYSIM(ULATION)*","",lines.section.sim)
    lines.sim <- NMdata:::NMwriteSectionOne(lines=lines.sim,section="SIM",newlines=lines.section.sim,backup=FALSE,quiet=FALSE)
    
#### .mod done
 
    
###### todo
    ## cant allow disjoint id's in data
    ## fail if ID's are non-unique in phi
    ## only one table in orig phi supported
    ## only type.mod="est" allowed (phi is used)

    ## phi file required
### read estimation phi file and select subjects to be simulated
    path.phi <- fnExtension(file.mod,".phi")
    phi <- NMreadTab(path.phi)

    data.sim[,rowtmp:=.I]
    dt.id.order <- data.sim[,.SD[1],by=.(ID=as.character(ID)),.SDcols=cc(rowtmp)]

    phi.lines <- data.table(text=readLines(path.phi))
    phi.lines[,n:=.I]        
    phi.lines[,is.data:=!grepl("[a-zABCDFGHIJKLMNOPQSTUVWXYZ]",x=text)]
    phi.lines[is.data==TRUE,textmod:=gsub(" +"," ",text)]
    phi.lines[is.data==TRUE,textmod:=gsub("^ +","",textmod)]
    phi.lines[is.data==TRUE,ID:=strsplit(textmod,split=" ")[[1]][2],by=.(n)]
    ## phi.lines

    phi.use <- mergeCheck(dt.id.order[,.(ID)],phi.lines[,.(ID,text)],by=cc(ID),all.x=TRUE)
### Error if subjects in data are not found in phi
    if(phi.use[,any(is.na(text))]){
        message("IDs not found in nonmem results (phi file):", paste(phi.use[is.na(text),ID],collapse=", "))
        phi.use <- phi.use[!is.na(text)]
    }
    phi.use <- rbind(phi.lines[is.data==FALSE,.(text)],phi.use,fill=TRUE)

    lines.phi <- phi.use[,text]
    path.phi.sim <- fnAppend(fnExtension(file.sim,".phi"),"input")

    if(return.text){
        return(list(mod=lines.sim,
                    phi=lines.phi))
    }
    
    writeTextFile(lines=lines.sim,file=file.sim)
    
    writeTextFile(lines.phi,path.phi.sim)

    files.needed <- data.table(path.sim=file.sim,files.needed=path.phi.sim)
    files.needed
}

