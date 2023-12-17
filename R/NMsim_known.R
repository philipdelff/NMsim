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
##' @param file.phi A phi file to take the known subjects from. The
##'     default is to replace the filename extension on file.mod with
##'     .phi. A different .phi file would be used if you want to reuse
##'     subjects simulated in a previous simulation.
##' @param return.text If TRUE, just the text will be returned, and
##'     resulting control stream is not written to file.
##' @import NMdata
##' @import data.table
##' @return Path to simulation control stream
##' @export


NMsim_known <- function(file.sim,file.mod,data.sim,file.phi,return.text=FALSE){
    
#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    . <- NULL
    rowtmp <- NULL
    ID <- NULL
    n <- NULL
    is.data <- NULL
    TABLE.NO <- NULL
    tableStart <- NULL
    text <- NULL
    textmod <- NULL
    par.type <- NULL

### Section end: Dummy variables, only not to get NOTE's in pacakge checks
    
    if(missing(file.phi)||is.null(file.phi)){
        file.phi <- fnExtension(file.mod,".phi")
    }

    path.phi.sim <- fnAppend(fnExtension(file.sim,".phi"),"input")
    files.needed.def <- NMsim_default(file.sim=file.sim,file.mod=file.mod,data.sim=data.sim)

    lines.sim <- readLines(file.sim)
    
### prepare simulation control stream
    ## get rid of any $ETAS sections
    lines.sim <- NMdata:::NMwriteSectionOne(lines=lines.sim,section="$ETAS",newlines="",backup=FALSE,quiet=TRUE)
    
    
### $SIM ONLYSIM does not work in combination with $ESTIM, so we have to drop ONLYSIM
    lines.section.sim <- NMreadSection(lines=lines.sim,section="SIM")
    lines.section.sim <- sub("ONLYSIM(ULATION)*","",lines.section.sim)
    lines.sim <- NMdata:::NMwriteSectionOne(lines=lines.sim,section="SIM",newlines=lines.section.sim,backup=FALSE,quiet=FALSE)

    
    
###### todo
    ## cant allow disjoint id's in data
    ## fail if ID's are non-unique in phi
    ## only one table in orig phi supported
    ## only type.mod="est" allowed (phi is used)

    ## phi file required
### read estimation phi file and select subjects to be simulated

### generate new phi file
    data.sim[,rowtmp:=.I]
    dt.id.order <- data.sim[,.SD[1],by=.(ID=as.character(ID)),.SDcols=cc(rowtmp)]

#### try to read phi file to see if it reads and has ETAs
    etasFromTabs <- FALSE
    ## res.phi <- try(NMreadPhi(file.phi,as.fun="data.table"))
    ## res.phi <- try(NMreadPhi(file.phi))
    res.phi <- try(NMdata::NMreadPhi(file.phi,as.fun="data.table"))
    
    if(inherits(res.phi,"try-error")) {
        etasFromTabs <- TRUE
    } else {
        if(res.phi[par.type=="ETA",.N]==0){
            etasFromTabs <- TRUE
        }
    }
    if(etasFromTabs){
        ## Could we just read tables and assume we can find an ID? Or how can NMscanData be informed with col.row etc?
        dt.res <- NMscanData(file.mod)
        file.phi <- tempfile()
        genPhiFile(data=dt.res,file=file.phi)
    }


###### using the last table found in .phi to generate lines for a new .phi file. 
    phi.lines <- data.table(text=readLines(file.phi))
    phi.lines[,n:=.I]
    ## Accepting E and R which can be in numbers (R?)
    phi.lines[,is.data:=!grepl("[abcdfghijklmnopqrstuvwxyzABCDFGHIJKLMNOPQSTUVWXYZ]",x=text)]
    phi.lines[is.data==TRUE,textmod:=gsub(" +"," ",text)]
    phi.lines[is.data==TRUE,textmod:=gsub("^ +","",textmod)]
    phi.lines[is.data==TRUE,ID:=strsplit(textmod,split=" ")[[1]][2],by=.(n)]
    ## phi.lines
    ## picking last table - for SAEM+IMP that means we use IMP phi's
    phi.lines[,tableStart:=FALSE]
    phi.lines[grepl("^ *TABLE NO\\..*",text),tableStart:=TRUE]
    phi.lines[,TABLE.NO:=cumsum(tableStart)]
    phi.lines <- phi.lines[TABLE.NO==max(TABLE.NO)]
    
    phi.use <- mergeCheck(dt.id.order[,.(ID)],phi.lines[,.(ID,text)],by=cc(ID),all.x=TRUE,quiet=TRUE)
    

### Error if subjects in data are not found in phi
    if(phi.use[,any(is.na(text))]){
        message("IDs not found in nonmem results (phi file): ", paste(phi.use[is.na(text),ID],collapse=", "))
        phi.use <- phi.use[!is.na(text)]
    }
    phi.use <- rbind(phi.lines[is.data==FALSE,.(text)],phi.use,fill=TRUE)

    lines.phi <- phi.use[,text]
    path.phi.sim <- fnAppend(fnExtension(file.sim,".phi"),"input")


### udpate simulation control stream
    lines.new <- sprintf("$ETAS FILE=%s FORMAT=s1pE15.8 TBLN=1
$ESTIMATION  MAXEVAL=0 NOABORT METHOD=1 INTERACTION FNLETA=2",basename(path.phi.sim))
    
    lines.sim <- NMdata:::NMwriteSectionOne(lines=lines.sim,section="TABLE",location="before",
                                            newlines=lines.new,backup=FALSE,quiet=TRUE)

#### .mod done

    
    if(return.text){
        return(list(mod=lines.sim,
                    phi=lines.phi))
    }
    
    writeTextFile(lines=lines.sim,file=file.sim)
    
    writeTextFile(lines.phi,path.phi.sim)

    
    
    files.needed <- data.table(path.sim=file.sim,files.needed=path.phi.sim)
    files.needed
}

