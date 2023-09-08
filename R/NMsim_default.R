##' Transform an estimated Nonmem model into a simulation control
##' stream
##'
##' The default behaviour of \code{NMsim}. Replaces any $ESTIMATION
##' and $COVARIANCE sections by a $SIMULATION section.
##' 
##' @param file.sim See \code{?NMsim}.
##' @param file.mod See \code{?NMsim}.
##' @param data.sim See \code{?NMsim}.
##' @param nsims Number of replications wanted. The default is 1. If
##'     greater, multiple control streams will be generated.
##' @param replace.sim If there is a $SIMULATION section in the
##'     contents of file.sim, should it be replaced? Default is
##'     yes. See the \code{list.section} argument to \code{NMsim} for
##'     how to provide custom contents to sections with \code{NMsim}
##'     instead of editing the control streams beforehand.
##' @param return.text If TRUE, just the text will be returned, and
##'     resulting control stream is not written to file.
##' @import NMdata
##' @import data.table
##' @return Character vector of simulation control stream paths
##' @keywords internal

NMsim_default <- function(file.sim,file.mod,data.sim,nsims=1,replace.sim=TRUE,return.text=FALSE){

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    . <- NULL
    submodel <- NULL
    SUBMODEL <- NULL
    fn.sim <- NULL
    path.sim <- NULL
    run.sim <- NULL

###  Section end: Dummy variables, only not to get NOTE's in pacakge checks
    
    lines.sim <- readLines(file.sim)

    sections.sim <- NMreadSection(lines=lines.sim)
    names.sections <- names(sections.sim)
    
    ## Remove any $EST, $COV.

    ## remove EST
    lines.sim <- NMdata:::NMwriteSectionOne(lines=lines.sim,section="$ESTIMATION",newlines="",backup=FALSE,quiet=TRUE)
    
    ## remove COV
    lines.sim <- NMdata:::NMwriteSectionOne(lines=lines.sim,section="$COVARIANCE",newlines="",backup=FALSE,quiet=TRUE)
    
    ## replace SIM - the user may not want this
    if(replace.sim){
        ##Insert $SIM before $TABLE. If not $TABLE, insert $SIM in the bottom.

        ## define the $SIM section to insert
        ## section.sim <- sprintf("$SIMULATION ONLYSIM (%s)",seed)
        section.sim <- "$SIMULATION ONLYSIM"
        ## section.sim <- "$SIMULATION"

        ## remove any existing $SIM
        lines.sim <- NMdata:::NMwriteSectionOne(lines=lines.sim,section="$SIMULATION",
                                                newlines="",backup=FALSE,quiet=TRUE)
        
        ## if there is $TABLE, insert before those
        tab.section.exists <- any(grepl("^(TABLE)$",names.sections))
        if(tab.section.exists){
            lines.sim <- NMdata:::NMwriteSectionOne(lines=lines.sim,
                                                    section="TABLE",
                                                    newlines=section.sim,
                                                    location="before",backup=FALSE,quiet=TRUE)
        } else {
            ## else in the bottom
            lines.sim <- NMdata:::NMwriteSectionOne(lines=lines.sim,section="SIMULATION",
                                                    newlines=section.sim,location="last",
                                                    backup=FALSE,quiet=TRUE)
        }

    }

    if(return.text){
        if(nsims!=1){stop("nsims must be 1 for return.text=TRUE")}
        return(lines.sim)            
    }
    
    writeTextFile(lines=lines.sim,file=file.sim)
    
    if(nsims==1){
        return(file.sim)
    }
    
### if nsims>1
    ## define new files

    path.sim.0 <- file.sim
    run.sim.0 <- fnExtension(basename(path.sim.0),"")
    rm(path.sim)
    dt.sims <- data.table(SUBMODEL=1:nsims)
    length.num.char <- ceiling(log10(nsims+1))
    dt.sims[,submodel:=sprintf(fmt=paste0("%0",length.num.char,"d"),SUBMODEL)]
    dt.sims[,path.sim:=fnAppend(path.sim.0,submodel),by=.(SUBMODEL)]
    dt.sims[,fn.sim:=basename(path.sim),by=.(SUBMODEL)]
    dt.sims[,run.sim:=fnExtension(fn.sim,""),by=.(SUBMODEL)]

    ## copy model control stream to submodel control streams
    
    res <- dt.sims[,
                   
                   file.copy(from=unique(path.sim.0)
                            ,to=unique(path.sim)
                            ,overwrite=TRUE)
                  ,by="SUBMODEL"]
    
### output tables.
    ## gsub the sim name string with a new subsetted simname string.
    
    sec.0 <- NMreadSection(file=path.sim.0,section="TABLE")
    dt.sims[,{
        sec.new <- gsub(run.sim.0,run.sim,x=sec.0)
        NMwriteSection(files=path.sim,section="TABLE",newlines=sec.new)
    },by=.(SUBMODEL)]

    
    invisible(dt.sims[,unique(path.sim)])
    
}
