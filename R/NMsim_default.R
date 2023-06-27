NMsim_default <- list(

    fun.mod=function(path.sim,seed,subproblems,replace.sim=TRUE){
        
        if(missing(subproblems)|| is.null(subproblems)) subproblems <- 0
        
        if(subproblems>0){
            section.sim <- paste(section.sim,sprintf("SUBPROBLEMS=%s",subproblems))
        }

        lines.sim <- readLines(path.sim)

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
            section.sim <- sprintf("$SIMULATION ONLYSIM (%s)",seed)
            
            lines.sim <- NMwriteSectionOne(lines=lines.sim,section="$SIMULATION",
                                           newlines="",backup=FALSE,quiet=TRUE)
            
            tab.section.exists <- any(grepl("^(TABLE)$",names.sections))
            if(tab.section.exists){
                lines.sim <- NMwriteSectionOne(lines=lines.sim,
                                               section="TABLE",
                                               newlines=section.sim,
                                               location="before",backup=FALSE,quiet=TRUE)
            } else {
                lines.sim <- NMwriteSectionOne(lines=lines.sim,section="SIMULATION",
                                               newlines=section.sim,location="last",
                                               backup=FALSE,quiet=TRUE)
            }

        }

        ## what to return?
        lines.sim
    }

)


