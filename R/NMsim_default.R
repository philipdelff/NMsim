NMsim_default <- list(

    fun.mod=function(lines.mod,seed,subproblems,replace.sim=TRUE){
        
        if(missing(subproblems)|| is.null(subproblems)) subproblems <- 0
        
        section.sim <- sprintf("$SIMULATION ONLYSIM (%s)",seed)
        if(subproblems>0){
            section.sim <- paste(section.sim,sprintf("SUBPROBLEMS=%s",subproblems))
        }
        

        sections.mod <- NMreadSection(lines=lines.mod)
        names.sections <- names(sections.mod)

        lines.sim <- lines.mod
        
        ## remove any estim or sim. Insert $SIM before $TABLE. If not $TABLE, insert $SIM in the bottom.
        ## remove EST
        lines.sim <- NMdata:::NMwriteSectionOne(lines=lines.sim,section="$ESTIMATION",newlines="",backup=FALSE,quiet=TRUE)
        
        ## remove COV
        lines.sim <- NMdata:::NMwriteSectionOne(lines=lines.sim,section="$COVARIANCE",newlines="",backup=FALSE,quiet=TRUE)

        ## replace SIM - the user may not want this
        if(replace.sim){
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
