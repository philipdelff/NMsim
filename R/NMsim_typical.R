NMsim_typical <- list(

    fun.mod=function(path.sim,seed){

        lines.sim <- NMsim_default$fun.mod(path.sim,seed=seed)

        extres <- NMreadExt(fnExtension(path.sim,"ext"))
        Netas <- extres$pars[par.type=="OMEGA",max(i)]

        lines.omega <- paste(c("$OMEGA",rep("0 FIX",Netas,"")),collapse="\n")
        lines.sim <- NMwriteSectionOne(lines=lines.sim,section="omega",newlines=lines.omega,backup=FALSE,quiet=TRUE)


        lines.sim
    }

    

)
