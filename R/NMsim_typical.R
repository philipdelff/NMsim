if(F){

    if(type.sim=="typical"){

        extres <- NMreadExt(fnExtension(path.mod,"ext"))
        Netas <- extres$pars[par.type=="OMEGA",max(i)]

### creates a full block of zeros. Works but unnecessarily large.
        ## lines.omega <- sprintf("$OMEGA BLOCK(%d)\n 0 FIX %s",Nomegas,paste(rep(0,(Nomegas**2-Nomegas)/2+Nomegas-1),collapse=" "))
        lines.omega <- paste(c("$OMEGA",rep("0 FIX",Netas,"")),collapse="\n")
        NMwriteSection(files=path.sim,section="omega",newlines=lines.omega,backup=FALSE,quiet=TRUE)
    }

    }
