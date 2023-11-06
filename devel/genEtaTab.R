genEtaTab <- function(file.mod,file.ext){

    ## determine number of etas based on ext file
    ext <- NMreadExt(file.ext)
    Netas <- ext$pars[par.type=="omega",max(i)]
    
    text.tab <- paste("ID",paste(paste0("ETA",Netas),collapse=" "),"FIRSTONLY NOPRINT NOAPPEND")

    text.tab
}
