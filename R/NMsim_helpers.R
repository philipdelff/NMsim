##' @keywords internal
##' 
adjust.method.update.inits <- function(method.update.inits,system.type,dir.psn,cmd.update.inits,file.ext){

    psn <- NULL
    nmsim <- NULL
    none <- NULL
    
    ## method.update.inits
    if(missing(method.update.inits)) method.update.inits <- NULL
    ## if method.execute is psn, default is psn. If not, it is NMsim.
    if(is.null(method.update.inits)) {
        method.update.inits <- "psn"
        cmd.update.inits <- file.psn(dir.psn,"update_inits")

        if(system.type=="windows"){
            ## We have seen problems with PSN on windows. Until
            ## clarified, internal method prefered on win.
            method.update.inits <- "nmsim"
        }
        
        ## check if update_inits is avail
        ## if(suppressWarnings(system(paste(cmd.update.inits,"-h"),show.output.on.console=FALSE)!=0)){
        if(system.type=="linux"){
            which.found <- system(paste("which",cmd.update.inits),ignore.stdout=T)
            if(which.found!=0){
                method.update.inits <- "nmsim"
                rm(cmd.update.inits)
            }
        }
    }

    
    method.update.inits <- simpleCharArg("method.update.inits",method.update.inits,"nmsim",cc(psn,nmsim,none))
    ## if update.inits with psn, it needs to be available
    if(method.update.inits=="psn"){
        cmd.update.inits <- file.psn(dir.psn,"update_inits")
        if(system.type=="linux" && suppressWarnings(system(paste(cmd.update.inits,"-h"),ignore.stdout = TRUE)!=0)){
            stop('Attempting to use PSN\'s update_inits but it was not found. Look at the dir.psn argument or use method.update.inits="nmsim"')
        }
    }

    if(!is.null(file.ext) && method.update.inits=="psn"){
        stop("argument `file.ext` is not allowed when method.update.inits==\"psn\"")
    }


    
    method.update.inits
}


##' Drop spaces and odd characters. Use to ensure generated file names
##' are usable.
##' @param x a string to clean
##' @keywords internal
##' @examples
##' NMsim:::cleanStrings("e w% # ff!l3:t,3?.csv")
##' NMsim:::cleanStrings("3!?:#;<>=, {}|=g+&-
##' .csv")

cleanStrings <- function(x){
    ## x <- gsub(" ","",as.character(x))
    ## x <- gsub("[[:punct:]]", "", x)
    ##  *^$@~% []
    ## x <- gsub("[ !?#:;<>/,[]\\{\\}\\|-=+&]", "", x)


    x <- gsub("[ +!?#:;<>&/,\\{\\}\\|=]", "",x) 
    x <- gsub(pattern="-",replacement="",x=x,perl=TRUE) 
    x <- gsub(pattern="\n",replacement="",x=x)
    
    x
}

file.psn <- function(dir.psn,file.psn){
    if(dir.psn=="none") stop("PSN not found")
    if(dir.psn=="") return(file.psn)
    file.path(dir.psn,file.psn)
}
