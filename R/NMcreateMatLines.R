##' Create text lines for OMEGA and SIGMA Nonmem sections
##'
##' @param omegas See NMreadExt and the pars element returned by that
##'     function.
##' @param type The matrix type. OMEGA or SIGMA - case in-sensitive.
##' @return Character vector
##'
##' @keywords internal

NMcreateMatLines <- function(omegas,type){

    . <- NULL
    j <- NULL
    i <- NULL
    value <- NULL
    maxOff <- NULL
    hasOff <- NULL
    offNonZero <- NULL

    ## the code was written in the oppositie direction, so switching i
    ## and j.
    omegas.long <- omegas[,.(i=j,j=i,value)]
    omegas.long[,maxOff:=0]
    omegas.long[,hasOff:=FALSE]
    omegas.long[,offNonZero:=abs(value)>1e-9&i!=j]

    
    if(any(omegas.long$offNonZero)){
        omegas.long[,hasOff:=any(offNonZero==TRUE),by=.(i)]
    }
    omegas.long[hasOff==TRUE,maxOff:=max(j[abs(value)>1e-9]-i),by=.(i)]

    
    
    is <- unique(omegas.long$i)

    i.idx <- 1
    loopres <- c()
    Netas <- omegas[,max(i)]


    
    while(i.idx <= length(is)){
        i.this <- is[i.idx]
        nis.block <- omegas.long[i==i.this,unique(maxOff)]
        if(nis.block>0){
            
            values.this <- omegas.long[i>=i.this&i<=(i.this+nis.block)&j<=(i.this+nis.block),value]
            values.this[values.this==0] <- 1e-30
            res <- paste0("BLOCK(",nis.block+1,") FIX ",paste(values.this,collapse=" "))
            loopres <- c(loopres,res)
            i.idx <- i.idx+nis.block+1
        } else {
            value.this <- omegas.long[i==i.this&j==i.this,value]
            res <- paste(value.this)
            if(value.this==0){
                res <- paste(res,"FIX")
            } 
            loopres <- c(loopres,res)
            i.idx <- i.idx+1
        }
    }
    lines.mat <- paste(paste0("$",toupper(type)),loopres)
    
    return(lines.mat)
}

