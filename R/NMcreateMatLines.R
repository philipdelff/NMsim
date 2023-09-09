NMcreateMatLines <- function(omegas,type){
    omegas.long <- omegas[,.(i,j,value)]
    omegas.long[,maxOff:=0]
    omegas.long[,hasOff:=FALSE]
    omegas.long[,offNonZero:=abs(value)>1e-9&i!=j]
    if(any(omegas.long$offNonZero)){
        omegas.long[,hasOff[any(offNonZero==TRUE)]:=TRUE,by:=i]
    }
    omegas.long[hasOff==TRUE,maxOff:=max(j[abs(value)>1e-9]),by=.(i)]

    is <- unique(omegas.long$i)

    i.idx <- 1
    loopres <- c()
    Netas <- omegas[,max(i)]

    while(i.idx <= length(is)){
        i.this <- is[i.idx]
        nis.block <- omegas.long[i==i.this,unique(maxOff)]
        if(nis.block>0){
            values.this <- omegas.long[i<=(i.this+nis.block)&j<=(i+nis.block)]
            values.this[values.this==0] <- 1e-30
            res <- paste0("BLOCK(,",Netas,") FIX ",paste(values.this,collapse=" "))
            loopres <- c(loopres,res)
            i.idx <- i.idx+nis.block
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
