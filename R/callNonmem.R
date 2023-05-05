##' importFrom NMdata fnExtension

### like execute but in R.
## copy necessary files into temporary
## run nonmem
## copy resulting files back out

## maybe just use bbr?

callNonmem <- function(file.mod,nonmem){
    sprintf("cd %s; %s %s %s; cd -",dirname(file.mod),nonmem,file.mod,fnExtension(file.mod,".lst"))
}
