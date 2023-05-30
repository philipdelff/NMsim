##' importFrom NMdata fnExtension

### like execute but in R.
## copy necessary files into temporary
## run nonmem
## copy resulting files back out

## maybe just use bbr?

callNonmem <- function(file.mod,nonmem){
    sprintf("cd %s; %s %s %s; cd -",dirname(file.mod),nonmem,file.mod,fnExtension(file.mod,".lst"))
}

execNonmem <- function(file.mod,nonmem){
### create temporary directory

### copy .mod and input data to temp dir. Call input data modelname_input.csv

### modify .mod to use local copy of input data

### execute nonmem 

### copy wanted files back to orig location of file.mod

### optionally remove temporary dir
}
