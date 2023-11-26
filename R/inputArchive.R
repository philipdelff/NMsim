##' Default location of input archive file 
##' @param file Path to input or output control stream.
##' @return A file name (character)
##' @import NMdata
##' @export
##' 
inputArchiveDefault <- function(file){
        fn.input <- fnAppend(file,"input")
        fn.input <- fnExtension(fn.input,".rds")
        fn.input
    }
