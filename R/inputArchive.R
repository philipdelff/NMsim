

##' @export
inputArchiveDefault <- function(file){
        fn.input <- fnAppend(file,"input")
        fn.input <- fnExtension(fn.input,".rds")
        fn.input
    }
