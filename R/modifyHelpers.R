##' Create function that adds text elements to vector
##' @param ... Elements to add.
##' @param .pos Either \"top\" or \"bottom\". Decides if new text is prepended or appended to existing text.
##' @return A function that adds the specified text to character vectors
##' @examples
##' myfun <- add("b","d")
##' myfun("a")
##' myfun2 <- add("b","d",.pos="top")
##' myfun2("a")
##' @export
add <- function(...,.pos="bottom"){
    
    switch(.pos,
           top=function(x)c(unlist(list(...)),x),
           bottom=function(x)c(x,unlist(list(...)))
           )
    
}

##' Create function that modifies text elements in a vector
##' @param ... Passed to `gsub()`
##' @return A function that runs `gsub` to character vectors
##' @examples
##' myfun <- overwrite("b","d")
##' myfun(c("a","b","c","abc"))
##' ## regular expressions
##' myfun2 <- overwrite("b.*","d")
##' myfun2(c("a","b","c","abc"))
##' @export
overwrite <- function(...){

    function(x){
        gsub(...,x=x)
    }
}
