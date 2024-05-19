##' @keywords internal

seedFunDefault <- function(n,val.max=2147483647){
    round(runif(n=n)*val.max)
}
