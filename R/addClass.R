## don't export

addClass <- function(data,class){

    setattr(data,"class",c(class,class(data)))
}
