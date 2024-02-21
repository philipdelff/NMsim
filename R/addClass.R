## don't export

addClass <- function(data,class){
    allclasses <- class(data)
    if(!class %in% allclasses) allclasses <- c(class,allclasses)
    setattr(data,"class",allclasses)
}
