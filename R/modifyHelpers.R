#### Do not export these functions. They should just be available to define arguments in `modify.model`

add <- function(...,.pos="bottom"){

    switch(.pos,
           top=function(x)c(unlist(...),x),
           bottom=function(x)c(x,unlist(...))
           )
    
}

overwrite <- function(...){

    function(x){
        gsub(...,x=x)
    }
}
