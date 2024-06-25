#### Do not export these functions. They should just be available to define arguments in `modify.model`

add <- function(...,.pos="end"){

    switch(.pos,
           end=function(x)c(x,unlist(...))
           )
    
}

overwrite <- function(...){

    function(x){
        gsub(...,x)
    }
}
