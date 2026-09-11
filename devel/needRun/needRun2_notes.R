

needRun2 <- function(obj,env,path.res,funs.unwrap=NULL){

    if(force.run || !file.exists(file.digest)){
        return("mustrun")
    }

    if(is.null(funs.unwrap)){
### this means obj must be a named list
        funs.unwrap <- lapply(obj,function(x)return(identity))
        names(funs.unwrap) <- names(obj)
    }


    ## calc digests, compare
    digs.new <- digestElements(obj,env,funs=funs.unwrap)
    ## compare to digests on file. mustrun if different

### by returning digs.new, the parent function can use this for
### saving once obj has been used in function evaluations.
    return(digs.new)

}

##### myfun3 is a function that internally makes use of needRun2. It
##### would be even better if we could do res <- needRun3(myfun,args,env,path.res,funs.unwrap)

## the big q is how we handle functions from within packages

myfun3 <- function(x,path.res){
    ## not sure this is quite enough. I just put in getArgs() because I think we can use that somehow.
    obj <- getArgs()
    ### fnExtension doesn't matter as long as we use readRDS to read results. 
    path.digs <- fnExtension(fnAppend(path.res,"digests"),".rds")

### env should just be the myfun3 environment. How do I get that?
    must.run <- needRun2(obj,env=,path.digs=path.digs)
    if(must.run){
        res <- x+3
        ## if new digests were provided by needRun2, use those. Otherwise recalc
        if(!is.null(must.run$digest)){
            digs.new <- must.run$digest
        } else {
            digs.new <- digestElements(obj,env=,funs=funs.unwrap)
        }
        saveRDS(res,path.res)
        saveRDS(digs.new,path.digs)

        return(res)
    } else {
        readRDS(path.res)
    }

    res
}




### evaluate object in environent, potentially using funs.unwrap

### 
