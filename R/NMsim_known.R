NMsim_known <- function(path.sim,path.mod,data.sim,return.text=FALSE){

    
    
    path.phi.sim <- fnAppend(fnExtension(path.sim,".phi"),"input")
    files.needed.def <- NMsim_default(path.sim=path.sim,path.mod=path.mod,data.sim=data.sim)

    lines.sim <- readLines(path.sim)
    
### prepare simulation control stream
    ## get rid of any $ETAS sections
    lines.sim <- NMdata:::NMwriteSectionOne(lines=lines.sim,section="$ETAS",newlines="",backup=FALSE,quiet=TRUE)
    
    lines.new <- sprintf("$ETAS FILE=%s  FORMAT=s1pE15.8 TBLN=1
$ESTIMATION  MAXEVAL=0 NOABORT METHOD=1 INTERACTION FNLETA=2",basename(path.phi.sim))
    
    lines.sim <- NMwriteSectionOne(lines=lines.sim,section="TABLE",location="before",
                                   newlines=lines.new,backup=FALSE,quiet=TRUE)

    
#### .mod done
    ##         lines.sim

    ##     }
    ## ,

    ## fun.phi = function(path.sim,path.mod,data.sim){

    ## simulation data is needed 
    
###### todo
    ## cant allow disjoint id's in data
    ## fail if ID's are non-unique in phi
    ## only one table in orig phi supported
    ## only type.mod="est" allowed (phi is used)

    ## phi file required
### read estimation phi file and select subjects to be simulated
    path.phi <- fnExtension(path.mod,".phi")
    phi <- NMreadTab(path.phi)

    data.sim[,rowtmp:=.I]
    dt.id.order <- data.sim[,.SD[1],by=.(ID=as.character(ID)),.SDcols=cc(rowtmp)]

    phi.lines <- data.table(text=readLines(path.phi))
    phi.lines[,n:=.I]        
    phi.lines[,is.data:=!grepl("[a-zABCDFGHIJKLMNOPQSTUVWXYZ]",x=text)]
    phi.lines[is.data==TRUE,textmod:=gsub(" +"," ",text)]
    phi.lines[is.data==TRUE,textmod:=gsub("^ +","",textmod)]
    phi.lines[is.data==TRUE,ID:=strsplit(textmod,split=" ")[[1]][2],by=.(n)]
    ## phi.lines

    phi.use <- mergeCheck(dt.id.order[,.(ID)],phi.lines[,.(ID,text)],by=cc(ID),all.x=TRUE)
### Error if subjects in data are not found in phi
    if(phi.use[,any(is.na(text))]){
        message("IDs not found in nonmem results (phi file):", paste(phi.use[is.na(text),ID],collapse=", "))
        phi.use <- phi.use[!is.na(text)]
    }
    phi.use <- rbind(phi.lines[is.data==FALSE,.(text)],phi.use,fill=TRUE)

    lines.phi <- phi.use[,text]
    path.phi.sim <- fnAppend(fnExtension(path.sim,".phi"),"input")

    if(return.text){
        return(list(mod=lines.sim,
                    phi=lines.phi))
    }
    
    writeTextFile(lines=lines.sim,file=path.sim)
    
    writeTextFile(lines.phi,path.phi.sim)

    files.needed <- data.table(path.sim=path.sim,files.needed=path.phi.sim)
    files.needed
}

