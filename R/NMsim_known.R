if(F){

if(type.sim=="known"){
    
###### todo
    ## cant allow disjoint id's in data
    ## fail if ID's are non-unique in phi
    ## only one table in orig phi supported
    ## only type.mod="est" allowed (phi is used)

    ## phi file required
### read phi file and select subjects to be simulated
    path.phi <- fnExtension(path.mod,".phi")
    phi <- NMreadTab(path.phi)

    data[,rowtmp:=.I]
    dt.id.order <- data[,.SD[1],by=.(ID=as.character(ID)),.SDcols=cc(rowtmp)]
    
    
    ## phi <- mergeCheck(phi,dt.id.order,by="ID",all.x=TRUE)
    ## phi <- phi[!is.na(rowtmp)]
    ## setorder(phi,rowtmp)
### save .phi file for simulation. Table number always 1.
    ## fwrite(phi,file=path.phi.sim,sep=" ",quote=FALSE
    ##       ,row.names= FALSE
    ##       ,scipen=0)

    
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
    }
    phi.use <- rbind(phi.lines[is.data==FALSE,.(text)],phi.use,fill=TRUE)

    path.phi.sim <- fnAppend(fnExtension(path.sim,".phi"),"input")
    con.newphi <- file(path.phi.sim, "wb")
    writeLines(phi.use[,text], con = con.newphi)
    close(con.newphi)
    files.needed <- c(files.needed,path.phi.sim)

### prepare simulation control stream
    ## get rid of any $ETAS sections
    lines.new <- sprintf("$ETAS FILE=%s  FORMAT=s1pE15.8 TBLN=1
$ESTIMATION  MAXEVAL=0 NOABORT METHOD=1 INTERACTION FNLETA=2",basename(path.phi.sim))

    NMwriteSection(files=path.sim,section="estimation",location="replace",
                   newlines=lines.new,backup=FALSE,quiet=TRUE)
}
}
