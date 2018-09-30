#DATA PULLING SEQUENCE
#STEP IV: EXTRACT WORD DATA FROM EXAMPLE PERMITS

make_tdm<-function(perm_df){
  word_set<-sort(unique(perm_df$WORDS))
  tdm<-do.call(rbind,lapply(split(perm_df,perm_df$PDF),function(x)data.frame(PDF=x$PDF[1],STATEABB=x$STATEABB[1],tbl_2_df(x$WORDS))))
  rownames(tdm)<-NULL
  return(tdm)
}

tdm_to_sql<-function(tdm){
  if (dbExistsTable(conn, "TERMDOC")){
    dbRemoveTable(conn, "TERMDOC")
  }
  dbWriteTable(conn, name = "TERMDOC", value = tdm, row.names = FALSE)
}

make_words_wrapper<-function(pdf,state=""){
  tryCatch(make_words(pdf,state),error=function(e)NULL)
}

tdm_write<-function(){
  setwd(paste0(PATH,"data/examples/"))
  perm_list<-sapply(STATES$STATEABB,function(x)paste0(getwd(),"/",x,"/",list.files(paste0("./",x))))
  states<-rep(names(perm_list),times=sapply(perm_list,length))
  perms<-unlist(perm_list)
  names(perms)<-NULL
  perm_df<-do.call(rbind,lapply(1:length(perms),function(i)make_words_wrapper(perms[i],states[i])))
  tdm<-make_tdm(perm_df)
  tdm_to_sql(tdm)
  setwd(PATH)
  return(tdm)
}

tdm_write()
