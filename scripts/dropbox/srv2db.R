#DATA PULLING SEQUENCE
#STEP II: LOAD DROPBOX FILES INTO DATABASE

DROPFILES<-sql_to_df("SELECT * FROM DROPFILES")

drop_wrapper<-function(row){
  print(paste0("Sending ",row$datapath," to handler..."))
  df<-handler(row,row$STATEABB)
  df$stateline<-NULL #Stateline is meaningless for standalone permits
  return(df)
}

DROPDATA<-do.call(rbind,lapply(1:nrow(DROPFILES),function(i)drop_wrapper(DROPFILES[i,])))

if (dbExistsTable(conn, "DROPDATA")){
  dbRemoveTable(conn, "DROPDATA")
}
dbWriteTable(conn, name = "DROPDATA", value = DROPDATA, row.names = FALSE)

rm(list=c("DROPDATA","DROPFILES"))