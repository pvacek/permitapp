sql_to_df<-function(q,con=conn){
  query<-dbSendQuery(con,q)
  data<-dbFetch(query)
  return(data)
}

get_db<-function(){
  source("./config/config.R")
  setwd(paste0(PATH,"data/db/"))
  conn<-dbConnect(SQLite(),"permits.db")
  return(conn)
}

if(any(grepl("conn",ls()))==FALSE){
  conn<-get_db()
}

rm("get_db")
setwd(PATH)