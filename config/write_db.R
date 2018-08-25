#WRITE DATABASE
source("config.R")

setwd(paste0(PATH,"/data/db"))

print("Creating permits.db...")

conn<-dbConnect(SQLite(),"permits.db")

setwd("../misc")

print("Creating STATES...")
STATES<-read.csv("STATES.csv")
dbWriteTable(conn,"STATES",STATES,overwrite=TRUE)

sql_to_df<-function(q,con=conn){
  query<-dbSendQuery(con,q)
  data<-dbFetch(query)
  return(data)
}

setwd(PATH)

source("data_pull.R")
