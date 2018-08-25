#New Handler
setwd("./config")
source("config.R")
source(paste0(PATH,"scripts/db/load_db.R"))
source(paste0(PATH,"statecheck2.R"))

state_regex<-paste0("( |^)(",paste0(state.abb,collapse="|"),")[ .]")
or_regex<-" (W(EST|B)*|S(OUTH|B|WB|EB)*|N(ORTH|B|WB|EB)*|E(AST|B)*)"

STATES<-sql_to_df("SELECT * FROM STATES")

#AUXILIARY FUNCTIONS

pdf_read<-function(pdf){
  return(paste0(pdf_text(pdf),collapse=" "))
}

field_pop<-function(data)return(gsub("^[ ]*[^ ]+[ ]+","",data))

null_data<-function(n=3){
  route_df<-data.frame(Route=c("",rep("",n),""),Or=NA,Miles=NA,To="",stringsAsFactors = FALSE)
  return(route_df)
}

#SCRIPTS

source("./scripts/handler/level1.R") #Level 1 difficulty scraping script.

source("./scripts/handler/level2.R") #Level 2 difficulty scraping script

source("./scripts/handler/level3and4.R") #Levels 3 & 4 difficulty scraping script

source("./scripts/handler/level5.R") #Level 5 difficulty scraping script

#UNIVERSAL CLEANING FUNCTIONS

dupe_cleaner<-function(df){
  route_rle<-rle(df$Route)
  df_split<-split(df,rep(1:length(route_rle$lengths),route_rle$lengths))
  new_df<-do.call(rbind,lapply(1:length(df_split),function(i)df_split[[i]][route_rle$lengths[i],]))
  if(any(!is.na(df$Miles))){
    new_miles<-sapply(df_split,function(x)sum(x$Miles))
    new_df$Miles<-new_miles
  }
  return(new_df)
}

sl_handler<-function(df){
  if(nrow(df)==1){
    df<-rbind(df,df)
  }
  df$stateline<-c("stateline",rep("highway",nrow(df)-2),"stateline")
  return(df)
}

route_cleaner<-function(df,name,state){
  State<-rep(toupper(state.name[match(state,state.abb)]),nrow(df))
  df$Route<-gsub("(SR|SH|CR|SL|BL)",state,df$Route)
  df$Route<-gsub("[^0-9A-z -]","",df$Route)
  df<-data.frame(name=name,State=State,df,stringsAsFactors = FALSE)
  if(any(nchar(df$Route)>0)){
    df<-df[grepl("^X",df$Route)==FALSE,] #Indiana needs exits removed
    df<-dupe_cleaner(df)
  }
  df<-sl_handler(df)
  return(df)
}

#SINGLE HANDLER

handler<-function(row,state,progress=NULL,n=NULL){
  pdf<-as.character(row$datapath)
  raw<-tryCatch(pdf_read(pdf),error=function(e)NULL)
  if(is.null(raw)||nchar(raw)<=100){
    print("Permit is unreadable. Returning null data.")
    return(route_cleaner(null_data(),row$name,state))
  }
  raw<-gsub("−","-",raw)
  level<-STATES$LEVEL[match(state,STATES$STATEABB)]
  if(level==1){
    data<-tryCatch(scrape_lvl1(raw,state),error=function(e)NULL)
    if(is.null(data)){
      level<-2
    }
  }
  if(level==2){
    data<-tryCatch(scrape_lvl2(raw,state),error=function(e)NULL)
    level<-ifelse(is.null(data),5,level)
  }
  if(level==3|level==4){
    data<-scrape_lvl34(raw,state)
    level<-ifelse(is.null(data),5,level)
  }
  if(level==5){
    data<-scrape_lvl5(raw,state)
  }
  if(is.null(data) || nrow(data)==0){
    data<-null_data()
  }
  if(!is.null(progress)){
    progress$inc(1/n,detail=row$name)
  }
  data<-route_cleaner(data,row$name,state)
  return(data)
}

#MULTI HANDLER

multihandler<-function(df,progress=NULL){
  states<-state_check(df)
  n<-nrow(df)
  df_list<-lapply(1:n,function(i)handler(df[i,],states[i],progress,n))
  df_full<-do.call(rbind,df_list)
  df_full$stateline[c(1,nrow(df_full))]<-"address"
  rownames(df_full)<-NULL
  df_full<-df_full[,c(2:3,7,4:6)]
  names(df_full)<-c("state","waypoint","type","orientation","miles","to")
  return(df_full)
}

