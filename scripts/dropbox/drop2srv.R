#DATA PULLING SEQUENCE
#STEP I: PULL FROM DROPBOX FOLDER

token<-readRDS(paste0(PATH,"scripts/dropbox/droptoken.rds"))
main<-drop_dir("/Permit Samples - Permits by State/Permits by State",dtoken=token)
DATAPATH<-paste0(PATH,"data/dropbox/")

drop_dl<-function(srv_path,drop_path){
  name<-str_extract(drop_path,"[^/]+.$")
  exists<-file.exists(paste0(srv_path,name))
  if(exists==FALSE){
    drop_download(drop_path,local_path=srv_path,dtoken=token,overwrite=TRUE)
  }
}

pdf_test<-function(pdf){
  tryCatch(ifelse(nchar(paste0(pdf_text(pdf),collapse=" "))>=100,TRUE,FALSE),error=function(e)FALSE)
}

handle_state<-function(row){
  state<-state.abb[match(toupper(row$name),toupper(state.name))]
  state<-ifelse(is.na(state),"UNK",state)
  dir<-paste0(DATAPATH,state,"/")
  subdir<-drop_dir(row$path_display,dtoken=token)
  flist<-subdir$path_display
  sapply(flist,function(x)drop_dl(dir,x))
  drop_data<-data.frame(STATEABB=state,droppath=flist)
  drop_data$name<-list.files(dir)
  drop_data$datapath<-paste0(dir,drop_data$name)
  drop_data$VALID<-sapply(drop_data$datapath,pdf_test)
  drop_data$COMMENTS<-"No comments yet."
  return(drop_data)
}

state_df_list<-lapply(1:nrow(main),function(i)handle_state(main[i,]))
all_states_df<-do.call(rbind,state_df_list)

if (dbExistsTable(conn, "DROPFILES")){
  dbRemoveTable(conn, "DROPFILES")
}
dbWriteTable(conn, name = "DROPFILES", value = all_states_df, row.names = FALSE)

rm(list=c("state_df_list","all_states_df"))
