
setwd(paste0(PATH,"data/dropbox"))

drop_init<-function(){
  file_reqs<-c(STATES$STATEABB,"UNK")
  DROP_STATES<-list.files()
  files_to_make<-is.na(match(file_reqs,DROP_STATES))
  if(any(files_to_make)){
    file_names<-file_reqs[files_to_make]
    sapply(file_names,function(x)system(paste0("mkdir ./",x)))
  }
}

drop_init()

setwd(PATH)

rm(list=c("drop_init"))
