
setwd(paste0(PATH,"data/examples"))

expl_init<-function(){
  file_reqs<-c(STATES$STATEABB)
  EXPL_STATES<-list.files()
  files_to_make<-is.na(match(file_reqs,EXPL_STATES))
  if(any(files_to_make)){
    file_names<-file_reqs[files_to_make]
    sapply(file_names,function(x)system(paste0("mkdir ./",x)))
  }
}

expl_init()

setwd(PATH)

rm(list=c("expl_init"))
