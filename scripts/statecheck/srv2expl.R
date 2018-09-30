#DATA PULLING SEQUENCE
#STEP III: SAMPLE DROPBOX FILES AS EXAMPLES

DUMMYPATH<-"data/misc/DummyPermits/"

get_examples<-function(state){
  statefiles<-sql_to_df(paste0("SELECT * FROM DROPFILES WHERE VALID==1 AND STATEABB=='",state,"'"))
  n_e<-nrow(statefiles)
  if(n_e==0){
    setwd(paste0(PATH,DUMMYPATH,state,"/"))
    files<-list.files()
  }
  else{
    setwd(paste0(PATH,"data/dropbox/",state,"/"))
    files<-sample(list.files(),min(c(5,n_e)))
  }
  sapply(files,function(x)system(paste0("cp '",x,"' ",PATH,"data/examples/",state,"/")))
  setwd(PATH)
}

sapply(STATES$STATEABB,get_examples)

rm(DUMMYPATH)