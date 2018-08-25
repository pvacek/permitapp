#HANDLER: Level 3 and 4 States.
#INCLUDES (3): AL,IN,LA,MS,NE,NV,NY,SC,TN,UT,WI
#INCLUDES (4): CA,CT,DE,ID,VA

scrape_lvl34<-function(raw,state,s=3){
  signal<-STATES$SIGNAL[match(state,STATES$STATEABB)]
  split<-str_split(raw,"\n")[[1]]
  start<-grep(signal,split)
  if(length(start)>0){
    data<-paste0(split[start[1]:(start[1]+s)],collapse=" ")
    data1<-str_extract_all(data,"[A-Z]{1,3}[-]{0,1}[0-9]{1,4}[ A-Z]{0,3}")[[1]]
    data2<-str_extract_all(data,"[0-9]{1,3}[NSWE]{1}[B]{0,1}")[[1]]
    data3<-str_extract_all(data,"[0-9]{1,3}[-, ]{1}")[[1]]
    if(length(data1)>0){
      data<-data1
      or<-str_extract(data,"(W(EST|B)*|S(OUTH|B|WB|EB)*|N(ORTH|B|WB|EB)*|E(AST|B)*)$")
      route<-str_extract(data,"^[A-Z]{1,3}[-]{0,1}[0-9]{1,4}")
      data<-data.frame(Route=route,Or=or,Miles=NA,To="",stringsAsFactors = FALSE)
    }
    else if(length(data2)>0){
      data<-data2
      or<-str_extract(data,"(W(EST|B)*|S(OUTH|B|WB|EB)*|N(ORTH|B|WB|EB)*|E(AST|B)*)$")
      route<-str_extract(data,"^[0-9]{1,3}")
      data<-data.frame(Route=route,Or=or,Miles=NA,To="",stringsAsFactors = FALSE)
    }
    else if(length(data3)>0){
      data<-data3
      route<-str_extract(data,"^[0-9]{1,3}")
      data<-data.frame(Route=route,Or=NA,Miles=NA,To="",stringsAsFactors = FALSE)
    }
    else{
      data<-NULL
    }
  }
  else{
    data<-NULL
  }
  return(data)
}