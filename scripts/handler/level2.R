#HANDLER: Level 2 States.
#INCLUDES: AZ,AR,IA,MD,MI,MN,MO,MT,NJ,NC,OH,SD,WV

scrape_lvl2<-function(raw,state){
  raw<-gsub("−","-",raw)
  split<-str_split(raw,"\n")[[1]]
  raw_noline<-paste(split,collapse=" ")
  test_2A<-str_extract(raw_noline,"START ON.*?END[ ]{1,}ON")
  test_2B<-grep("Via Highways ",split)
  if(!is.na(test_2A)){
    data<-str_extract(raw_noline,"START ON.*?END[ ]{1,}ON")
    end<-gsub("(END[ ]{1,}ON|\n)","",str_extract(raw,"END[ ]{1,}ON.*?\n"))
    data<-paste0(data,end)
  }
  else if(length(test_2B)>0){
    data<-split[test_2B+1]
    data<-paste(data,collapse=" ")
  }
  else if(state=="MT"){ #MT
    MT_start<-grep("[ ]*?From[ ]*?To[ ]*?ViaRoute",split)
    data<-paste(split[MT_start+1],collapse=" ")
  }
  else if(state=="NJ"){ #NJ
    start<-grep(" Routes: START",split)
    end<-grep("[ ]{5,}END",split)
    data<-paste(split[start:end],collapse=" ")
  }
  else{
    return(NULL)
  }
  data<-gsub("[ ]{2,}"," ",data)
  rouor<-str_extract_all(data,"[A-Z]{1,3}[-]{0,1}[0-9//]{1,4}[ A-Z0-9.,]{1,4}")[[1]]
  data<-str_split(rouor," ")
  route<-sapply(data,function(x)x[1])
  or<-sapply(data,function(x)str_extract(x[2],"(W(EST|B)*|S(OUTH|B|WB|EB)*|N(ORTH|B|WB|EB)*|E(AST|B)*)"))
  data<-data.frame(Route=route,Or=or,Miles=NA,To="")
  return(data)
}
