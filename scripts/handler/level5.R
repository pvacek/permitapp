#HANDLER: Level 5 States.
#INCLUDES: KY,ME,NH,OR,RI,VT,WY

scrape_lvl5<-function(raw,state,k=.1){
  split<-str_split(raw,"\n")[[1]]
  #Numcaps test
  numcaps<-sapply(str_extract_all(split,"[0-9A-Z]"),length)
  size<-nchar(split)
  split_lean<-split[(numcaps/size)>k]
  lean<-paste(split_lean,collapse=" ")
  routes<-str_extract_all(lean,".{3}[0-9]{1,4}")[[1]]
  dtest<-routes[grep(paste0("(I|US|SR|SH|IH",state,")-[0-9]{1,4}"),routes)]
  stest<-routes[grep(paste0("(I|US|SR|SH|IH",state,") [0-9]{1,4}"),routes)]
  nstest<-routes[grep(paste0("(I|US|SR|SH|IH",state,")[0-9]{1,4}"),routes)]
  all_routes<-unique(c(dtest,stest,nstest))
  all_routes<-paste0("(",paste0(gsub("[^A-Z0-9 -]","",all_routes),collapse="|"),")")
  data<-split_lean[grepl(all_routes,split_lean)]
  data<-paste(data[grepl("INFORMATIONAL|SCHEDULED|NOTICE|RESTRICT",data)==FALSE],collapse=" ")
  ratio<-nchar(data)/nchar(lean)
  rouor<-str_extract_all(data,"[A-Z]{1,3}[-]{0,1}[0-9]{1,4}.{3}")[[1]]
  if(ratio>=0.4|| ratio == 0 || length(rouor)==0){
    return(NULL)
  }
  data<-str_split(rouor," ")
  route<-sapply(data,function(x)x[1])
  or<-sapply(data,function(x)str_extract(x[2],"(W(EST|B)*|S(OUTH|B|WB|EB)*|N(ORTH|B|WB|EB)*|E(AST|B)*)"))
  data<-data.frame(Route=route,Or=or,Miles=NA,To="",stringsAsFactors = FALSE)
  return(data)
}