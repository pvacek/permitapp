#HANDLER: Level 1 States.
#INCLUDES: CO,FL,GA,IL,KS,MA,NM,ND,OK,PA,TX,WA

#handle_topbot<-function(line){
#  line<-str_split(line,"[ ]{2,}")[[1]]
#  line<-line[grep("[A-z]",line)]
#  return(data.frame(Route="",Or=NA,Miles=0,To=line,stringsAsFactors = FALSE))
#}

scrape_lvl1<-function(raw,state=""){
  split<-str_split(raw,"\n")[[1]]
  if(state%in%c("KS","TX","CO","GA","MA","ND","NM")){
    has_mr<-grep("[0-9.]+?[ ]{2,}[A-Z]{1,3}[- ]{0,1}[0-9]{1,4}",split)
    has_et<-grep("[0-9:]+$",split)
    mret<-intersect(has_mr,has_et)
    data<-split[mret]
    if(is.na(data)||length(data)==0){
      return(NULL)
    }
#    top<-bot<-NULL
#    if(grep("Origin",data[1])){
#      top<-handle_topbot(data[1])
#    }
#    data<-data[-1]
#    if(!is.na(data[length(data)])){#Remove footer if it doesn't exist
#      bot<-handle_topbot(data[length(data)])
#    }
#    data<-data[-length(data)]
    data<-field_pop(gsub("[ ]{1,}[0-9:]+$","",data))
    miles<-as.numeric(str_extract(data,"[0-9.]+$"))
    data<-gsub("[ ]+[^ ]+$","",data)[!is.na(miles)]
    route<-str_extract(data,"[A-Z]{1,3}[- ]{0,1}[0-9]{1,4}")
    data<-field_pop(data)
    or<-str_extract(data,"(W(Est|B)*|S(outh|B|WB|EB)*|N(orth|B|WB|EB)*|E(ast|B)*|w|s|n|e)")
    data<-gsub("^.*?[ ]{2,}","",data)
    data<-data.frame(Route=route,Or=or,Miles=c(miles[1],diff(miles)),To=data,stringsAsFactors = FALSE)
    #data<-rbind(top,data,bot)
  }
  else if(state=="OK"){ #OK
    has_mi<-grep("^[0-9.].*?[A-Z]{1,3}[- ]{1}",split)
    has_or<-grep(" (W(EST|B)*|S(OUTH|B|WB|EB)*|N(ORTH|B|WB|EB)*|E(AST|B)*)$",split)
    mior<-intersect(has_mi,has_or)
    data<-split[mior]
    miles<-str_extract(data,"^[0-9.]+")
    data<-field_pop(data)
    or<-str_extract(data," (W(EST|B)*|S(OUTH|B|WB|EB)*|N(ORTH|B|WB|EB)*|E(AST|B)*)$")
    route<-str_extract(data,"[A-Z]{1,3}-[0-9]{1,4}")
    data<-gsub("\\(.*?$","",data)
    data<-data.frame(Route=route,Or=or,Miles=as.numeric(miles),To=data,stringsAsFactors = FALSE)
  }
  else if(state=="IL"){ #IL
    data<-split[grep("^[0-9]{1,2}. \\[.*?\\] ",split)]
    route<-str_extract(data,"[A-Z]{1,3}-[0-9]{1,4}")
    miles<-as.numeric(gsub(" miles","",str_extract(data,"[0-9.]+ miles")))
    data<-gsub(" \\(.*?$","",gsub("^.*?\\] ","",data))
    or<-str_extract(data,"(W(EST|B)*|S(OUTH|B|WB|EB)*|N(ORTH|B|WB|EB)*|E(AST|B)*)")
    data<-data.frame(Route=route,Or=or,Miles=miles,To=data,stringsAsFactors = FALSE)
  }
  else if(state=="PA"){#PA
    data<-split[grep("^[ ]*?[0-9]{1,2}[ ]*?[A-Z]*?[ ]{7,}.*?(North|South|West|East)",split)]
    data<-gsub("^[ ]*[0-9]{1,2}[ ]+[A-Z]+[ ]+","",data)
    route<-gsub("SR0{0,3}","PA",str_extract(data,"^[A-Z0-9]+"))
    data<-data[!is.na(route)]
    #Strategy: Iteratively pop off fields to get data
    data<-field_pop(data)
    or<-str_extract(data,"^[A-z]+")
    data<-field_pop(data)
    miles<-str_extract(data,"^[0-9.]+")
    data<-field_pop(data)
    data<-data.frame(Route=route[!is.na(route)],Or=or,Miles=as.numeric(miles),To=gsub("[ ]{1,}"," ",data),stringsAsFactors = FALSE)
  }
  else if(state=="FL"){ #FL
    has_route<-grep(" [A-Z]{1,3}[- ]{1,2}[0-9]{1,4}",split)
    has_or<-grep(" (W(EST|B)*|S(OUTH|B|WB|EB)*|N(ORTH|B|WB|EB)*|E(AST|B)*)",split)
    rouor<-intersect(has_route,has_or)
    data<-split[rouor]
    data<-gsub("ROUTE.*?:","     ",data)
    data<-gsub("^[ ]{1,}","",data[grep("^[ ]{8,}",data)])
    route<-gsub("SR","FL",str_extract(data,"[A-Z]{1,3}-[0-9]{1,4}"))
    data<-data[!is.na(route)]
    or<-str_extract(data," (W(EST|B)*|S(OUTH|B|WB|EB)*|N(ORTH|B|WB|EB)*|E(AST|B)*) ")
    data<-data.frame(Route=route[!is.na(route)],Or=or,Miles=NA,To="",stringsAsFactors = FALSE)
  }
  else if(state=="WA"){
    signal<-grep("^[ ]*Route Nbr",split)
    if(length(signal)==0){
      return(NULL)
    }
    data<-split[signal[1]:length(split)]
    data<-data[grep("[ ]{2,}[0-9.]+[ ]{2,}[0-9.]+",data)]
    route<-gsub("^[ ]+","",str_extract(data,"^[ ]*?[A-z0-9]+"))
    to<-gsub("[0-9][ ]+","",str_extract(data,"[0-9][ ]{2,}[A-z].+$"))
    data<-data.frame(Route=route,Or=NA,Miles=NA,To=to)
  }
  else{
    data<-NULL
  }
  return(data)
}