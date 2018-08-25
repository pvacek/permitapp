#Statecheck II

#Method I

extract_state<-function(pdf){
  state<-gsub("[ .]","",str_extract(toupper(pdf),state_regex))
  return(ifelse(!is.na(state),state,""))
}

#Method II

DOT_test<-function(pdf){
  raw<-pdf_read(pdf)
  lines12<-gsub("[ ]{2,}"," ",paste0(str_split(raw,"\n")[[1]][1:2],collapse=" "))
  gram1<-toupper(str_split(lines12," ")[[1]])
  gram2<-sapply(2:length(gram1),function(x)paste(gram1[x-1],gram1[x]))
  g1_test<-gram1%in%toupper(state.name)
  g2_test<-gram2%in%toupper(state.name)
  if(sum(g1_test)==0 && sum(g2_test)==0){
    return("")
  }
  else if(sum(g2_test)>=sum(g1_test) && sum(g2_test)>0){
    state_full<-gram2[which(g2_test)][1]
  }
  else{
    state_full<-gram1[which(g1_test)][1]
  }
  state<-sql_to_df(paste0("SELECT STATEABB FROM STATES WHERE STATENAME=='",state_full,"'"))[1,1]
  return(state)
}

#Method III

make_words<-function(pdf,state=""){
  raw<-pdf_text(pdf)[1]#Get the first page
  words<-str_split(gsub("[^A-z0-9]"," ",raw)," ")[[1]]
  word_clean<-toupper(words[which(nchar(words)>0)])
  if(nchar(state)==0){
    wdf<-data.frame(PDF=pdf,WORDS=word_clean,stringsAsFactors = FALSE)
  }
  else{
    wdf<-data.frame(PDF=pdf,STATEABB=state,WORDS=word_clean,stringsAsFactors = FALSE)
  }
  return(wdf)
}

tbl_2_df<-function(x){
  tbl<-table(x)
  data.frame(WORDS=names(tbl),COUNTS=as.vector(tbl))
}

make_newvecs<-function(pdfvec){
  wordlist<-lapply(pdfvec,make_words)
  veclist<-lapply(wordlist,function(x)tbl_2_df(x$WORDS))
  return(veclist)
}

query_newvec<-function(vec){
  wq<-paste0("(",paste0("'",vec$WORDS,"'",collapse=","),")")
  query<-sql_to_df(paste0("SELECT * FROM TERMDOC WHERE WORDS IN ",wq,"AND COUNTS>0"))
  query$NEWCOUNTS<-vec$COUNTS[match(query$WORDS,vec$WORDS)]
  prods<-sapply(split(query,query$PDF),function(x)sum(x$COUNTS*x$NEWCOUNTS))
  return(prods)
}

compute_sims<-function(pdfvec){
  veclist<-make_newvecs(pdfvec)
  #Get cross products
  prodmat<-sapply(veclist,query_newvec)
  #Get squares
  newsq<-sapply(veclist,function(x)sqrt(sum(x$COUNTS^2)))
  termdoc<-sql_to_df("SELECT COUNTS,PDF FROM TERMDOC")
  tdmsq<-sqrt(aggregate(COUNTS~PDF,termdoc,function(x)sum(x^2))$COUNTS)
  sqmat<-as.matrix(tdmsq)%*%t(as.vector(newsq))
  #Find similarity
  cos_sim<-prodmat/sqmat
  #Find the name of the closest document
  closest_doc<-rownames(cos_sim)[apply(cos_sim,2,which.max)]
  docmap<-sql_to_df("SELECT DISTINCT PDF,STATEABB FROM TERMDOC")
  state<-docmap$STATEABB[match(closest_doc,docmap$PDF)]
  return(state)
}

state_check<-function(df){
  #STEP 1: Detect STATE in PDF: Simplest, least robust
  states<-sapply(as.character(df$name),extract_state)
  if(all(nchar(states)>0)){
    return(states)
  }
  #STEP 2: Use DOT TEST: More complex, more robust
  states_left<-which(nchar(states)==0)
  states[states_left]<-sapply(states_left,function(i)DOT_test(as.character(df$datapath[i])))
  if(all(nchar(states))>0){
    return(states)
  }
  #STEP 3: Use ML APPROACH: Most complicated but MOST robust
  states_left<-which(nchar(states)==0)
  states[states_left]<-compute_sims(as.character(df$datapath[states_left]))
  return(states)
}
