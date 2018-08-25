#SCRAPER

scraper_sidebar<-function(){
  box(
  fileInput("files", "Choose Pdf File",accept = c(".pdf"),multiple=TRUE),
  conditionalPanel(
    condition = "output.file_present == 'TRUE'",
    actionButton("upload", "Upload")
  ),
  conditionalPanel(
    condition = "output.data_present == 'TRUE'",
    actionButton("execute", "Execute")
  ),
  conditionalPanel(
    condition = "output.switch == 'TRUE'",
    actionButton("Clear", "Clear Results")
  )
  ,width=3)
}

#QA

single_button<-function(val){
  col<-ifelse(val==0,"red",ifelse(val>=5,"green","yellow"))
  valueBox(
    val, names(val),
    color = col
    ,width=2)
}

button_row<-function(vals){
  fluidRow(tagList(lapply(1:length(vals),function(i)single_button(vals[i]))))
}

get_state_buttons<-function(){
  STATEABB<-sql_to_df("SELECT STATEABB FROM DROPFILES WHERE VALID==1")$STATEABB
  STATE_COUNT<-sort(table(factor(STATEABB,levels=STATES$STATEABB)),decreasing=TRUE)
  STATE_SPLIT<-split(STATE_COUNT,rep(1:8,each=6))
  button_list<-tagList(lapply(STATE_SPLIT,button_row))
  return(button_list)
}

render_permit<-function(df,i){
  fname<-df$name[1]
  link<-a(fname,href=paste0("https://dropbox.com/home",df$droppath[1]))
  status<-p(paste0("Readable? "),ifelse(df$VALID[1]==1,"Yes","No"))
  df$name<-NULL
  table<-renderTable(df[,2:5])
  comments<-textAreaInput(inputId=paste0("Comments",i),label="Comments",df[1,9])
  return(fluidRow(box(link,status,comments),box(table)))
}

select_state<-function(state){
  base_q<-"SELECT * FROM DROPDATA INNER JOIN DROPFILES ON DROPDATA.name==DROPFILES.name WHERE DROPDATA.State=='"
  data<-sql_to_df(paste0(base_q,state,"'"))[,-c(7,9)]
  if(nrow(data)==0){
    return(tagList(p("This state does not have any permits.")))
  }
  data_list<-split(data,data$name)
  state_ui<-tagList(actionButton(inputId="Update",label="Update Comments"),
                    lapply(1:length(data_list),function(i)render_permit(data_list[[i]],i)))
  return(state_ui)
}

update_comment<-function(name,comment="No comments yet!"){
  sql_to_df(paste0("UPDATE DROPFILES SET COMMENTS='",comment,"' WHERE name=='",name,"'"))
}

#DB

#LEAFLET

route<-fromJSON("./data/misc/example-results-309.json")$coordinates
n<-nrow(route)
lonlat<-apply(route,2,median)
routeline<-Line(route)