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
    actionButton("Clear", "Clear Results"),
    actionButton("Post","Send data to Routing API")
  )
  ,width=3)
}

scraper_input<-function(statenames,files){
  tabPanel("Input",fluidRow(renderText("If you want to put a starting point, you may fill in the info below"),
                    orderInput(inputId = 'foo', label = 'Order permits here (left to right, top to bottom)', items = files),
                    p("The scraper tool reads this table from top to bottom."),
                    tableOutput('order')),
                    fluidRow(selectInput("startType","Select type of starting point",
                                choices=c("address","highway","stateline","intersection")),
                    textInput("startWaypoint","Enter the waypoint"),
                    selectInput("startState","Select the starting state",choices=statenames),
                    checkboxInput("useStart","Check this box to include the custom starting point."))
           )
}

scraper_output<-function(switch){
  if(switch==FALSE){
    return(tabPanel("",p("")))
  }
  else{
    return(tabPanel("Output", renderText("This tab is where you see your results."),
                    rHandsontableOutput("pdfdf"),
                    actionButton('sort','Sort Data'),
                    downloadButton('downloadData', 'Download Data File'),
                    conditionalPanel(
                      condition = "output.mapstatus == 'TRUE'",
                      downloadButton('downloadGPX', 'Download GPX File')
                    )
                    )
    )
  }
}

scraper_pdf<-function(){
  tabPanel("PdfView",uiOutput("pdfview"))
}

scraper_map<-function(){
  tabPanel("Map",textOutput("maperr"),leafletOutput("mymap"))
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

null_map<-function(){
  m <- leaflet() %>% setView(lng = -100, lat = 41, zoom = 3) %>% addTiles()
  m
}

error_message<-function(content){
  return(paste0("ERROR in ",content$err_file," on line ",content$err_line,
                ". MESSAGE: ",content$message))
}

POST2MAP<-function(content){
  coords<-content$coordinates
  wps<-content$waypoints
  n<-length(coords)
  route<-matrix(unlist(coords),n,2,byrow=TRUE)
  routeline<-Line(route)
  lonlat<-apply(route,2,median)
  m <- leaflet(data=routeline) %>% setView(lng = lonlat[1], lat = lonlat[2], zoom = 3)
  m <- m %>% addTiles() %>% addPolylines()
  for(i in 1:length(wps)){
    colchoice<-ifelse(i==1,"green",ifelse(i==length(wps),"red","yellow"))
    waypoint<-unlist(wps[[i]])
    m<- m %>% addCircleMarkers(lng=waypoint[1],lat=waypoint[2],
                               popup=paste0("Waypoint #",i),color=colchoice)
  }
  return(m)
}

POST2GPX<-function(coords){
  coords_df<-data.frame(lon=sapply(coords,function(x)x[[1]]),lat=sapply(coords,function(x)x[[2]]))
  write.csv(coords_df,file="coords.csv",row.names=FALSE)
  #system command: gpsbabel
  system(paste0("gpsbabel -r -i unicsv -f coords.csv -o gpx -F way.gpx"),intern=TRUE)
  way<-readLines("way.gpx")
  way2<-way[-grep("name",way)]
  writeLines(way2,"way.gpx")
}