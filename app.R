source("handler.R")
source("global.R")


state_choice<-c(toupper(state.name[-c(2,11)]),"UNK")

ui <- dashboardPage(
  dashboardHeader(title="Permit Application"),
  dashboardSidebar(
    sidebarMenu(
    menuItem("Permit Scraping Tool", tabName = "scraper", icon = icon("file")),
    menuItem("Permit QA Tool", tabName = "qa", icon = icon("edit")),
    menuItem("Permit DB Tool", tabName = "db", icon = icon("database")),
    menuItem("Leaflet Demo", tabName = "leaf", icon=icon("globe"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "scraper",scraper_sidebar(),box(uiOutput("ui"),width=9)),
      tabItem(tabName = "qa",p("DISCLAIMER: Results here are not ran through the multi-file handler script.
                      Because of this, there may be minor discrepancies in the data here compared
                      to the data in the main application."),
              selectInput("state",label="Select a State",choices=state_choice),
              actionButton("select",label="View results from this state"),
              uiOutput("STATERES")),
      tabItem(tabName = "db",
                                             fluidRow(box(selectInput(inputId="TBL",label="Choose Table",choices=dbListTables(conn)),
                                             actionButton(inputId="QUERY",label="Send to Query"),width=4),
                                             box(dataTableOutput("DB"),width=8))
              ),
      tabItem(tabName = "leaf",box(p("LEAFLET DEMO"),
                                   leafletOutput("mymap"))
      )
    )
  )
)
server <- function(input, output,session) {
  #SCRAPER
  vals<-reactiveValues()
  vals$switch<-FALSE
  vals$data_present<-FALSE
  vals$files_present<-FALSE
  vals$stateabb<-NULL
  
  data_check<-reactive({
    if(is.null(input$hot)){
      val<-FALSE
    }
    else{
      DF<-hot_to_r(input$hot)
      val<-ifelse(nrow(DF)>0,TRUE,FALSE)
    }
    return(val)
  })
  
  file_check<-reactive({
    if(is.null(input$files)){
      val<-FALSE
    }
    else{
      val<-TRUE
    }
    return(val)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      data<-hot_to_r(input$pdfdf)
      data$Order<-NULL
      data$Keep<-NULL
      write.csv(data, con,row.names=FALSE)
    }
  )
  
  output$downloadJSON <- downloadHandler(
    filename = function() {
      paste('jsondata-', Sys.Date(), '.json', sep='')
    },
    content = function(con) {
      data<-hot_to_r(input$pdfdf)
      state_ord<-as.character(unique(data$state))
      
      stateJSON<-function(state){
        stateabb<-state.abb[match(state,toupper(state.name))]
        list(state=stateabb,waypoints=subset(data[,2:3],data$state==state))
      }
      
      jfile<-toJSON(lapply(state_ord,stateJSON),auto_unbox=TRUE,pretty=TRUE)
      
      write(jfile, con)
    }
  )
  
  output$ui <- renderUI({
    
    vals$data_present<-data_check()
    vals$file_present<-file_check()
    
    readfile<-observeEvent(input$upload,{
      inFiles<-input$files
      n<-min(c(nrow(inFiles),1))
      if(is.null(input$hot)){
        DF<-data.frame(Order=1:n,Keep=rep(TRUE,n),name=inFiles$name,datapath=inFiles$datapath,state=rep("",n),stringsAsFactors=FALSE)
      }
      else{
        DF1<-hot_to_r(input$hot)
        DF2<-data.frame(Order=1:n,Keep=rep(TRUE,n),name=inFiles$name,datapath=inFiles$datapath,state=rep("",n),stringsAsFactors=FALSE)
        DF<-rbind(DF1,DF2)
      }
      DF$Order<-1:nrow(DF)
      DF_hot<-rhandsontable(DF,selectCallback = TRUE,readOnly=FALSE)
      output$hot<-renderRHandsontable(DF_hot)
    },ignoreInit=TRUE)
    
    if(vals$data_present==TRUE){
      filter1<-observeEvent(input$filter1,{
        DF<-hot_to_r(input$hot)
        DF<-DF[DF$Keep==TRUE,]
        DF_hot<-rhandsontable(DF,selectCallback = TRUE,readOnly=FALSE)
        output$hot<-renderRHandsontable(DF_hot)
      },ignoreInit=TRUE)
      
      order1<-observeEvent(input$order1,{
        DF<-hot_to_r(input$hot)
        DF<-DF[order(DF$Order),]
        DF_hot<-rhandsontable(DF,selectCallback = TRUE,readOnly=FALSE)
        output$hot<-renderRHandsontable(DF_hot)
      },ignoreInit=TRUE)
    }
    
    PDFtoCSV<-observeEvent(input$execute,{
      DF<-hot_to_r(input$hot)
      progress<-Progress$new(session,min=1,max=nrow(df))
      on.exit(progress$close())
      result_df<-multihandler(DF,progress)
      vals$switch<-TRUE
      result_df[] <- lapply(result_df, as.character)
      result_df$Order<-1:nrow(result_df)
      result_df$Keep<-TRUE
      RDF_hot<-rhandsontable(result_df,selectCallback = TRUE,readOnly=FALSE)
      output$pdfdf<-renderRHandsontable(RDF_hot)
    },ignoreInit=TRUE)
    
    filter2<-observeEvent(input$filter2,{
      DF<-hot_to_r(input$pdfdf)
      DF<-DF[DF$Keep==TRUE,]
      RDF_hot<-rhandsontable(DF,selectCallback = TRUE,readOnly=FALSE)
      output$pdfdf<-renderRHandsontable(RDF_hot)
    },ignoreInit=TRUE)
    
    order2<-observeEvent(input$order2,{
      DF<-hot_to_r(input$pdfdf)
      DF<-DF[order(DF$Order),]
      RDF_hot<-rhandsontable(DF,selectCallback = TRUE,readOnly=FALSE)
      output$pdfdf<-renderRHandsontable(RDF_hot)
    },ignoreInit=TRUE)
    
    output$switch<-renderText(vals$switch)
    outputOptions(output, "switch", suspendWhenHidden = FALSE)
    output$file_present<-renderText(vals$file_present)
    outputOptions(output, "file_present", suspendWhenHidden = FALSE)
    output$data_present<-renderText(vals$data_present)
    outputOptions(output, "data_present", suspendWhenHidden = FALSE)
    
    clear<-observeEvent(input$Clear,{
      vals$switch<-FALSE
      DF<-rhandsontable(hot_to_r(input$hot)[NULL,],selectCallback=TRUE,readOnly=FALSE)
      output$hot<-renderRHandsontable(DF)
      RDF<-rhandsontable(hot_to_r(input$pdfdf)[NULL,],selectCallback=TRUE,readOnly=FALSE)
      output$pdfdf<-renderRHandsontable(RDF)
    },ignoreInit=TRUE)
    if(vals$switch==TRUE){
      tabBox(title = "Permit App",id= "ttabs", width = 12, height = "800px",
             tabPanel("Input", renderText("This tab is where you input your permits."),
                      rHandsontableOutput("hot"),
                      actionButton("filter1", label = "Filter Rows", value = FALSE),
                      actionButton("order1", label = "Order Rows", value = FALSE)),
             tabPanel("Output", renderText("This tab is where you see your results."),
                      rHandsontableOutput("pdfdf"),
                      actionButton("filter2", label = "Filter Rows", value = FALSE),
                      actionButton("order2", label = "Order Rows", value = FALSE),
                      downloadButton('downloadData', 'Download Data File'),
                      downloadButton('downloadJSON','Download JSON File')))
    }
    else{
      tabBox(title = "Permit App",id= "ttabs", width = 12, height = "800px",
             tabPanel("Input", renderText("This tab is where you input your permits."),
                      rHandsontableOutput("hot"),
                      actionButton("filter1", label = "Filter Rows", value = FALSE),
                      actionButton("order1", label = "Order Rows", value = FALSE)))
    }
  })
  #QA
  STATERES<-eventReactive(input$select,{
    select_state(input$state)
  })
  output$STATERES<-renderUI(STATERES())
  observeEvent(input$Update,{
    vals$stateabb<-state.abb[match(input$state,toupper(state.name))]
    static_data<-sql_to_df(paste0("SELECT * FROM DROPFILES WHERE STATEABB=='",vals$stateabb,"'"))
    n<-nrow(static_data)
    live_comments<-sapply(1:n,function(i)input[[paste0("Comments",i)]])
    comment_diffs<-sapply(1:n,function(i)live_comments[i]!=static_data$COMMENTS[i])
    if(sum(comment_diffs)>0){
      docs_to_update<-static_data$name[comment_diffs]
      new_comments<-live_comments[comment_diffs]
      sapply(1:length(new_comments),function(i)update_comment(docs_to_update[i],new_comments[i]))
    }
  },ignoreInit=TRUE)
  #DATABASE
  make_tbl<-eventReactive(input$QUERY,{
    tbl<-sql_to_df(paste0("SELECT * FROM ",input$TBL," LIMIT 1000"))
    return(tbl)
  })
  output$DB<-renderDataTable(make_tbl())
  #LEAFLET
  output$mymap <- renderLeaflet({
    m <- leaflet(data=routeline) %>% setView(lng = lonlat[1], lat = lonlat[2], zoom = 3)
    m <- m %>% addTiles() %>% addPolylines()
    m <- m  %>% addCircleMarkers(lng=route[1,1],lat=route[1,2],popup="START",color="green")
    m <- m %>% addCircleMarkers(lng=route[n,1],lat=route[n,2],popup="END",color="red")
    m
  })
}

shinyApp(ui, server)