source("handler.R")
statenames<-c("None",sql_to_df("SELECT * FROM STATES")$STATENAME)
source("global.R")

state_choice<-c(toupper(state.name[-c(2,11)]),"UNK")

ui <- dashboardPage(
  dashboardHeader(title="Permit Application"),
  dashboardSidebar(
    sidebarMenu(
    menuItem("Permit Scraping Tool", tabName = "scraper", icon = icon("file")),
    menuItem("Permit QA Tool", tabName = "qa", icon = icon("edit")),
    menuItem("Permit DB Tool", tabName = "db", icon = icon("database"))
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
  vals$mapstatus<-FALSE
  vals$filedata<-NULL
  
  data_check<-reactive({
    if(is.null(vals$filedata)){
      val<-FALSE
    }
    else{
      val<-TRUE
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
      write.csv(data, con,row.names=FALSE)
    }
  )
  
  output$downloadGPX <- downloadHandler(
    filename = function() {
      paste('gpx-', Sys.Date(), '.gpx', sep='')
    },
    
    content = function(file) {
      file.copy("way.gpx", file)
    }
  )
  
  output$ui <- renderUI({
    
    vals$data_present<-data_check()
    vals$file_present<-file_check()
    
    observeEvent(input$upload,{
      inFiles<-input$files
      if(is.null(input$foo_order)){
        vals$filedata<-data.frame(name=inFiles$name,datapath=inFiles$datapath)
      }else{
        df1<-vals$filedata
        df2<-data.frame(name=inFiles$name,datapath=inFiles$datapath)
        DF<-rbind(df1,df2)
        vals$filedata<-DF[duplicated(DF)==FALSE,]
      }
    },ignoreInit=TRUE,once=TRUE)
    
    output$order <- renderTable({vals$filedata[as.numeric(input$foo_order),]})
    
    PDFtoCSV<-observeEvent(input$execute,{
      DF<-vals$filedata[as.numeric(input$foo_order),]
      progress<-Progress$new(session,min=1,max=nrow(df))
      on.exit(progress$close())
      result_df<-multihandler(DF,progress)
      vals$switch<-TRUE
      result_df[] <- lapply(result_df, as.character)
      if(input$useStart==TRUE){
        type<-input$startType
        wpt<-input$startWaypoint
        state<-ifelse(input$startState=="None",result_df$state[1],input$startState)
        header<-data.frame(state=state,waypoint=wpt,type=type,
                           orientation="",miles=0,to=paste0("Start at ",wpt),
                           stringsAsFactors = FALSE)
        result_df<-rbind(header,result_df)
      }
      result_df$state<-factor(result_df$state,levels=statenames[-1])
      result_df$Order<-1:nrow(result_df)
      result_df$type<-factor(result_df$type,levels=c("address","highway","stateline","intersection"))
      RDF_hot<-rhandsontable(result_df,selectCallback = TRUE,readOnly=FALSE)  %>%
        hot_cols(columnSorting = TRUE) %>% 
        hot_col(col = "type", type = "dropdown", source = c("address","highway","stateline","intersection"))
      output$pdfdf<-renderRHandsontable(RDF_hot)
    },ignoreInit=TRUE)
    
    DFtoJSON<-observeEvent(input$Post,{
      data<-hot_to_r(input$pdfdf)
      state_ord<-as.character(unique(data$state))
      
      stateJSON<-function(state){
        stateabb<-state.abb[match(state,toupper(state.name))]
        list(state=stateabb,waypoints=subset(data[,2:3],data$state==state))
      }
      write(toJSON(lapply(state_ord,stateJSON),auto_unbox=TRUE,pretty=TRUE), "test.json")
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Sending route to API...", value = 0)
      posting<-tryCatch(POST(url="http://159.65.98.104:6000",body=fromJSON("test.json"),encode="json"),
                        error=function(e){
                          print(e)
                          return(NA)
                          })
      #posting<-POST(url="http://159.65.98.104:6000",body=fromJSON("test.json"),encode="json")
      progress$set(message = "Received post from API.", value = 1)
      if(length(posting)==1){
        m<-null_map()
        maperror<-"ERROR: Connection to API failed."
      }else if(length(posting)>1){
        saveRDS(posting,"posting.rds")
        cont<-content(posting)
        maperror<-ifelse(cont$status=="ERROR",error_message(cont),"")
        if(length(cont$coordinates)>0){
          POST2GPX(cont$coordinates)
          m<-POST2MAP(cont)
        }
        else{
          m<-null_map()
        }
      }
      vals$mapstatus<-TRUE
      output$maperr<-renderText(maperror)
      output$mymap<-renderLeaflet(m)
    },ignoreInit=TRUE)
    
    output$switch<-renderText(vals$switch)
    outputOptions(output, "switch", suspendWhenHidden = FALSE)
    output$mapstatus<-renderText(vals$mapstatus)
    outputOptions(output, "mapstatus", suspendWhenHidden = FALSE)
    output$file_present<-renderText(vals$file_present)
    outputOptions(output, "file_present", suspendWhenHidden = FALSE)
    output$data_present<-renderText(vals$data_present)
    outputOptions(output, "data_present", suspendWhenHidden = FALSE)
    
    clear<-observeEvent(input$Clear,{
      vals$switch<-FALSE
      vals$mapstatus<-FALSE
      vals$filedata<-NULL
      RDF<-rhandsontable(hot_to_r(input$pdfdf)[NULL,],selectCallback=TRUE,readOnly=FALSE)
      output$pdfdf<-renderRHandsontable(RDF)
    },ignoreInit=TRUE)
    tabBox(title = "Permit App",id= "ttabs", width = 12, height = "800px",
           scraper_input2(statenames,vals$filedata$name),
           scraper_output(vals$switch),
           scraper_map()
    )
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
}

shinyApp(ui, server)