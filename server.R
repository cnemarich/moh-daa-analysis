library(shiny)
library(shinyjs)
require(datimvalidation)
require(shinyWidgets)
require(magrittr)
require(ggplot2)
require(rpivotTable)
source("./utils.R")

shinyServer(function(input, output, session) {
  
  ready <- reactiveValues(ok = FALSE)
  
  observeEvent(input$fetch, {
    shinyjs::disable("ou")
    shinyjs::disable("fetch")
    ready$ok <- TRUE
    d <- analysis_data()
  })
  
  observeEvent(input$reset_input, {
    shinyjs::enable("ou")
    shinyjs::enable("fetch")
    shinyjs::disable("downloadHTS")
    shinyjs::disable("downloadPMTCTART")
    shinyjs::disable("downloadPMTCTSTAT")
    shinyjs::disable("downloadTBPREV")
    shinyjs::disable("downloadTXNEW")
    shinyjs::disable("downloadTXCURR")
    shinyjs::disable("downloadRaw")
    shinyjs::disable("reset_input")
    ready$ok <- FALSE
  })
  
  fetch <- function() {
    
    shinyjs::disable("downloadHTS")
    shinyjs::disable("downloadPMTCTART")
    shinyjs::disable("downloadPMTCTSTAT")
    shinyjs::disable("downloadTBPREV")
    shinyjs::disable("downloadTXNEW")
    shinyjs::disable("downloadTXCURR")
    shinyjs::disable("downloadRaw")
    shinyjs::disable("reset_input")

    if (!user_input$authenticated | !ready$ok)  {
      return(NULL)
    } else {
      
      sendSweetAlert(
        session,
        title = "Fetching data",
        text = "Sit tight. I'm getting your data",
        btn_labels= NA
      )
      
      #sites<-analysis_getSitesTable(input$ou)
      indicators<-analysis_getIndicatorsTable(input$ou)
      shinyjs::disable("ou")
      shinyjs::disable("fetch")
      shinyjs::enable("downloadHTS")
      shinyjs::enable("downloadPMTCTART")
      shinyjs::enable("downloadPMTCTSTAT")
      shinyjs::enable("downloadTBPREV")
      shinyjs::enable("downloadTXNEW")
      shinyjs::enable("downloadTXCURR")
      shinyjs::enable("downloadRaw")
      shinyjs::enable("reset_input")
      closeSweetAlert(session)
      my_data<-list(sites=sites,indicators=indicators)
      if (is.null(my_data$sites) & is.null(my_data$indicators)) {
        sendSweetAlert(
          session,
          title = "Oops!",
          text = "Sorry, I could not find any data for you!"
        )
      }
      return(my_data)
    }
    
  }
  
  analysis_data <- reactive({ fetch() })
  
  user_input <- reactiveValues(authenticated = FALSE, 
                               status = "",
                               user_orgunit = NA)
  
  observeEvent(input$login_button, {
    is_logged_in <- FALSE
    user_input$authenticated <- DHISLogin(input$server, input$user_name, input$password)
    if (user_input$authenticated) {
      user_input$user_orgunit<-getOption("organisationUnit")
      flog.info(paste0("User ", input$user_name, " logged in."), name = "datapack")
    } else {
      sendSweetAlert(
        session,
        title = "Login failed",
        text = "Please check your username/password!",
        type = "error")
      flog.info(paste0("User ", input$user_name, " login failed."), name = "datapack")
    }
  })
  
  output$site_table <- DT::renderDataTable({
    
    d <- analysis_data()
    
    if (!inherits(d, "error") & !is.null(d)) {
      
      table_formatted <- d %>%
        purrr::pluck("sites")
      
      DT::datatable(table_formatted,options = list(pageLength = 50, 
                                     columnDefs = list(list(className = 'dt-right', 
                                                            targets = 3:8)))) %>% 
        formatCurrency(3:8, '',digits =0)
      
    } else
    {
      NULL
    }
  })
  
  output$indicator_table <- DT::renderDataTable({
    
    d <- analysis_data()
    
    if (!inherits(d, "error") & !is.null(d)) {
      
      table_formatted <- d %>% 
        purrr::pluck("indicators")
      
      DT::datatable(table_formatted,
                    options = list(pageLength = 50, columnDefs = list(list(
                      className = 'dt-right', targets = 2),
                      list(
                        className = 'dt-right', targets = 3),
                      list(
                        className = 'dt-right', targets = 4),
                      list(
                        className = 'dt-right', targets = 5),
                      list(
                        className = 'dt-right', targets = 6),
                      list(
                        className = 'dt-right', targets = 7),
                      list(
                        className = 'dt-right', targets = 8),
                      list(
                        className = 'dt-right', targets = 9),
                      list(
                        className = 'dt-right', targets = 10),
                      list(
                        className = 'dt-right', targets = 11),
                      list(
                        className = 'dt-right', targets = 12),
                      list(
                        className = 'dt-right', targets = 13),
                      list(
                        className = 'dt-right', targets = 14),
                      list(
                        className = 'dt-right', targets = 15),
                      list(
                        className = 'dt-right', targets = 16),
                      list(
                        className = 'dt-right', targets = 17)
                    )))
      
    } else
    {
      NULL
    }
  })
  
  output$uiLogin <- renderUI({
    wellPanel(fluidRow(
      img(src = 'pepfar.png', align = "center"),
      h4(
        "Welcome to the MoH-DAA Analysis app. Please login with your DATIM credentials:"
      )
    ),
    fluidRow(
      textInput("user_name", "Username: ", width = "600px"),
      passwordInput("password", "Password:", width = "600px"),
      actionButton("login_button", "Log in!")
    ))
  })
  
  output$ui <- renderUI({
    if (user_input$authenticated == FALSE) {
      ##### UI code for login page
      fluidPage(fluidRow(
        column(
          width = 2,
          offset = 5,
          br(),
          br(),
          br(),
          br(),
          uiOutput("uiLogin"),
          uiOutput("pass")
        )
      ))
    } else {
      wiki_url <- a("MoH Data Alignment Support Site",
                    href = "https://datim.zendesk.com/hc/en-us/categories/360000927432-PEPFAR-MoH-Data-Alignment-Activity",
                    target = "_blank")
      
      fluidPage(tags$head(
        tags$style(
          ".shiny-notification {
        position: fixed;
        top: 10%;
        left: 33%;
        right: 33%;}"
        )
      ),
      sidebarLayout(
        sidebarPanel(
          shinyjs::useShinyjs(),
          id = "side-panel",
          tagList(wiki_url),
          tags$hr(),
          selectInput("ou", "Operating Unit",getUserOperatingUnits(user_input$user_orgunit)),
          actionButton("fetch","Get Data"),
          tags$hr(),
          "Download Analysis Workbooks",
          disabled(downloadButton("downloadHTS", "HTS_TST analysis", style = "width:100%;text-align: left;"),
          downloadButton("downloadPMTCTART", "PMTCT_ART analysis", style = "width:100%;text-align: left;"),
          downloadButton("downloadPMTCTSTAT", "PMTCT_STAT analysis", style = "width:100%;text-align: left;"),
          downloadButton("downloadTBPREV", "TB_PREV analysis", style = "width:100%;text-align: left;"),
          downloadButton("downloadTXCURR", "TX_CURR analysis", style = "width:100%;text-align: left;"),
          downloadButton("downloadTXNEW", "TX_NEW analysis", style = "width:100%;text-align: left;"),
          tags$hr(),
          downloadButton("downloadRaw", "Raw results data", style = "width:100%;text-align: left;"),
          tags$hr(),
          actionButton("reset_input","Reset Inputs")),
          width = 2
        ),
        mainPanel(tabsetPanel(
          id = "main-panel",
          type = "tabs",
          tabPanel("Discordance Graph", plotOutput("discordance_graph")),
          tabPanel("Site Alignment Analysis", dataTableOutput("site_table")),
          tabPanel("Indicator Analysis", dataTableOutput("indicator_table")),
          tabPanel("Pivot Table", rpivotTableOutput({"pivot"})),
          tabPanel("Country Comparison", plotOutput("country_comparison"))
        ))
      ))
    }
    
  })
  
  output$downloadHTS <- downloadHandler(
    filename = wb_filename(ou = input$ou, my_indicator = "HTS"),
    content = function(file) {
      
      d <- analysis_data()
      wb <- wb_filecontent(d,"HTS_TST",file)
      return(wb)
      
    })
  
  output$downloadPMTCTART <- downloadHandler(
    filename = wb_filename(ou = input$ou, my_indicator = "PMTCT_ART"),
    content = function(file) {
      
      d <- analysis_data()
      wb <- wb_filecontent(d,"PMTCT_ART",file)
      return(wb)
      
    })
  
  output$downloadPMTCTSTAT <- downloadHandler(
    filename = wb_filename(ou = input$ou, my_indicator = "PMTCT_STAT"),
    content = function(file) {
      
      d <- analysis_data()
      wb <- wb_filecontent(d,"PMTCT_STAT",file)
      return(wb)
      
    })
  
  output$downloadTBPREV <- downloadHandler(
    filename = wb_filename(ou = input$ou, my_indicator = "TB"),
    content = function(file) {
      
      d <- analysis_data()
      wb <- wb_filecontent(d,"TB_PREV",file)
      return(wb)
      
    })
  
  output$downloadTXCURR <- downloadHandler(
    filename = wb_filename(ou = input$ou, my_indicator = "TX_CURR"),
    content = function(file) {
      
      d <- analysis_data()
      wb <- wb_filecontent(d,"TX_CURR",file)
      return(wb)
      
    })
  
  output$downloadTXNEW <- downloadHandler(
    filename = wb_filename(ou = input$ou, my_indicator = "TX_NEW"),
    content = function(file) {
      
      d <- analysis_data()
      wb <- wb_filecontent(d,"TX_NEW",file)
      
    })
  
  output$downloadRaw <- downloadHandler(
    filename = function() {
      
      suffix <- "raw_data"
      date<-format(Sys.time(),"%Y%m%d_%H%M%S")
      name <- paste0(paste(input$ou,suffix,date,sep="_"),".xlsx")
      
      },
    content = function(file) {
      
      d <- analysis_data()
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb,"RawData")
      openxlsx::writeDataTable(wb = wb,
                               sheet = "RawData",x = d$indicators)
      openxlsx::saveWorkbook(wb,file=file,overwrite = TRUE)
      return(wb)
      
    })
  
})
