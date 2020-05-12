library(shiny)
library(shinyjs)
require(datimvalidation)
require(shinyWidgets)
require(magrittr)
require(purrr)
require(ggplot2)
require(gt)
require(rpivotTable)
source("./utils.R")
source("./visuals.R")

shinyServer(function(input, output, session) {

  ready <- reactiveValues(ok = FALSE)

  observeEvent(input$fetch, {
    shinyjs::disable("pe")
    shinyjs::disable("ou")
    shinyjs::disable("fetch")
    ready$ok <- TRUE
    d <- analysis_data()
  })

  observeEvent(input$reset_input, {
    shinyjs::enable("pe")
    shinyjs::enable("ou")
    shinyjs::enable("fetch")
    shinyjs::disable("downloadInput")
    shinyjs::disable("download_wb")
    shinyjs::disable("download_raw")
    shinyjs::disable("reset_input")
    ready$ok <- FALSE
  })

  download_filter <- reactiveValues(wb_filter = NULL)

  observeEvent(input$downloadInput, {

    download_filter$wb_filter <- input$downloadInput

  })

  discordance_filter <- reactiveValues(disc_indicator_filter = NULL)

  observeEvent(input$discordanceInput, {

    discordance_filter$disc_indicator_filter <- input$discordanceInput

  })

  site_filter <- reactiveValues(site_indicator_filter = NULL,
                                site_period_filter = NULL)

  observeEvent(input$indicatorInput, {

    site_filter$site_indicator_filter <- input$indicatorInput

    })

  observeEvent(input$periodInput, {

    site_filter$site_period_filter <- input$periodInput

  })

  fetch <- function() {

    shinyjs::disable("downloadInput")
    shinyjs::disable("download_wb")
    shinyjs::disable("download_raw")
    shinyjs::disable("reset_input")

    if (!user_input$datim_authenticated |
        !user_input$geoalign_authenticated |
        !ready$ok)  {
      return(NULL)
    } else {

      shinyjs::disable("pe")
      shinyjs::disable("ou")
      shinyjs::disable("fetch")

      withProgress(message = "Fetching data", value = 0, {

      incProgress(0.25, detail = ("Fetching indicator data"))
      indicators <- get_indicators_table(input$ou)
      Sys.sleep(0.5)

      incProgress(0.25, detail = ("Fetching EMR data"))
      emr <- get_emr_table(input$ou)
      Sys.sleep(0.5)

      incProgress(0.25, detail = ("Merging datasets"))
      analytics <- combine_data(indicators, emr)
      Sys.sleep(0.5)

      incProgress(0.25, detail = ("Generating final dataset"))
      my_data <- list(indicators = indicators, emr = emr, analytics = analytics)
      Sys.sleep(0.5)

      })

      if (is.null(my_data$emr) & is.null(my_data$indicators)) {
        sendSweetAlert(
          session,
          title = "Oops!",
          text = "Sorry, I could not find any data for you!"
        )

        shinyjs::enable("reset_input")

        ready$ok <- FALSE

        return(NULL)

      } else {

        shinyjs::enable("downloadInput")
        shinyjs::enable("download_wb")
        shinyjs::enable("download_raw")
        shinyjs::enable("reset_input")

        return(my_data)
      }
    }
  }

  analysis_data <- reactive({
    fetch()
    })

  user_input <- reactiveValues(datim_authenticated = FALSE,
                               geoalign_authenticated = FALSE,
                               status = "",
                               datim_user_orgunit = NA,
                               geoalign_user_orgunit = NA)

  observeEvent(input$datim_login_button, {
    is_logged_in <- FALSE
    user_input$datim_authenticated <- dhis_login(input$server,
                                          input$datim_user_name,
                                          input$datim_password)
    if (user_input$datim_authenticated) {
      user_input$datim_user_orgunit <- getOption("organisationUnit")
      flog.info(paste0("User ", input$datim_user_name, " logged in."),
                name = "datapack")
    } else {
      sendSweetAlert(
        session,
        title = "Login failed",
        text = "Please check your username/password!",
        type = "error")
      flog.info(paste0("User ", input$datim_user_name, " login failed."),
                name = "datapack")
    }
  })

  output$ui_datim_login <- renderUI({
    wellPanel(fluidRow(
      HTML('<center><img src="pepfar.png"></center>'),
      h3("Welcome to the PEPFAR-MoH Data Alignment Activity Analysis app.",
         align = "center"),
      h4("Please login with your DATIM credentials:", align = "center")
    ),
    fluidRow(
      textInput("datim_user_name", "Username: ", width = "600px"),
      passwordInput("datim_password", "Password:", width = "600px"),
      actionButton("datim_login_button", "Log in!")
    ))
  })

  observeEvent(input$geoalign_login_button, {
    is_logged_in <- FALSE
    user_input$geoalign_authenticated <- geoalign_login(input$server,
                                                 input$geoalign_user_name,
                                                 input$geoalign_password)
    if (user_input$geoalign_authenticated) {
      user_input$geoalign_user_orgunit <- getOption("organisationUnit")
      flog.info(paste0("User ", input$geoalign_user_name, " logged in."),
                name = "datapack")
    } else {
      sendSweetAlert(
        session,
        title = "Login failed",
        text = "Please check your username/password!",
        type = "error")
      flog.info(paste0("User ", input$geoalign_user_name, " login failed."),
                name = "datapack")
    }
  })

  output$ui_geoalign_login <- renderUI({
    wellPanel(fluidRow(
      HTML('<center><img src="pepfar.png"></center>'),
      h4(
        "Thank you. Now please login with your GeoAlign credentials:"
      )
    ),
    fluidRow(
      textInput("geoalign_user_name", "Username: ", width = "600px"),
      passwordInput("geoalign_password", "Password:", width = "600px"),
      actionButton("geoalign_login_button", "Log in!")
    ))
  })

  output$ui <- renderUI({

    if (user_input$datim_authenticated == FALSE) {
      ##### UI code for DATIM login page
      fluidPage(fluidRow(
        column(
          width = 2,
          offset = 5,
          br(),
          br(),
          br(),
          br(),
          uiOutput("ui_datim_login"),
          uiOutput("pass")
        )
      ))
    } else if (user_input$geoalign_authenticated == FALSE) {
      ##### UI code for GeoAlign login page
      fluidPage(fluidRow(
        column(
          width = 2,
          offset = 5,
          br(),
          br(),
          br(),
          br(),
          uiOutput("ui_geoalign_login"),
          uiOutput("pass")
        )
      ))
    } else {
      wiki_url <- a("Data Alignment Support Site",
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
          selectInput("ou", "Operating Unit",
                      get_user_operating_units(user_input$datim_user_orgunit)),
          actionButton("fetch", "Get Data"),
          tags$hr(),
          "Download Analysis Workbooks",
          disabled(selectInput("downloadInput", "Choose a dataset:",
                               choices = c("HTS_TST", "PMTCT_STAT", "PMTCT_ART",
                                           "TB_PREV", "TX_CURR", "TX_NEW")),
                   downloadButton("download_wb", "Download",
                                  style = "width:100%;text-align: left;"),
                   tags$hr(),
                   downloadButton("download_raw", "Raw data",
                                  style = "width:100%;text-align: left;"),
                   tags$hr(),
                   actionButton("reset_input", "Reset Inputs")),
          width = 2
          ),
        mainPanel(tabsetPanel(
          id = "main-panel",
          type = "tabs",
          tabPanel(title = "Discordance Graph",
                   pickerInput("discordanceInput", "Indicator",
                               choices = c("HTS_TST", "PMTCT_STAT", "PMTCT_ART",
                                           "TB_PREV", "TX_CURR", "TX_NEW"),
                               selected = c("HTS_TST", "PMTCT_STAT",
                                            "PMTCT_ART", "TB_PREV",
                                            "TX_CURR", "TX_NEW"),
                               options = list(`actions-box` = TRUE),
                               multiple = T),
                   plotOutput("discordance_graph")),
          tabPanel(title = "Site Alignment Analysis",
                   wellPanel(
                     fluidRow(
                       column(6,
                              pickerInput("indicatorInput", "Indicator",
                                          choices = c("HTS_TST", "PMTCT_STAT",
                                                      "PMTCT_ART", "TB_PREV",
                                                      "TX_CURR", "TX_NEW"),
                                          selected = "HTS_TST",
                                          options = list(`actions-box` = TRUE),
                                          multiple = T)),
                       column(6,
                              pickerInput("periodInput", "Period",
                                          choices = c("FY2019", "FY2018"),
                                          selected = "FY2019",
                                          options = list(`actions-box` = TRUE),
                                          multiple = T))
                       )),
                   hr(),
                   dataTableOutput("site_table")),
          tabPanel(title = "Indicator Analysis", gt_output("indicator_table")),
          tabPanel(title = "Pivot Table",
                   rpivotTableOutput({
                     "pivot"
                     })),
          tabPanel(title = "Country Comparison",
                   plotOutput("country_comparison"))
        ))
      ))
    }
  })

  output$discordance_graph <- renderPlot({

    d <- analysis_data()

    if (!inherits(d, "error") & !is.null(d)) {

      discordance_chart_data <- d %>%
        purrr::pluck(., "analytics") %>%
        dplyr::filter(indicator %in%
                        discordance_filter$disc_indicator_filter) %>%
        indicator_table_data()

      discordance_chart(discordance_chart_data)

    } else {
      NULL
    }
  })

  output$site_table <- DT::renderDataTable({

    d <- analysis_data()

    if (!inherits(d, "error") & !is.null(d)) {

      table_formatted <- d %>%
        purrr::pluck(., "analytics") %>%
        dplyr::filter(indicator %in% site_filter$site_indicator_filter,
                      period %in% site_filter$site_period_filter) %>%
        site_table_data()

      DT::datatable(table_formatted, options = list(pageLength = 50)) %>%
        DT::formatPercentage("Weighted difference")

    } else {
      NULL
    }
  })


  output$indicator_table <- render_gt(

    expr = if (ready$ok) {

      analysis_data() %>%
        purrr::pluck(., "analytics") %>%
        indicator_table_data() %>%
        gt() %>%
        fmt_number(columns = vars(MOH, PEPFAR, Difference), decimals = 0) %>%
        fmt_percent(columns = vars(`Weighted difference`), decimals = 2)

    } else {
      NULL
      },
    height = px(700),
    width = "70%"

  )

  output$pivot <- renderRpivotTable({

    d <- analysis_data()

    if (!is.null(d)) {

      if (is.null(d$analytics)) {
        return(NULL)
        }
      moh_pivot(d)

    } else {
      NULL
      }
  })

  output$download_wb <- downloadHandler(
    filename = function() {

      ou_name <- get_ou_name(input$ou)

      name <- wb_filename(ou = ou_name,
                          my_indicator = download_filter$wb_filter)

      },
    content = function(file) {

      d <- analysis_data()
      wb <- wb_filecontent(d, download_filter$wb_filter, file)
      return(wb)

    })

  output$download_raw <- downloadHandler(
    filename = function() {

      ou_name <- get_ou_name(input$ou)
      name <- raw_filename(ou = ou_name)

      },
    content = function(file) {

      d <- analysis_data()
      wb <- raw_filecontent(d, file)
      return(wb)

      })

})
