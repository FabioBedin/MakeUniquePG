#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  options(shiny.maxRequestSize=10000*1024^2)

  PG <- eventReactive(input$LoadFromFile, {
    read.delim(
      file = input$LoadFromFile$datapath,
      header = TRUE,
      sep = "\t",
      check.names = FALSE
    )

    # vroom::vroom(file = input$LoadFromFile$datapath,
    #       delim = "\t",
    #       show_col_types = F)
  })

  output$rows <- bs4Dash::renderbs4ValueBox({

    if(is.null(input$LoadFromFile)){
      value <- 0
    }else{
      req(input$LoadFromFile)
      value <- nrow(PG())
    }
    bs4Dash::bs4InfoBox(
      title = "N° of Rows",
      value = value,
      icon = icon("list", lib = "font-awesome"),
      color = "warning",
      fill = F,
    )
  })

  output$duplicate <- bs4Dash::renderbs4ValueBox({

    if(is.null(input$LoadFromFile)){
      dupe <- 0
    }else{
      req(input$LoadFromFile)
      dupe <- ListDupes(PG()) %>% nrow()
    }
    bs4Dash::bs4InfoBox(
      title = "Duplicate Gene names",
      color = "warning",
      value = dupe,
      icon = icon("clone", lib = "font-awesome"),
      fill = TRUE
    )
  })

  output$empty <- bs4Dash::renderbs4ValueBox({
    if(is.null(input$LoadFromFile)){
      empty <- 0
    }else{
      req(input$LoadFromFile)
      empty <- EmptyNames(PG())
    }
    bs4Dash::bs4InfoBox(
      title = "Empty Gene names",
      value = empty,
      icon = icon("eye-slash", lib = "font-awesome"),
      color = "warning",
      fill = F
    )
  })

  output$before <- DT::renderDataTable({
    req(input$LoadFromFile)
    BeforeTable(PG())
  })

  output$after <- DT::renderDataTable({
    req(input$LoadFromFile)
    AfterTable(PG())
  })

  output$select_dupe <- renderUI({
    selectInput(inputId = "selectDupe", label = NULL, choices = ListDupes(PG()))
  })


  output$duplicatePlot <- echarts4r::renderEcharts4r({
    req(input$LoadFromFile)
    req(input$selectDupe)

    eChartFunction(PG(), gene = input$selectDupe)
  })


  output$DownloadUpdatePG <- downloadHandler(
    filename = function() {
      paste("PG_Unique", ".txt", sep="")
    },
    content = function(file) {
      req(input$LoadFromFile)
      PG_Unique <- MakeUniqueGeneNameinPG(PG())
      write.table(PG_Unique, file, quote = FALSE, sep = "\t", col.names = TRUE, na = "", row.names = FALSE)
    })

  observe({
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Welcome!",
      text = tags$div(h4("This app remove duplicates in your PG table. Load your data:", style = "color: #adb5bd"),
                      div(style= "display: flex; justify-content: center;",
                          fileInput("LoadFromFile", "", accept = ".txt", placeholder = "proteinGroups.txt"))
      ),

      type = "info",
      btn_labels = "Ok",
      btn_colors = "#ffc107",
      html = TRUE,
      closeOnClickOutside = TRUE,
      showCloseButton = FALSE,
      width = NULL
    )
  })
}


