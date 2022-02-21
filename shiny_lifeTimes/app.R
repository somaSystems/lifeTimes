library(shiny)
library(lifeTimes)
library(janitor)


ui_upload <- sidebarLayout(

  sidebarPanel(
    fileInput("file", "Data", buttonLabel = "Upload..."),
    textInput("delim", "Delimiter (leave blank to guess)", ""),
    numericInput("skip", "Rows to skip", 0, min = 0),
    numericInput("rows", "Rows to preview", 10, min = 1)
  ),

mainPanel(
    h3("Raw data"),
    tableOutput("preview1")
  ),

)

ui_DatsetSelect <- sidebarLayout(

  sidebarPanel(
  selectInput("file1", label = "Dataset", choices = ls("package:lifeTimes")),
  verbatimTextOutput("summary"),
  tableOutput("table")

  ),

  mainPanel(
    h3("myselectinputUI"),
    uiOutput("myselectinputUI")
  ),

)




ui_clean <- sidebarLayout(
  sidebarPanel(
    checkboxInput("snake", "Rename columns to snake case?"),
    checkboxInput("constant", "Remove constant columns?"),
    checkboxInput("empty", "Remove empty cols?")
  ),
  mainPanel(
    h3("Cleaner data"),
    tableOutput("preview2")
  )
)

ui_download <- fluidRow(
  column(width = 12, downloadButton("download", class = "btn-block"))
)


ui <- fluidPage(
  titlePanel(
    img(src='lifeTimesLogo.png', align = "right", height="20%", width="20%")),
  ui_upload,
  ui_DatsetSelect,
  ui_clean,
  ui_download,


    selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
    verbatimTextOutput("summary"),
    tableOutput("table")


)





server <- function(input, output, session) {
  # Upload ---------------------------------------------------------
  raw <- reactive({
    req(input$file)
    delim <- if (input$delim == "") NULL else input$delim
    vroom::vroom(input$file$datapath, delim = delim, skip = input$skip)
  })
  output$preview1 <- renderTable(head(raw(), input$rows))


  #selectFromInput ---------------------------------
  output$myselectinputUI <- renderUI({
    list_label_value <- input$file1

    # read my link with an other answer of mine below if you need many columns in the select input

    setNames(list_label_value$value,list_label_value$label)

    selectizeInput(inputId="myselectinput",
                   label="Report Choice",
                   choices = list_label_value,
                   width = '500px',
                   selected = "1"
                   #  , options = list(render = I(''))
    )
  })



  # Clean ----------------------------------------------------------
  tidied <- reactive({
    out <- raw()
    if (input$snake) {
      names(out) <- janitor::make_clean_names(names(out))
    }
    if (input$empty) {
      out <- janitor::remove_empty(out, "cols")
    }
    if (input$constant) {
      out <- janitor::remove_constant(out)
    }

    out
  })
  output$preview2 <- renderTable(head(tidied(), input$rows))

  # Download -------------------------------------------------------
  output$download <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(input$file$name), ".tsv")
    },
    content = function(file) {
      vroom::vroom_write(tidied(), file)
    }
  )

  # Dataset example --------------------
  output$summary <- renderPrint({
    dataset <- get(input$dataset, "package:datasets")
    summary(dataset)
  })

  output$table <- renderTable({
    dataset <- get(input$dataset, "package:datasets")
    dataset
  })
}
shinyApp(ui = ui, server = server)
