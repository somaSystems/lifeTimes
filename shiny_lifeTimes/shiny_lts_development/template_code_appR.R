#cars test data upload app:

library(shiny)

ui <- fluidPage(
  h2('The uploaded file data'),
  dataTableOutput('mytable'),
  fileInput('file', 'Choose info-file to upload',
            accept = c(
              'text/csv',
              'text/comma-separated-values',
              'text/tab-separated-values',
              'text/plain',
              '.csv',
              '.tsv'
            )
  ),
  # Taken from: http://shiny.rstudio.com/gallery/file-upload.html
  tags$hr(),
  checkboxInput('header', 'Header', TRUE),
  radioButtons('sep', 'Separator',
               c(Comma=',',
                 Semicolon=';',
                 Tab='\t'),
               ','),
  radioButtons('quote', 'Quote',
               c(None='',
                 'Double Quote'='"',
                 'Single Quote'="'"),
               '"'),
  ################################################################

  actionButton("choice", "incorporate external information"),

  selectInput("columns", "Select Columns", choices = NULL), # no choices before uploading

  tableOutput("table_display")
)

server <- function(input, output, session) { # added session for updateSelectInput

  info <- eventReactive(input$choice, {
    inFile <- input$file
    # Instead # if (is.null(inFile)) ... use "req"
    req(inFile)

    # Changes in read.table
    f <- read.table(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
    vars <- names(f)
    # Update select input immediately after clicking on the action button.
    updateSelectInput(session, "columns","Select Columns", choices = vars)

    f
  })

  output$table_display <- renderTable({
    f <- info()
    f <- subset(f, select = input$columns) #subsetting takes place here
    head(f)
  })
}
shinyApp(ui, server)
