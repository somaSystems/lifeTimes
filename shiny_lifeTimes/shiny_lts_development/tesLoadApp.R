# https://community.rstudio.com/t/using-column-names-in-csv-file-in-drop-down-box-and-use-data-in-analysis/46569/3

library(shiny)
library(ggplot2)
ui <- fluidPage(
  titlePanel("Analysis of Returns Data"),
  fileInput("file1", "Choose CSV file", multiple = FALSE,
            accept = c("text/csv","text/comma-separated-values,text/plain",".csv"),
            width = NULL, buttonLabel = "Browse...",
            placeholder = "No file selected"),
  selectInput("series", "Choose a stock ticker:", choices=c()),

  mainPanel(
    plotOutput("time",width="63%",height="275px")
    # tableOutput("TBL")
  )
)

#################################################################################
#
# Server function
#
server <- function(session,input,output) {

  data1 <- reactive({
    validate(need(input$file1,""))
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath,na.strings = c("", "NA", "#N/A"))
    df2 <- head(df, 8)
    return(df2)
  })

  data2 <- reactive({
    df3 <- data1()[,-1]
    updateSelectInput(session,"series",choices=colnames(df3))
    return(df3)
  })

  output$time <- renderPlot({
    ggplot(data=data2(), aes_string(x="Country",y=input$series)) + geom_point(color="darkblue")
  })
}
shinyApp(ui = ui, server = server)
