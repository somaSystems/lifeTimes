
#ui fluidPage
library(shiny)
library(lifeTimes)
library(ggplot2)

#lifeTimes logo
ui_Title <-   titlePanel(
  img(src='lifeTimesLogo.png', align = "right", height="20%", width="20%"))

#csv fileinput c
# colnames can be chosen from csv
ui_fileInput <- fileInput("file1", "Choose CSV file", multiple = FALSE,
                          accept = c("text/csv","text/comma-separated-values,text/plain",".csv"),
                          width = NULL, buttonLabel = "Browse...",
                          placeholder = "No file selected")

#call this inputID, series
#input choices come from fileInput file1
ui_selectInput_lts_time <- selectInput("lts_time", "select \"Time\" variable:", choices=c())

ui_selectInput_lts_categorical <- selectInput("lts_categorical", "select \"lts_categorical\" variable:", choices=c())

# ui_selectInput_lts_pairedComparisons <- selectInput("lts_pairedComparisons", "select \"lts_pairedComparisons\" variable:", choices=c())

ui_selectInput_lts_uniqueID <- selectInput("lts_uniqueID", "select \"lts_uniqueID\" variable:", choices=c())


ui_mainPanel <- mainPanel(
  # Output: Verbatim text for data summary ----
  textOutput("dims"),
  verbatimTextOutput("summary"),
  # tableOutput("structure"),
  plotOutput("time",width="63%",height="275px"))
# tableOutput("TBL")

ui <- fluidPage(
  ui_Title,
  ui_fileInput,
  ui_selectInput_lts_time,
  ui_selectInput_lts_categorical,
  # ui_selectInput_lts_pairedComparisons,
  ui_selectInput_lts_uniqueID,
  ui_mainPanel
)


#################################################################################
#
# Server function
#
server <- function(session,input,output) {
  
  #get dataframe from input
  InputData_lts <- reactive({
    validate(need(input$file1,""))
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    df_load <- read.csv(inFile$datapath,na.strings = c("", "NA", "#N/A"))
    # df2 <- head(df, 8)
    df_lts <- data.frame(df_load)
    # return(df2)
    return(df_lts)
  })
 
  #Get dataframe 
  output$dims <- renderText({ 
   c("data dimensions are:",dim(InputData_lts())[[1]], "rows, by ", dim(InputData_lts())[[2]], "columns")
           })
  
  output$summary <- renderPrint({
    # dataset <- datasetInput()
    str(InputData_lts())
  })
}

shinyApp(ui = ui, server = server)


