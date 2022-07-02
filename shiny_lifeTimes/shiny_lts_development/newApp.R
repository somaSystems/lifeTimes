# https://community.rstudio.com/t/using-column-names-in-csv-file-in-drop-down-box-and-use-data-in-analysis/46569/3

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

  #data1 looks like full dataframe, input file
  #df is read.csv datapath
  #df2 is the head of dataframe
  data1 <- reactive({
    validate(need(input$file1,""))
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath,na.strings = c("", "NA", "#N/A"))
    df2 <- head(df, 8)
    # df2 <- data.frame(df)
    # return(df2)
    return(df2)
  })

  #data2 is just the colnames of dataframe, except for last column
  #update each in reactive brace
  data2 <- reactive({
    df3 <- data1()[,-1]
    # df3 <- data1()
    updateSelectInput(session,"lts_time",choices=colnames(df3))
    updateSelectInput(session,"lts_categorical",choices=colnames(df3))
    updateSelectInput(session,"lts_uniqueID",choices=colnames(df3))
    # updateSelectInput(session,"lts_pairedComparisons",choices=colnames(df3))
    # updateSelectInput(session,"lts_uniqueID",choices=colnames(df3))
    return(df3)
  })

  # data3 <- reactive({
  #   df3 <- data1()[,-1]
  #   updateSelectInput(session,"lts_measVar",choices=colnames(df3))
  #   return(df3)
  # })

  # lts_run <- reactive({
  #   lts_in(.in_tsData = data1(),
  #                   .in_time = input$lts_time,
  #                   .in_compare_categorical = input$lts_categorical,
  #                   .in_plot_measured_variables = FALSE,
  #                   .in_pairedComparisons = c("season", "catchmentRegion"),
  #                   .in_uniqueID_colname = input$lts_uniqueID,
  #                   .in_metaData = NULL)
  # })
  #
  # output$time <- renderPlot({
  #  lts_plot_ccfs(lts_run())
  # })

  # https://shiny.rstudio.com/reference/shiny/1.6.0/renderPrint.html
  # # renderPrint captures any print output, converts it to a string, and
  # # returns it
  # visFun <- renderPrint({ "foo" })
  # visFun()
  # # '[1] "foo"'

#pass input to a function
  output$time <- renderPlot({
    ggplot(data=data2(), aes_string(x=input$lts_categorical,y=input$lts_time)) + geom_point(color="darkblue")
  })

} #END server function
shinyApp(ui = ui, server = server)
