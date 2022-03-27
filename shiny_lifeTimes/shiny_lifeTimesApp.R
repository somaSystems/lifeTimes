
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

ui_selectInput_lts_cat1 <- selectInput("lts_cat1", "select \"First categorical\" variable:", choices=c())

ui_selectInput_lts_cat2 <- selectInput("lts_cat2", "select \"Second categorical\" variable:", choices=c())

ui_selectInput_lts_uniqueID <- selectInput("lts_uniqueID", "select \"lts_uniqueID\" variable:", choices=c())

submitAction <- actionButton("go", "Calculate cross correlations")

ui_mainPanel <- mainPanel(
  textOutput("dims"),
  plotOutput("lts_plot_ccfs")
  )

ui <- fluidPage(
  ui_Title,
  ui_fileInput,
  ui_selectInput_lts_time,
  ui_selectInput_lts_cat1,
  ui_selectInput_lts_cat2,
  # ui_selectInput_lts_pairedComparisons,
  ui_selectInput_lts_uniqueID,
  submitAction,
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

observe({
  x <- colnames(InputData_lts())
  
  if (is.null(x))
    x <- character(0)
  
                  updateSelectInput(session, "lts_time",
                                    label = paste("Select 'time' input column", length(x)),
                                    choices = x,
                                    selected = tail(x, 1))
                  
                  updateSelectInput(session, "lts_cat1",
                                    label = paste("Select 'first categorical' input column", length(x)),
                                    choices = x,
                                    selected = tail(x, 1))
                  
                  updateSelectInput(session, "lts_cat2",
                                    label = paste("Select 'second categorical' input column", length(x)),
                                    choices = x,
                                    selected = tail(x, 1))
  
                  updateSelectInput(session, "lts_pairedComparisons",
                                    label = paste("Select 'pairedComparisons' input column", length(x)),
                                    choices = x,
                                    selected = tail(x, 1))
                  
                  updateSelectInput(session, "lts_uniqueID",
                                    label = paste("Select 'uniqueID' input column", length(x)),
                                    choices = x,
                                    selected = tail(x, 1))
                  
                  
})

#Reactive calculation of ccfs following button press
lts_result <- eventReactive(input$go, {
  lts_in(.in_tsData = InputData_lts() ,
         .in_time  = input$lts_time,
         .in_compare_categorical = c(input$lts_cat1, input$lts_cat2),
         .in_plot_measured_variables = FALSE,
         .in_pairedComparisons = list(pair_1 = list(y = "rainfall_cm", 
                                                    x = "flow_m3s")),
         .in_uniqueID_colname = input$lts_uniqueID ,
         .in_metaData = NULL)
  })

#plot output
output$lts_plot_ccfs <- renderPlot({lts_plot_ccfs(lts_result() )
  } )

}


shinyApp(ui = ui, server = server)



