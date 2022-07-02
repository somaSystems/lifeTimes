
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

# ui_selectInput_lts_pairedComparisons <- selectInput("lts_pairedComparisons", "select \"lts_pairedComparisons\" variable:", choices=c())

ui_selectInput_lts_uniqueID <- selectInput("lts_uniqueID", "select \"lts_uniqueID\" variable:", choices=c())

submitAction <- actionButton("go", "Calculate cross correlations")


ui_mainPanel <- mainPanel(
  # Output: Verbatim text for data summary ----
  textOutput("dims"),
  # verbatimTextOutput("structure"),
  plotOutput("lts_plot_ccfs")
  )
# tableOutput("TBL")

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
  # #Get structure of dataframe
  # output$structure <- renderPrint({
  #   # dataset <- datasetInput()
  #   str(InputData_lts())
  # })


#get just the top of dataframe for lateruse in colnames selection
#do this once
#returns head of data

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

###Toggle one #####
###set up reactive plotting of ccfs
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

output$lts_plot_ccfs <- renderPlot({lts_plot_ccfs(lts_result() )
  } )
####Toggle one end #####

###Toggle TWO###
# output$lts_plot_ccfs <- renderPlot({lts_plot_ccfs(lts_in() )
# } )
###Toggle TWO END###
}


shinyApp(ui = ui, server = server)


# data_head_lts <- reactive({
#   df <- InputData_lts()
#   # validate(need(input$file1,""))
#   # inFile <- input$file1
#   # if (is.null(inFile))
#   #   return(NULL)
#   # df <- read.csv(inFile$datapath,na.strings = c("", "NA", "#N/A"))
#   df2 <- head(df, 5)
#   # df2 <- data.frame(df)
#   # return(df2)
#   return(df2)
# })


# #take only colnames from dataframe and use for select input
# #data2 is just the colnames of dataframe, except for last column
# #update each in reactive brace
# data_colnames_lts <- reactive({
#   # df3 <- data1()[,-1]
#   df3 <- data_head_lts()
#   updateSelectInput(session,"lts_time",choices=c(colnames(df3),"mock"))
#   updateSelectInput(session,"lts_categorical",choices=colnames(df3))
#   updateSelectInput(session,"lts_uniqueID",choices=colnames(df3))
#   # updateSelectInput(session,"lts_pairedComparisons",choices=colnames(df3))
#   # updateSelectInput(session,"lts_uniqueID",choices=colnames(df3))
#   return(df3)
# })


#select column names from input



