library(shiny)
library(lifeTimes)
# Define UI ----
ui <- fluidPage(
  titlePanel("censusVis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create demographic maps with 
               information from the 2010 US Census."),
      
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = c("Percent White", 
                              "Percent Black",
                              "Percent Hispanic", 
                              "Percent Asian"),
                  selected = "Percent White"),
      
      sliderInput("range", 
                  label = "Range of interest:",
                  min = 0, max = 100, value = c(0, 100))
    ),
    
    mainPanel(
      textOutput("selected_var"),
      plotOutput("selectedPlot")
    )
  )
)

#########server logic

server <- function(input, output) {
  
  lts <- lts_in()
  
  # output$selected_var <- renderText({ 
  #  
  # })
  
  output$selectedPlot <- renderPlot({
   # lts_plot_ccfs(lts_in())
    lts_plot_ccfs(lts)
 })
}
shinyApp(ui = ui, server = server)