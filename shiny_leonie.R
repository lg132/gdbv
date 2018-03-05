library(shiny)
library(ggplot2)
library(tidyverse)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("seaaroundus"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for range of years to be displayed ----
      sliderInput(inputId = "range",
                  label = "Range:",
                  min = 1950,
                  max = 2014,
                  value = c(1950, 2014)),
      
      # Input: Select-option for data.frame
      selectInput(inputId = "df",
                  label = "select data.frame",
                  choices = c("fish.final", "df_Q2p"))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Table with coloumn-range ----
      #verbatimTextOutput("Range"),
      
      # Output: Plot ----
      plotOutput(outputId = "areaPlot")
    )
  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  
  dataInput <- reactive({
    data <- switch(input$df,
                   "fish.final" = df_Q2p,
                   "df_Q2p" = df_Q2p)
    range <- input$range
 
    return(list("data"=data, "range"=range))
  })
  
  output$areaPlot <- renderPlot({
    
    data <- dataInput()$data
    range <- dataInput()$range
    as.numeric(range)
    data <- data %>% filter(years>=range[1], years<=range[2])
    
    ifelse(colnames(data)[2]=="catchtype",
           ggplot(data=data, aes(x=years, y=percentage))+
             geom_area(aes(fill=catchtype))+
             scale_fill_brewer(palette = "Dark2"),
           )
    
    
  })
}


# Create Shiny app ----
shinyApp(ui = ui, server = server)
