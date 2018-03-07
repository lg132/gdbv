library(shiny)

ui <- fluidPage(
  
  titlePanel("Hallöchen."),
  
  # put the side bar on the right
  sidebarLayout(position = "right",
                
               sidebarPanel(
                  sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30)
                ),
                
                mainPanel(
                  plotOutput("distPlot")
                )
  )
)

server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    x    <- faithful[, 2]  # Old Faithful Geyser data
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)


