library(shiny)

df_fishing_all <- source("df_fishing_all.Rdmpd")
df_fishing_all <- df_fishing_all[[1]]

# Define UI for slider demo app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Sliders"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      
      # Input: Specification of range within an interval ----
      sliderInput("range", "Year:",
                  min = 1950, max = 2014,
                  value = c(1950,2014),
                  step=1)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Table summarizing the values entered ----
      tabsetPanel(type="tabs",
                  tabPanel("values", tableOutput("values")),
                  tabPanel("Graph", plotOutput(outputId = "graph"))
                  
      
    ))
  )
)

# Define server logic for slider examples ----
server <- function(input, output) {
  
  # Reactive expression to create data frame of all input values ----
  sliderValues <- reactive({
    
    data.frame(
      Name = c("Range"),
      Value = as.character(c(paste(input$range, collapse = " ")
                             )),
      stringsAsFactors = FALSE)
    
  })
  
  # Show the values in an HTML table ----
  output$values <- renderTable({
    sliderValues()
  }) 
    
  output$graph <- renderPlot({
    x <- df_fishing_all$years
    y <- df_fishing_all$albania
    plot(x,y,type="l", xlab="Year",ylab="tonnage",main="Albania")
    
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
