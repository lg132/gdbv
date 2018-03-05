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
      sliderInput(inputId = "range", 
                  label="Year:",
                  min = 1950, max = 2014,
                  value = c(1950,2014),
                  sep = "",
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
    
    range <- input$range
    df1 <- df_fishing_all
    df2 <- df1 %>% gather(., key="country", value="tonnage", -c(years)) 
    df2.1 <- df2 %>% filter(years>=input$range[1], years<=input$range[2])
    
    df3 <- df2.1 %>% group_by(country) %>% summarise(avg=mean(tonnage))
    
    df4 <- df3 %>% filter(avg<2000000) 
    df5 <- df3 %>% filter(avg>2000000) 
    
    fishing_high <- df2.1 %>% filter(country %in% df5$country) #all countries with more than 2mio catches
    others1 <- df2.1 %>% filter(country %in% df4$country)
    others2 <- others1 %>% group_by(years) %>% summarise(tonnage = sum(tonnage))
    others3 <- others2 %>% mutate(country = "Others") %>% select(years,country,tonnage)
    
    fish.final <- bind_rows(fishing_high,others3)
    
    ggplot(fish.final, aes(x= years, y=tonnage)) +
      geom_area(aes(fill=factor(country, levels=c(df5$country, "Others"))))+  
      #levels: to have "others" last (default of ggplot would be alphabetical order)
      theme(legend.position = "right") + #add legend on the right 
      guides(fill=guide_legend(title="Countries")) + #change the legend title 
      scale_fill_hue (l=30) #just darken/lighten the colours 
  
  })
    
  
}

# Create Shiny app ----
shinyApp(ui, server)
