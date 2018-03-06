library(shiny)

#read in data: ===============================================
df_fishing_all <- source("df_fishing_all.Rdmpd")
df_fishing_all <- df_fishing_all[[1]]

# Define UI for slider demo app ===============================
ui <- fluidPage(
  
  # App title ----
  titlePanel("Sliders"),
  
  # Sidebar layout with input and output definitions -------
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options --------
    sidebarPanel(
      
      # Input: Specification of range within an interval ----
      sliderInput(inputId = "range", 
                  label="Year:",
                  min = 1950, max = 2014,
                  value = c(1950,2014),
                  sep = "",
                  step=1)
      
    ),
    
    # Main panel for displaying outputs ==========================
    mainPanel(
      
      # Output: The two tab panels:-----------------
      tabsetPanel(type="tabs",
                  tabPanel("Graph", plotOutput(outputId = "graph")),
                  tabPanel("values", tableOutput("values"))        
                  
      
    ))
  )
)

# Define server logic for slider examples ==========================
server <- function(input, output) {
  
  # Reactive expression to create data frame of all input values ----
  sliderValues <- reactive({
    
    data.frame(
      Name = c("Range"),
      Value = as.character(c(paste(input$range, collapse = " ")
                             )),
      stringsAsFactors = FALSE)
    
  })
  
  # Reactive expression for Question1: ----------------------------
  # Create data frame (df3) for the ggplot and the ordered list----
  
  df_function <- reactive({
    
    df2 <- df_fishing_all %>% 
      gather(., key="country", value="tonnage", -c(years)) %>% 
      filter(years>=input$range[1], years<=input$range[2])
    df3 <- df2 %>% group_by(country) %>% summarise(avg=mean(tonnage))
      
    return(list("df2"=df2,"df3"=df3))
  })
  

  #OUTPUT1: stacked ggplot---------------------------------------------
  output$graph <- renderPlot({
    
    #preparing data frames: ----------
    df3 <- df_function()$df3
    df4 <- df3 %>% filter(avg<2000000) 
    df5 <- df3 %>% filter(avg>2000000) 
    
    fishing_high <- df_function()$df2 %>% filter(country %in% df5$country) #all countries with more than 2mio catches
    others1 <- df_function()$df2 %>% filter(country %in% df4$country)
    others2 <- others1 %>% group_by(years) %>% summarise(tonnage = sum(tonnage))
    others3 <- others2 %>% mutate(country = "Others") %>% select(years,country,tonnage)
    
    fish.final <- bind_rows(fishing_high,others3)
    
    #plotting: ----------------------
    ggplot(fish.final, aes(x= years, y=tonnage)) +
      geom_area(aes(fill=factor(country, levels=c(df5$country, "Others"))))+  
      #levels: to have "others" last (default of ggplot would be alphabetical order)
      theme(legend.position = "right") + #add legend on the right 
      guides(fill=guide_legend(title="Countries")) + #change the legend title 
      scale_fill_hue (l=30) #just darken/lighten the colours 
  
   
  })
  
  # OUTPUT2: Show the values in an HTML table: ------------
  # ordered list of the 10 countries with most catches ----
  output$values <- renderTable({
    arrange(df_function()$df3,desc(avg))[c(1:10),]
  }) 
  
}

# Create Shiny app =======================================
shinyApp(ui, server)
