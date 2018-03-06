library(shiny)
library(ggplot2)
library(tidyverse)

#read in data: ===============================================
df_fishing_all <- source("df_fishing_all.Rdmpd")
df_fishing_all <- df_fishing_all[[1]]
df_Q2 <- read.delim("df_Q2")

# Define UI for slider demo app ===============================
ui <- fluidPage(
  
  # App title ----
  titlePanel("seaaroundus"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for range of years to be displayed ----
      sliderInput(inputId = "range",
                  label = "Years:",
                  min = 1950,
                  max = 2014,
                  sep = "",
                  value = c(1950, 2014)),
      
      # Input: Select-option for data.frame
      selectInput(inputId = "dim",
                  label = "select dimension",
                  choices = c("total catches", "discards")),
      
      radioButtons(inputId = "table_len",
                   label = "Show in table:",
                   inline=T,
                   choiceNames = c("5", "10", "15", "20", "all"),
                   choiceValues = c(5, 10, 15, 20, 197))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: The two tab panels:-----------------
      tabsetPanel(type="tabs",
                  tabPanel("Graph", plotOutput(outputId = "areaPlot")),
                  tabPanel("values", tableOutput("values"))
      )
    )
  )
)


# Define server logic required to draw a plot ----
server <- function(input, output) {
  
  # Reactive expression to create data frame of all input values ----
  sliderValues <- reactive({
    
    data.frame(
      Name = c("Range"),
      Value = as.character(c(paste(input$range, collapse = " ")
      )),
      stringsAsFactors = FALSE)
    
  })
  
  # Reactive expression to create plots
  
  dataInput <- reactive({
    
    data <- switch(input$dim,
                   "total catches" = df_fishing_all,
                   "discards" = df_Q2)
    range <- input$range
    
    
    data_plot <- data %>% filter(years>=range[1], years<=range[2])
    
    if(input$dim == "total catches"){
      data_table <- data_plot %>% gather(., key="country", value="tonnage", -c(years)) %>%
        group_by(country) %>% summarise(avg=sum(tonnage))
    }
    else {
      data_table <- data %>%
        mutate(perc_disc = (discards/(landings+discards))*100) %>% 
        group_by(country) %>% summarise(avg=mean(perc_disc))
    }
    
    return(list("data_plot"=data_plot, "data_table" = data_table))
  })
  
  calcTable <- reactive({
    data_table <- dataInput()$data_table
    if(input$dim=="total catches"){
      data_table <- arrange(data_table, desc(avg))[c(1:input$table_len),]
      colnames(data_table) <- c("country", "sum of catches in tons")
    }
    else{
      data_table <- arrange(data_table, desc(avg))[c(1:input$table_len),]
      colnames(data_table) <- c("country", "average discards in %")
    }
    
    return(data_table)
  
  })
  
  
  # OUTPUT 1: Plots
  output$areaPlot <- renderPlot({
    
    data <- dataInput()$data_plot
    
    if (input$dim == "total catches"){
      data <- data %>% gather(., key="country", value="tonnage", -c(years)) #%>% 
      #  filter(years>=input$range[1], years<=input$range[2])
      data1 <- data %>% group_by(country) %>% summarise(avg=mean(tonnage)) %>% filter(avg>2000000)
      data2 <- data %>% group_by(country) %>% summarise(avg=mean(tonnage)) %>% filter(avg<2000000)
      data_high <- data %>% filter(country %in% data1$country)
      data_low <- data %>% filter(country %in% data2$country) %>% group_by(years) %>%
        summarise(tonnage = sum(tonnage)) %>% mutate(country = "Others") %>% select(years,country,tonnage)
      
      data <- bind_rows(data_high, data_low)
      
      ggplot(data=data, aes(x= years, y=tonnage))+
        geom_area(aes(fill=factor(country, levels=c("Others", data1$country))))+
        theme(legend.position = "right")+
        guides(fill=guide_legend(title="countries"))+
        labs(title = "Catch by countries (> 2,000,000 tons)")
    }
    else {
      data <- data %>%
        mutate(perc_land = landings/(landings+discards)) %>%
        mutate(perc_disc = (discards/(landings+discards))*100)
      data1 <- data %>%
        group_by(country) %>%
        summarise(avg=mean(perc_disc)) %>%
        filter(avg>30)
      
      data_high <- data %>% filter(country %in% data1$country)
      
      ggplot(data=data_high, aes(x=years, y=perc_disc, colour=country))+
        geom_line(size=1.3)+
        labs(title = "Discards > 30%",  y="percentage")+
        theme(legend.position = "bottom")
      
      # ggplot(data=data_high, aes(x=years, y=perc_disc))+
      #   geom_area(aes(fill=country))+
      #   labs(title = "Discards > 30%",  y="percentage")+
      #   theme(legend.position = "right")
    }
    
    
  })
  
  # OUTPUT 2: Table
  output$values <- renderTable({
    calcTable()
  }) 
}


# Create Shiny app ----
shinyApp(ui = ui, server = server)
