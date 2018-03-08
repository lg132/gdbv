library(shiny)
library(ggplot2)
library(tidyverse)
library(tmap)


#read in data: ----
df_fishing_all <- source("df_fishing_all.Rdmpd")
df_fishing_all <- df_fishing_all[[1]]
df_Q2 <- read.delim("df_Q2")
df_eez <-read.delim("df_eez")
eez_shp_sau <- source("eez_shp_sau.Rdmpd")
eez_shp_sau <- eez_shp_sau[[1]]

# Define UI for seaaroundus app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("seaaroundus"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select-option for type of information in plots ----
      selectInput(inputId = "dim",
                  label = "Select type",
                  choices = c("total catch", "discards")),
      
      # Input: Slider for range of years to be displayed ----
      sliderInput(inputId = "range",
                  label = "Select time span",
                  min = 1950,
                  max = 2014,
                  sep = "",
                  value = c(1950, 2014)),
      
      # Input: Select-option for number of countries to be displayed in plots
      selectInput(inputId = "number",
                  label = "Select number of countries in Graph",
                  choices = c(1:12),
                  selected = 6),
      
      # Input: Select-option for number of entries to be listed in tables
      radioButtons(inputId = "table_len",
                   label = "Select number of entries in Table",
                   inline=T,
                   choiceNames = c("5", "10", "15", "20", "all"),
                   choiceValues = c(5, 10, 15, 20, 197))
    ),
    
    # Main panel for displaying outputs with options ----
    mainPanel(
      
      # Output: Two tab panels: ----
      tabsetPanel(type="tabs",
                  tabPanel("Graph", plotOutput(outputId = "areaPlot")),
                  tabPanel("Table", tableOutput("values")),
                  tabPanel("Map", plotOutput(outputId = "mapPlot"))
      )
    )
  )
)


# Define server logic required to draw plots and show tables ----
server <- function(input, output) {
  
  # First reactive function returning basis for plots and tables (output 1 and 2)
  dataInput <- reactive({
    
    data <- switch(input$dim,
                   "total catch" = df_fishing_all,
                   "discards" = df_Q2)
    
    range <- input$range
    
    number <- input$number
    
    data_plot <- data %>% filter(years>=range[1], years<=range[2])
    
    if(input$dim == "total catch"){
      data_table <- data_plot %>% gather(., key="country", value="tonnage", -c(years)) %>%
        group_by(country) %>% summarise(avg=sum(tonnage))
      data_map <- df_eez %>% 
        filter(years>=range[1], years<=range[2]) %>% group_by(eez) %>% 
        summarise(avg=mean(landings+discards))
    }
    else {
      data_table <- data %>%
        #mutate(perc_disc = (discards/(landings+discards))*100) %>% 
        group_by(country) %>% summarise(avg=mean((discards/(landings+discards))*100))
      data_map <- df_eez %>%
        filter(years>=range[1], years<=range[2]) %>% group_by(eez) %>% 
        summarise(avg=mean(discards/(landings+discards))*100)
    }
    
    return(list("data_plot"=data_plot, "data_table" = data_table, "data_map" = data_map, "number"=number))
  })
  
  # Second reactive expression returning values for tables (output 2) ----
  calcTable <- reactive({
    
    data_table <- dataInput()$data_table
    
    if(input$dim=="total catch"){
      data_table <- arrange(data_table, desc(avg))[c(1:input$table_len),]
      colnames(data_table) <- c("country", "average catch per year in tons")
    }
    else{
      data_table <- arrange(data_table, desc(avg))[c(1:input$table_len),]
      colnames(data_table) <- c("country", "share of discards in average catch per year in %")
    }
    
    return(data_table)
  })
  
  
  # Output 1: Plots ----
  output$areaPlot <- renderPlot({
    
    data <- dataInput()$data_plot
    number <- as.integer(dataInput()$number)
    
    if (input$dim == "total catch"){
      
      data <- data %>% gather(., key="country", value="tonnage", -c(years)) 
      
      data_arranged <- data %>% group_by(country) %>% summarise(avg=mean(tonnage)) %>% arrange(., desc(avg))
      
      data_high <- data %>% filter(country %in% data_arranged[c(1:number),]$country)
      data_low <- data %>% filter(country %in% data_arranged[c((number +1):nrow(data)),]$country)  %>% group_by(years) %>%
        summarise(tonnage = sum(tonnage)) %>% mutate(country = "Others") %>%
        select(years,country,tonnage)
      
      
      data <- bind_rows(data_high, data_low)
      
      ggplot(data=data, aes(x= years, y=tonnage))+
        geom_area(aes(fill=factor(country, levels=c(data_arranged[c(1:number),]$country, "Others"))))+
        theme(legend.position = "right")+
        guides(fill=guide_legend(title="countries"))+
        labs(title = "Total catch")
    }
    else {
      # data <- data %>%
      #   mutate(perc_disc = (discards/(landings+discards))*100)
      data1 <- data %>%
        group_by(country) %>%
        summarise(avg=mean((discards/(landings+discards))*100)) %>%
        arrange(., desc(avg)) %>% 
        top_n(., n=number)
      
      data_high <- data %>% filter(country %in% data1$country)
      
      ggplot(data=data_high, aes(x=years, y=perc_disc, colour=country))+
        geom_line(size=1.3)+
        theme(legend.position = "right")+
        labs(title = "Share of discards in total catch", y = "percentage")
    }
  })
  
  # Output 2: Table ----
  output$values <- renderTable({
    calcTable()
  })
  
  # Output 3: Map ----
  output$mapPlot <- renderPlot({
    
    eez_merge <- merge(eez_shp_sau, data_map, by="sau_id", all.x=T)
    
    tm_shape(eez_merge, fill="avg")+
      tm_fill("avg", palette = "Blues", n=5, style = "jenks", legend.hist = T)
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)