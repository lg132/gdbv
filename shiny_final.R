library(shiny)
library(ggplot2)
library(tidyverse)
library(tmap)
library(sf)


#read in data: ----
df_total_catch <- read.delim("df_total_catch")
df_discards <- read.delim("df_discards")
df_eez <-read.delim("df_eez")
sau_id <-read.delim("sau_id")

#eez_shp <- st_read("../../../../Dropbox/GeoVis/World_EEZ_v8_20140228_LR/World_EEZ_v8_2014.shp")
eez_shp <- st_read("~/Dropbox/GeoVis/World_EEZ_v8_20140228_LR/World_EEZ_v8_2014.shp")

#Add the SeeAroundUs EEZ Id  to the shapefile:
eez_shp_sau <- merge(eez_shp, sau_id, by="Country", all.x=T) %>% dplyr::select(Country, EEZ, sau_id, Longitude, Latitude, geometry)
eez_shp_sau <- eez_shp_sau %>% dplyr::select(Country, EEZ, sau_id, Longitude, Latitude, geometry)

# Define UI for seaaroundus app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("seaaroundus"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select-option for type of information in plots ----
      selectInput(inputId = "type",
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
                  tabPanel("Table", tableOutput("valuesTable")),
                  tabPanel("Map", plotOutput(outputId = "mapPlot"))
      )
    )
  )
)


# Define server logic required to draw plots and show tables ----
server <- function(input, output) {
  
  # First reactive function returning basis for plots and tables (output 1-3)
  dataInput <- reactive({
    
    data <- switch(input$type,
                   "total catch" = df_total_catch,
                   "discards" = df_discards)
    
    range <- input$range
    
    number <- input$number
    
    #Filter the dataframe (total_catch or discards) depending on the selected time range:
    data_plot <- data %>% filter(years>=range[1], years<=range[2])
    
    if(input$type == "total catch"){
      
      #average total catch for every country:
      data_table <- data_plot %>% gather(., key="country", value="tonnage", -c(years)) %>%
        group_by(country) %>% summarise(avg=sum(tonnage))
      
      #average total catch for every EEZ:
      data_map <- df_eez %>%
        filter(years>=range[1], years<=range[2]) %>% group_by(sau_id) %>%
        summarise(avg=mean(landings+discards))
    }
    else { 
      
      #average percentage of discards for every country:
      data_table <- data %>%
        mutate(perc_disc = (discards/(landings+discards))*100) %>% 
        group_by(country) %>% summarise(avg=mean(perc_disc))
      
      #average percentage of discards for every EEZ:
      data_map <- df_eez %>%
        filter(years>=range[1], years<=range[2]) %>% group_by(sau_id) %>%
        summarise(avg=mean(discards/(landings+discards))*100)
    }
    
    return(list("data_plot"=data_plot, "data_table" = data_table, "data_map" = data_map,
                "number"=number))
  })
  
  # Second reactive expression returning values for tables (output 2) ----
  calcTable <- reactive({
    
    data_table <- dataInput()$data_table
    
    if(input$type=="total catch"){
      #sorting the table by average catch (from highest to lowest), take only the selected number 
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
    
    if (input$type == "total catch"){
      
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
      data <- data %>%
        mutate(perc_disc = (discards/(landings+discards))*100)
      data1 <- data %>%
        group_by(country) %>%
        summarise(avg=mean(perc_disc)) %>%
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
  output$valuesTable <- renderTable({
    calcTable()
  })
  
  # Output 3: Map ----
  output$mapPlot <- renderPlot({
    
    data_map <- dataInput()$data_map

    eez_merge <- merge(eez_shp_sau, data_map, by="sau_id", all.x=T)

    tm_shape(eez_merge, fill="avg")+
      tm_fill("avg", palette = "Blues", n=5, style = "jenks", legend.hist = T)
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
