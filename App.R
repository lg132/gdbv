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
                  choices = c("country", "catchtype"),
      
      radioButtons(inputId = "dim", "alle oder nur zehn", c(0, 10)))
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
                   "country" = df_fishing_all,
                   "catchtype" = df_Q2)
    range <- input$range
    
    return(list("data"=data, "range"=range))
  })
  
  tableInput <- reactive({
    data <- dataInput()$data
    range <- dataInput()$range
    data <- data %>% filter(years>=range[1], years<=range[2])
    
    if(colnames(data)[2]=="albania"){
      data <- data %>% gather(., key="country", value="tonnage", -c(years)) %>%
        group_by(country) %>% summarise(avg=mean(tonnage))
    }
    else {
      data <- data %>%
        mutate(perc_disc = discards/(landings+discards)) %>% 
        group_by(country) %>% summarise(avg=mean(perc_disc))
    }
    
    return("data"=data)
  })
  
  # OUTPUT 1: Plots
  output$areaPlot <- renderPlot({
    
    data <- dataInput()$data
    range <- dataInput()$range
    data <- data %>% filter(years>=range[1], years<=range[2])
    
    # if (colnames(data)[2]=="landings"){
    #   data <- data %>%
    #     mutate(perc_land = landings/(landings+discards)) %>% 
    #     mutate(perc_disc = discards/(landings+discards))
    #   data1 <- data %>%
    #     group_by(country) %>%
    #     summarise(avg=mean(perc_disc)) %>% 
    #     filter(avg>0.3)
    #   data2 <- data %>%
    #     group_by(country) %>%
    #     summarise(avg=mean(perc_disc)) %>%
    #     filter(avg<0.3)
    #   data_high <- data %>% filter(country %in% data1$country)
    # 
    #   ggplot(data=data_high, aes(x=years, y=perc_disc))+
    #     geom_area(aes(fill=country))
    #   }
    
    if (colnames(data)[2]=="albania"){
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
        mutate(perc_disc = discards/(landings+discards))
      data1 <- data %>%
        group_by(country) %>%
        summarise(avg=mean(perc_disc)) %>%
        filter(avg>0.3)
      # data2 <- data %>%
      #   group_by(country) %>%
      #   summarise(avg=mean(perc_disc)) %>%
      #   filter(avg<0.3)
      data_high <- data %>% filter(country %in% data1$country)
      
      ggplot(data=data_high, aes(x=years, y=perc_disc))+
        geom_area(aes(fill=country))+
        labs(title = "Discards > 30%",  y="percentage")+
        theme(legend.position = "right")
    }
    
    #    if (colnames(data)[2]=="catchtype"){
    #      ggplot(data=data, aes(x=years, y=percentage))+
    #        geom_area(aes(fill=catchtype))+
    #        scale_fill_brewer(palette = "Dark2")
    #    }
    
  })
  
  # OUTPUT 2: Table
  output$values <- renderTable({
    arrange(tableInput(), desc(avg))[c(1:10),]
  }) 
}


# Create Shiny app ----
shinyApp(ui = ui, server = server)
