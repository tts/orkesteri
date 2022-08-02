library(shiny)
library(shinydashboard)
library(cowplot)
library(tidyverse)

data_df_v <- readRDS(file = "data_df_v.RDS")

composers <- unique(data_df_v[, "Säveltäjä"])

ui <- function(request) {
  
  sidebar <- dashboardSidebar(
    width = 300,
    sidebarMenu(
      selectizeInput(inputId = "composers",
                  label = "Composers",
                  choices = sort(composers),
                  multiple = TRUE,
                  selected = NULL)
    ),
    tags$div(class="form-group shiny-input-container", 
             HTML("<p>Data: Helsingin kaupunginorkesterin konsertit. Fetched 2020-12-07</p>")
    ))
  
  
  body <- dashboardBody(
    
    fluidRow(
      column(width = 12,
             height = "300px",
             plotOutput("comchart", width = "100%", height = "300px"))
    ),
    fluidRow(
      column(width = 12,
             height = "300px",
             plotOutput("conchart", width = "100%", height = "300px"))
    )
    
  )
  
  
  dashboardPage(
    dashboardHeader(title = "Helsingin kaupunginorkesterin konsertit 1882-", titleWidth = "800"),
    sidebar,
    body,
    skin = "black"
  )
  
}


server <- function(input, output, session) {
  
  
  selectedComposers <- reactive({
    
    req(input$composers)
    
    data_df_v %>% 
      filter(Säveltäjä %in% input$composers) 
    
  })
  
  
  output$comchart <- renderPlot({
    
    df_com <- selectedComposers() %>% 
      group_by(Vuosi, Säveltäjä) %>%  
      summarise(Konsertteja = n())
    
    p <- ggplot(data = df_com, aes(Vuosi, Konsertteja, fill = Säveltäjä))
    
    p <- p + geom_bar(stat="identity")
    
    print(p)
    
  })
  
  
  output$conchart <- renderPlot({
    
    df_con <- selectedComposers() %>% 
      group_by(Vuosi, Säveltäjä, Kapellimestari) %>%  
      summarise(Konsertteja = n()) %>% 
      filter(Konsertteja >= 10 & !is.na(Kapellimestari))
    
    p <- ggplot(data = df_con, aes(Vuosi, Konsertteja))
    
    p <- p + geom_point(aes(colour = factor(Kapellimestari)), position = "jitter")
    
    print(p)
    
  })
  
}


shinyApp(ui = ui, server = server)
