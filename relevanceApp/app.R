library(shiny)
library(tidyverse)
library(plotly)
library(DT)

data <- readRDS("data/Words3_Authors.rds")
papers <- readRDS("data/allDatabasesClean.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  tags$style(HTML("#worksTable {
             font-size: 11px; }")
             ),

    # Application title
    titlePanel("HF in Latinamerica Relevance App"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          radioButtons("radio", label = "Criteria of Popularity",
                       choices = list("Productivity" = "productivity", "Total Citations" = "TC"), 
                       selected = "productivity"),
            sliderInput("productivity",
                        "Grade of Productivity:",
                        min = min(data$productivity),
                        max = max(data$productivity),
                        value = 16),
            sliderInput("relevance",
                        "Grade of Relevance:",
                        min = min(data$score),
                        max = max(data$score),
                        value = 50),
          plotlyOutput("summary")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          HTML("Each point is an author, you can consult their works by clicking on the points."),
           plotlyOutput("distPlot"),
           dataTableOutput("worksTable"),
           textOutput("authorName")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # updateSelectizeInput(session, 'countries', choices = data$Countries, server = TRUE, 
  #                      selected = NULL)

    output$distPlot <- renderPlotly(if (input$radio=="productivity"){
      plotScores <- data %>% 
        #filter(if (!is.null(input$countries)) Countries==input$countries else Countries%in%Countries) %>% 
        #filter(Countries %in% input$countries) %>% 
        filter(productivity>=input$productivity) %>% 
        filter(score>=input$relevance) %>% 
        mutate(scoreN=(score-min(score))/(max(score)-min(score))) %>% 
        mutate(productivityN=(productivity-min(productivity))/(max(productivity)-min(productivity))) %>% 
        mutate(text=paste0(Authors, "\n", 
                           Affiliations, "\n",
                           Countries, "\n",
                           "Productivity: ", productivity, "\n",
                           "total Citations: ", TC, "\n",
                           "Relevance: ", score)) %>% 
        ggplot(aes(scoreN, productivityN, color= Countries, text=text, key=Authors))+
        geom_jitter(alpha=0.6, height = 0, width = 0.005)+
        xlab("Normalized Relevance")+
        ylab("Normalized Productivity")
      
      ggplotly(plotScores, tooltip = "text")
    } else {
      plotScores <- data %>% 
        #filter(if (!is.null(input$countries)) Countries==input$countries else Countries%in%Countries) %>% 
        #filter(Countries %in% input$countries) %>% 
        filter(productivity>=input$productivity) %>% 
        filter(score>=input$relevance) %>% 
        mutate(scoreN=(score-min(score))/(max(score)-min(score))) %>% 
        mutate(TCN=(TC-min(TC))/(max(TC)-min(TC))) %>% 
        mutate(text=paste0(Authors, "\n", 
                           Affiliations, "\n",
                           Countries, "\n",
                           "Productivity: ", productivity, "\n",
                           "Total Citations: ", TC, "\n",
                           "Relevance: ", score)) %>% 
        ggplot(aes(scoreN, TCN, color= Countries, text=text, key=Authors))+
        geom_jitter(alpha=0.6, height = 0, width = 0.005)+
        xlab("Normalized Relevance")+
        ylab("Normalized Total Citations")
      
      ggplotly(plotScores, tooltip = "text")
    }
    )
    
    output$summary <- renderPlotly({
      summaryPlot <- data %>% 
        filter(productivity>=input$productivity) %>% 
        filter(score>=input$relevance) %>%
        group_by(Countries) %>% 
        summarize(items=n()) %>% 
        ungroup() %>% 
        mutate(text=paste0(Countries, "\n", "Total documents: ", items)) %>% 
        ggplot(aes(items, reorder(as.factor(Countries), items), fill=Countries, text=text))+
        geom_col()+
        xlab(NULL)+
        ylab(NULL)+
        guides(fill=FALSE)+
        theme(plot.background = element_rect(fill = "#f5f5f5"),
              panel.background = element_rect(fill = "#f5f5f5"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text.x=element_blank(), 
              axis.ticks.x=element_blank())
      
      ggplotly(summaryPlot, tooltip = "text") %>% config(displayModeBar = FALSE)
    })
    
    output$authorName <- renderText(pull(event_data("plotly_click")$key))
    
    output$worksTable = renderDataTable({
      papers %>% 
        filter(str_detect(Authors, event_data("plotly_click")$key)) %>% 
        arrange(desc(Citations)) %>% 
        datatable(escape = FALSE,
                  options = list(searching = FALSE, pageLength = 25,lengthMenu = c(50, 100, 500, 1000), scrollX = T,
                                 width="100%",autoWidth = TRUE))
    })
    #div(DT::dataTableOutput("worksTable"), style = "font-size: 10%")
}

# Run the application 
shinyApp(ui = ui, server = server)
