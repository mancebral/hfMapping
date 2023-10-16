library(shiny)
library(DT)
library(tidyr)
library(stringr)
library(dplyr)
library(ggplot2)
#library(plotly)
library(ggpubr)

databaseAll <- readRDS("data/allDatabasesClean.rds")

ui <- fluidPage(

  tags$style(HTML('#distPlot{font-size: 10px;}'),
             type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),

  # Application title
    titlePanel("Human Flourishing Latin American Mapping"),

    # Sidebar with a slider input for number of bins 
   # sidebarLayout(
      column(6,
        wellPanel(
            textInput("words",
                        "Search a word:"),
            checkboxGroupInput("lang", "Filter by language:",
                               choices = c("English"="EN", "Portuguese"="PT", "Spanish"="ES", "Other"="OTHER"),
                               # choicesNames = c("en", "pt", "es", "others"),
                               # choicesValues = c("en", "pt", "es", "others"),
                               selected = c("EN", "PT", "ES", "OTHER"),
                               inline = TRUE
            ),
            sliderInput("year", "Filter by publication year:", min = 2000, max = 2023, step = 1,
                        value = c(2000,2023), timeFormat = "%YYYY"),
            checkboxGroupInput("type", "Filter by type:", choices = c("ARTICLE", "BOOK", "BOOK-CHAPTER", "DISSERTATION"),
                               selected = c("ARTICLE", "BOOK", "BOOK-CHAPTER", "DISSERTATION"),
                               inline = TRUE),
            submitButton("Search!")
        )
        ),
      column(6,
            plotOutput("total")#,
           # plotlyOutput("plotly")
            ),
        
        DTOutput("distPlot", width = "100%")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  filterData <- reactive(
    databaseAll %>% 
      dplyr::filter(str_detect(paste(Title, Abstract, sep= " "), 
                               toupper(input$words))) %>% 
      #dplyr::filter(Language%in%input$lang) %>% 
      #dplyr::filter(Type%in%input$type) %>% 
      dplyr::filter(Year>=input$year[1]) %>% 
      dplyr::filter(Year<=input$year[2]) 
  )
  
  output$total <- renderPlot ({
    totalPlot <- filterData() %>% 
      count(Database) %>% 
      ggplot(aes(Database,n, fill=Database))+
      geom_col()+
      coord_flip()+
      xlab(NULL)+
      ylab(NULL)+
      theme_bw()+
      theme(legend.position='none')
    
    plotCountries <- filterData() %>% 
      separate_rows(Countries) %>% 
      count(Countries) %>% 
      head(10) %>% 
      ggplot(aes(reorder(as.factor(Countries), n), n, fill=Countries))+
      geom_col()+
      coord_flip()+
      xlab(NULL)+
      ylab(NULL)+
      theme_bw()+
      theme(legend.position='none')
    
    #ggplotly(plotCountries)
    ggarrange(totalPlot, plotCountries, ncol = 1, align = "h", heights = c(1,4))
    
    # subplot(list(totalPlot, plotCountries),
    #         #shareX = TRUE,
    #         nrows = 2, heights = c(0.2,0.8),
    #         margin = 0) %>%
    #   config(displayModeBar = FALSE)
  })
  
    output$distPlot <- renderDT (
      filterData() %>% 
        datatable(
          options = list(searching = FALSE, pageLength = 25,lengthMenu = c(50, 100, 500, 1000), scrollX = T,
                         width="100%",autoWidth = TRUE,
                         columnDefs = list(list(targets = 3, width = '600px'),
                                           list(targets = 2, width = '200px')))
          ))
      
    #   output$plotly <- renderPlotly ({
    #   plotCountries <- filterData() %>% 
    #     separate_rows(Countries) %>% 
    #     count(Countries) %>% 
    #     head(10) %>% 
    #     ggplot(aes(reorder(as.factor(Countries), n), n, fill=Countries))+
    #     geom_col()+
    #     coord_flip()+
    #     xlab(NULL)+
    #     ylab(NULL)+
    #     theme(legend.position='none')
    #   
    #   ggplotly(plotCountries) %>% config(displayModeBar = FALSE)
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
