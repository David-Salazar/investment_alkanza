#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(xts)
library(tidyverse)
library(quantmod)
library(ggfortify)
library(plotly)
library(magrittr)
library(lubridate)
library(PerformanceAnalytics)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Comparación de Mercado"),
   
   # Sidebar with a date input
   sidebarLayout(
      sidebarPanel(
        dateInput("fecha_inicial", "Fecha de inicio de comparación:", value = "2013-01-01"),
        dateInput("fecha_final", "Fecha final de comparación:", value = Sys.Date())
        ),
      
      # Show a plot of the generated distribution
      mainPanel(
        
        h1("iCOLCAP vs. Renta Fija 4%"),
        h2("David Salazar"),
        
        p("COLCAP es el índice (market-cap de país) con peor rendimiento en los últimos cinco años, 
          de acuerdo a ",  
          a(href = "https://www.bloomberg.com/news/videos/2018-03-23/colombia-s-duque-says-trade-limits-would-hurt-emerging-economies-video", "Bloomberg.")),
        
        p("A continuación, muestro los siguientes datos para una inversión inicial de $5,339,692:"),
        tags$ul(
          tags$li(" Tasa E.A. de iCOLCAP (ETF) "),
          tags$li(" Rendimiento en absoluto de iCOLCAP vs instrumento de renta fija de 4% E.A."),
          tags$li(" Gráfica de comportamiento del dinero en el tiempo. ")),
        
        
        p("Esta aplicación calcula todos estos datos de forma interactiva de acuerdo a las fechas seleccionadas, gracias a que 
          descarga los datos en tiempo real desde Yahoo Finance."),
        
        tags$br(),
        
        tags$b(textOutput("ea.colcap")),
        
        tags$br(),
        
        
        tags$b(textOutput("diff.colcap")),
        
        
        plotlyOutput("plot_comparison")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
  dataInput <- reactive({
    
    ### ETF Data
    icolcap <- Ad(getSymbols("ICOL", src = "yahoo", from = input$fecha_inicial, to = input$fecha_final, 
                             auto.assign = FALSE))
    
    ### Calculate Data
    icolcap_returns <- Return.calculate(icolcap, method = "discrete")
    initial_investment <-  5339692 ##  $5,339,692
    
    icolcap_returns_plus <- icolcap_returns + 1
    icolcap_returns_plus[1] <- initial_investment
    
    time_value <- cumprod(icolcap_returns_plus)
    
    ### 4% EA
    
    other <- icolcap_returns 
    coredata(other) <- rep((1+0.04)^(1/252), length(icolcap_returns))
    other[1] <- initial_investment
    other_cumulative <- cumprod(other)
    
    ### Merge
    
    total <- merge.xts(time_value, other_cumulative)
    total_tbl <- timetk::tk_tbl(total)
    colnames(total_tbl) <- c("Fecha","iCOLCAP", "Renta Fija 4%")
    total_tbl_new <- total_tbl %>%  mutate(diff = abs(iCOLCAP - `Renta Fija 4%`))
    
    
    total_tbl_gather <- total_tbl_new %>%  
      gather(key = "variable", value = "valor", -c(Fecha, diff))
    
    
    list(total_tbl_gather, icolcap_returns)
    
  })
  
  output$ea.colcap <-  renderText({
    
    data <- dataInput()
    time_series <- data[[2]]
    
    ea <- round(Return.annualized(time_series)[1]*100, 2)
    
    paste0("La tasa E.A. del iCOLCAP, para el período seleccionado, es de : ", ea , "%")
  })
  
  output$diff.colcap <- renderText({
    
    data <- dataInput()
    
    dato <- data[[1]]
    
    x <- dato[nrow(dato), 2][[1]]
    
    dollar_d <- scales::dollar_format()
    
    y <- dollar_d(x)
    
    paste0("La diferencia absoluta en el rendimiento entre los dos activos para el período fue de : ",
           y)
  })
   
  output$plot_comparison <- renderPlotly({
     
     data <- dataInput()
     
     dollar_d <- scales::dollar_format()
     
     plot <-  ggplot(data[[1]], aes(x = Fecha, y = valor, color = variable,
                                           text = paste("Fecha: ", as.Date(Fecha),
                                                        '<br>Valor : ', dollar_d(valor),
                                                        "<br>Diferencia :", dollar_d(diff)
                                           ), group = 1)) + ## trick from: https://stackoverflow.com/questions/47507186/tooltip-removes-regression-line-ggplotly
       geom_line() +
       hrbrthemes::theme_ipsum() +
       ggthemes::scale_color_gdocs(guide = FALSE) +
       labs(x = "",
            y = "Inversión a través del tiempo",
            title = "Inversión en iCOLCAP") +
       scale_y_continuous(labels= scales::dollar_format()) 
     ggplotly(plot, tooltip = c("text"))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

