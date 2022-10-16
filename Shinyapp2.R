library(shiny)
library(dplyr)
library(ggplot2)
library(shiny)
library(plotly)
library(shinythemes)
library(readr)
library(Dict)
library(zoo)
packages =  c("ggplot2", "dplyr", "tidyr", "data.table", 'corrplot','ggfortify', 'gridExtra', 'forecast', 'tseries', 'TSA','tsibble' ,'tibble', 'TTR','tidyverse','zoo','readr')

 
my.install <- function(pkg, ...){
  if (!(pkg %in% installed.packages()[,1])) {
    install.packages(pkg)
  }
  return (library(pkg, ...))
}
 

purrr::walk(packages, my.install, character.only = TRUE, warn.conflicts = FALSE)

usdcad_forecast <- read_csv("USDCAD_forecast.csv")
usdmxn_forecast <- read_csv("USDMXN_forecast.csv")
usdkrw_forecast <- read_csv("USDKRW_forecast.csv")
usdjpy_forecast <- read_csv("USDJPY_forecast.csv")
eurusd_forecast <- read_csv("EURUSD_forecast.csv")
df_currency <- read.csv(file ="compiled.csv")
#df_currency$month <- as.Date(as.yearmon(df_currency$month))
inds <- seq(as.Date("2010-01-01"), as.Date("2021-12-01"), by = "month")

create_ts <- function(col_idx){
  ## Create a time series object
  i_ts <- as.numeric(df_currency[,col_idx]) %>%
    tsclean(replace.missing = TRUE, lambda = NULL) %>%
    ts(start = c(2010, as.numeric(format(inds[1], "%j"))),
       frequency = 12)
  return(i_ts)
}

i_ts1 = create_ts(which(colnames(df_currency) == "USDCAD"))
i_tsarima1 <- auto.arima(i_ts1, max.p = 3, max.q = 3, max.d = 3)

i_ts2 = create_ts(which(colnames(df_currency) == "USDMXN"))
i_tsarima2 <- auto.arima(i_ts2, max.p = 3, max.q = 3, max.d = 3)

i_ts3 = create_ts(which(colnames(df_currency) == "USDKRW"))
i_tsarima3 <- auto.arima(i_ts3, max.p = 3, max.q = 3, max.d = 3)

i_ts4 = create_ts(which(colnames(df_currency) == "USDJPY"))
i_tsarima4 <- auto.arima(i_ts4, max.p = 3, max.q = 3, max.d = 3)

i_ts5 = create_ts(which(colnames(df_currency) == "EURUSD"))
i_tsarima5 <- auto.arima(i_ts5, max.p = 3, max.q = 3, max.d = 3)



currencyarima <- Dict$new(
  USDCAD = i_tsarima1,
  USDJPY = i_tsarima4,
  USDKRW = i_tsarima3,
  USDMXN = i_tsarima2,
  EURUSD = i_tsarima5,
  .overwrite = TRUE
)
currencyfc <- Dict$new(
  USDCAD = usdcad_forecast,
  USDJPY = usdjpy_forecast,
  USDKRW = usdkrw_forecast,
  USDMXN = usdmxn_forecast,
  EURUSD = eurusd_forecast,
  .overwrite = TRUE
)

currencynames <- Dict$new(
  USDCAD = "Canada_Inflation",
  USDJPY = "Japan_Inflation",
  USDKRW = "South Korea_Inflation",
  USDMXN = "Mexico_Inflation",
  EURUSD = "Euro Area_Inflation",
  .overwrite = TRUE
)

stack_data <-  read_csv("compiled.csv")
stack_data$month <- as.character(as.Date(paste(stack_data$month,"-01",sep=""), format="%Y-%m-%d"))



ui <- navbarPage("Forex Prediction",
   tabPanel("Data Exploration",
            sidebarLayout(
              sidebarPanel(
                
                selectInput(inputId = "currency",
                            label = "Choose a currency :",
                            choices = c("USDMXN", "USDKRW","USDCAD","EURUSD","USDJPY"),
                            selected = "USDKRW"),
                
                #dateRangeInput("date", strong("Date range"), start = "2010-01-01", end = "2021-01-01",
                #             min = "2010-01-01", max = "2021-01-01")),
                
                dateRangeInput('dateRange',
                               label = 'Filter by date',
                               start = as.Date('2010-01-01') , end = as.Date('2021-12-01')),
              
            ),
            
            mainPanel(
              
            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(type = "tabs",
                        tabPanel("Plot", plotOutput("ts_plot")),
                        tabPanel("Table", tableOutput("table"))
            )))),
   tabPanel(
     "Forecasting",
     sidebarLayout(
       sidebarPanel(
         
         selectInput(inputId = "currency1",
                     label = "Choose a currency :",
                     choices = c("USDMXN", "USDKRW","USDCAD","EURUSD","USDJPY"),
                     selected = "USDKRW"),
         
         #dateInput("startdate", "Plot Start Date:", min = "2010-01-01", max = "2021-12-01",value="2010-01-01", format='yyyy-mm-dd'),
         
         selectInput(inputId = "months", choices=c(1,3,6,9,12,18,24),selected=12,label="No. of forecast months")
         
       ),
       
       mainPanel(
         
         # Output: Tabset w/ plot, summary, and table ----
         tabsetPanel(type = "tabs",
                     tabPanel("Forecast",plotOutput("fc_plot")),
         )))
   )
)

  
  
server <- shinyServer(
  
  function(input,output){
    
    datasetInput <- reactive({
      if(input$dateRange){
        validate(need(!is.na(input$dateRange[1]) & !is.na(input$dateRange[2]), "Error: Please provide both a start and an end date."))
        validate(need(input$dateRange[1] < input$dateRange[2], "Error: Start date should be earlier than end date."))  
      }
      
      #stack_data <- stack_data[stack_data$month >= input$dateRange[1] & stack_data$month <= input$dateRange[2], ]
      
      #stack_data[,c(input$currency)]
      stack_data[stack_data$month >= input$dateRange[1] & stack_data$month <= input$dateRange[2],c(input$currency,currencynames[input$currency],"3M T-Bill")]
    })
    
    tabledatasetInput <- reactive({
      if(input$dateRange){
        validate(need(!is.na(input$dateRange[1]) & !is.na(input$dateRange[2]), "Error: Please provide both a start and an end date."))
        validate(need(input$dateRange[1] < input$dateRange[2], "Error: Start date should be earlier than end date."))  
      }
      stack_data <- stack_data[stack_data$month >= input$dateRange[1] & stack_data$month <= input$dateRange[2], ]
      stack_data[,c('month',input$currency)]
    })
    
    tableforecastInput <- reactive({
      ar <- currencyarima[input$currency1]
      i_tsforecasts <-forecast(ar, h = input$months)
      
      
    })
    

    
    # plot time series
    output$ts_plot <- renderPlot({
      
      dataset <- datasetInput()
      
      plot.ts(dataset%>%ts(start = c(year(input$dateRange[1]), month(input$dateRange[1])),end = c(year(input$dateRange[2]),month(input$dateRange[2])),
                           frequency = 12), xlab = "Time", ylab = input$currency, main = "Time Series", col = "red")
      
    })
    
    output$fc_plot <- renderPlot({
      
      i_tsforecast <- tableforecastInput()
      plot(i_tsforecast, col = "red")
      
    })
    
    output$table <- renderTable({
      tabledatasetInput <- tabledatasetInput()
      head(tabledatasetInput,20)
    })
  })


shiny::shinyApp(ui = ui, server = server)