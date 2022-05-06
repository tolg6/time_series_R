library(tidyverse)
library(forecast)
library(urca)
library(highcharter)
library(quantmod)
library(TTR)
library(ggExtra)
library(PerformanceAnalytics)
library(shinydashboard)
library(shinythemes)

climate = read.csv("C:/Users/tolga/Desktop/data/Global-mean monthly.csv")
climate = climate%>%select(-J.D ,-D.N,  -DJF,-MAM,-JJA,-SON)
x = seq.Date(from = as.Date("1880/1/1"),to = as.Date("2022/2/1"),by = "month")
l = NULL
## Merge row(year) and columns(month)
for(i in 1:dim(climate)[1])
{
  for(j in 2:13)
  {
    l = c(l,climate[i,j])
  }
}
l = l[-c(1707:1716)] # drop *** value
climate_new = data.frame(Date = x,Mean = as.numeric(l))

##create ts object
climatets = ts(climate_new$Mean,start = c(1880,1,1),frequency = 12)


# Predict 12SMA Data
sma12 = SMA(climatets,24)%>%na.omit()
train = window(sma12,end = c(2020,12))
test = window(sma12,start = c(2021,1))
model = Arima(train,order = c(3,1,0))
pred = forecast(model,h = length(test))
acc = accuracy(pred,test)

## auto arima
model1 = auto.arima(y = log1p(train),max.d = 2,seasonal = T,stepwise = T,approximation = T,trace = T,allowdrift = F)
pred1 = forecast(model1,h = length(test))
acc1 = accuracy(expm1(pred1$mean),test)



# Next 6 month predict
full_model = Arima(log1p(climatets),order = c(4,1,2),seasonal = c(2,0,1))
pred_next = forecast(full_model,h = 24)
climate_sep = climate_new%>%filter(Date > as.Date("2000-01-01"))
climate_sep_ts = ts(climate_sep$Mean,start = c(2000,1),frequency = 12)

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Monthly Climate Change Forecasting"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("First Look Data", tabName = "first_look", icon = icon("dashboard")),
      menuItem("Forecasting",tabName = "forecasting",icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "first_look",
              fluidRow(
                box(dateRangeInput("plot1_daterange",label = "Select Date Range",start = "1880-01-01",end = "2022-02-01")),
                box(highchartOutput("plot1",width = 1500,height = 800),height = 900,width = 1550)
              )
      ),
      tabItem(tabName = "forecasting",
              fluidRow(
                box(dateRangeInput("test_range",label = "Set Test Data Range",start = "2015-01-06",end = "2022-02-01")),
                box(plotOutput("forecast_310",width = 800,height = 500)),
                box(plotOutput("next24",width = 800,height = 500)),
                box(plotOutput("forecast_arima",width = 800,height = 500))
              )
              
      )
    )
    
  )
)

server <- function(input, output) {
  output$plot1 <- renderHighchart({
    date_filtered_data = climate_new%>%filter(Date >=as.Date(input$plot1_daterange[1]) & Date <= as.Date(input$plot1_daterange[2]))
    highchart(type = "chart") %>% 
      hc_add_series(date_filtered_data$Mean, type = "line", color = "red")%>%
      hc_add_series(SMA(date_filtered_data$Mean,12),type = "line",color = "black",labels = "SMA12")%>%
      hc_add_series(SMA(date_filtered_data$Mean,24),type = "line",color = "green",labels = "SMA24")%>%
      hc_add_series(SMA(date_filtered_data$Mean,36),type = "line",color = "blue",labels = "SMA36")%>%
      hc_title(text = "Mean Global Change")%>%
      hc_subtitle(text = "Souce : Nasa GISS")%>%
      hc_xAxis(categories = c("1900", "1920", "1940", "1960", "1980", "2000", "2020"))
  })
  fitted_layer <- forecast::autolayer(pred$mean, series="ARIMA(3,1,0)",size = 2)
  fitted_values <- fitted_layer$layer_data()
  output$forecast_310 = renderPlot({
    forecast::autoplot(test,series = "Test",size = 2) +
      fitted_layer +
      geom_point() +
      geom_point(data = fitted_values, aes(x = timeVal, y = seriesVal))+
      ggtitle(label = paste0(" ARIMA(3,1,0) -- MAE : ",round(acc[6],3)))+ylab("Value")+theme_minimal()
  })
  
  
  fitted_layer1 <- forecast::autolayer(expm1(pred1$mean), series="AutoArima",size = 2)
  fitted_values1 <- fitted_layer1$layer_data()
  output$forecast_arima = renderPlot({
    forecast::autoplot(test,series = "Test",size = 2) +
      fitted_layer1 +
      geom_point() +
      geom_point(data = fitted_values1, aes(x = timeVal, y = seriesVal))+
      ggtitle(label = paste0(" ARIMA(4,1,2)(2,0,1) -- MAE : ",round(acc1[3],3)))+ylab("Value")+theme_minimal()
  })
  
  
  fitted_layer2 <- forecast::autolayer(expm1(pred_next$mean), series="Forecast",size = .5)
  fitted_values2 <- fitted_layer2$layer_data()
  output$next24 = renderPlot({
    forecast::autoplot(climate_sep_ts,series = "Train",size = .5) +
      fitted_layer2 +
      geom_point()+
      ggtitle(label = paste0(" ARIMA(4,1,2)(2,0,1) Next 24 Month Prediction"))+ylab("Value")+theme_minimal()
  })
}

shinyApp(ui, server)



