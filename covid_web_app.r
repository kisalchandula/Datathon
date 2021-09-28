library(RCurl)#for dawnloard the csv file
library(shiny)
library(shinythemes)
library(leaflet)
library(sp)
library(covid19.analytics)
library(dplyr)
library(prophet)
library(lubridate)
library(ggplot2)
library(dygraphs)
library(plotly)
library(lubridate)
library(xts)
################Over Libraries
ui <- fluidPage(theme = shinytheme("cerulean"),
                # use a gradient in background
                
                navbarPage(
                  # theme = "cerulean",  # <--- To use a theme, uncomment this
                  "COVID-PREDICTOR",
                  tabPanel("Island wide",
                           sidebarPanel(
                             h1("COVID-PREDICTOR"),
                             h1("SRI LANKA"),
                             h4("#Team cifra")
                             
                           ), # sidebarPanel
                           mainPanel( 
                             h1("Island wide confirmed cases"),
                             leafletOutput("mymap"),
                             
                             
                             h1("Island wide predicted cases for next 20 days"),
                             plotlyOutput( "displot"),
                             
                           ) # mainPanel
                           
                  ), # Navbar 1, tabPanel
                  tabPanel("Existing Clusters", 
                           sidebarPanel(
                             h1("COVID-PREDICTOR"),
                             h1("SRI LANKA"),
                             h4("#Team cifra")
                             
                           ), # sidebarPanel
                           mainPanel( 
                             h1("brandex cluster cases for next 20 days"),
                             plotlyOutput( "brandex_plot"),
                             h1("overseas cluster cases for next 20days"),
                             plotlyOutput("overseas_cluster")
                             
                           ) # mainPanel
                  ),
                  tabPanel("Navbar 3", "This panel is intentionally left blank")
                  
                ) # navbarPage
) # fluidPage

# Define server function

server <- function(input, output,session) {
  
  
  
  output$mymap<-renderLeaflet({
    
    iri<-read.csv(text = getURL("https://raw.githubusercontent.com/ishancoderr/Covid_19_sri_lanka/main/Distribution_covid19.csv")) 
    iri$Total_positive_cases=cut(iri$totalcase,
                                 breaks = c(0,10,100,500,1000,2000,5000,10000),right = FALSE,
                                 labels = c("1-10","10-100","100-500","500-1000","1000-2000","2000-5000","5000-10000"))
    
    pal=colorFactor(palette = c("yellow","green","orange","red","brown","black","white"),domain = iri$rangecase)
    
    leaflet(data = iri) %>%
      
      addTiles() %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      addProviderTiles(providers$OpenStreetMap, group = "OSM") %>%
      addLayersControl(baseGroups = c( "Toner Lite","OSM")) %>%
      setView(lng=80.65167,lat =7.86,zoom = 7) %>%
      #addMarkers(lng= ~LON,lat = ~ LAT)
      addCircleMarkers(lng= ~LON,lat = ~ LAT,
                       color =~pal(Total_positive_cases),
                       radius = ~sqrt(totalcase)*1,
                       label = paste("",iri$District,"POSITIVE:",iri$totalcase))%>%
      addLegend(position = "bottomright",pal = pal,values=~Total_positive_cases)
    
  })
  output$displot<-renderPlotly({
    tsc<-covid19.data(case='ts-confirmed')
    tsc<-tsc %>% filter(Country.Region=="Sri Lanka")
    tsc<-data.frame(t(tsc))
    tsc<-cbind(rownames(tsc),data.frame(tsc,row.names =NULL))
    colnames(tsc)<- c('Date','Confirmed')
    tsc<-tsc[-c(1:250),]
    tsc$Date<-ymd(tsc$Date)
    str(tsc)
    tsc$Confirmed<-as.numeric(tsc$Confirmed)
    #plot
    qplot(Date,Confirmed,data=tsc,
          main='Covid-19 confirmed cases in sri lanka')
    ds<-tsc$Date
    y<-tsc$Confirmed
    df<-data.frame(ds,y)
    #Forecasting
    m<-prophet(df)
    #Prediction
    future<-make_future_dataframe(m,periods = 20)
    forecast<-predict(m,future)
    #plot
    plot(m,forecast,ylab='Total predicted cases',xlab='Date')
    #dyplot.prophet(m,forecast)
    
  })
  output$brandex_plot<-renderPlotly({
    cases<-read.csv(text = getURL("https://raw.githubusercontent.com/ishancoderr/Covid_19_sri_lanka/main/confirmed_cases1.csv"))
    cases_new<-read.csv(text=getURL("https://raw.githubusercontent.com/ishancoderr/Covid_19_sri_lanka/main/confirmed_cases.csv"))
    
    ##############################################################################
    #data preparation-brandix cluster
    minuwangoda_clust<-cases[1:35,]
    minuwangoda_clust<-minuwangoda_clust[-c(2:34),]
    minuwangoda_clust<-data.frame(t(minuwangoda_clust))
    minuwangoda_clust<-minuwangoda_clust[-c(1),]
    minuwangoda_clust<-cbind(rownames(minuwangoda_clust),data.frame(minuwangoda_clust, row.names = NULL))
    minuwangoda_clust<-minuwangoda_clust[-1]
    colnames(minuwangoda_clust)<-c('Date','Confirmed')
    
    #convert data formats
    minuwangoda_clust$Confirmed<-as.numeric(minuwangoda_clust$Confirmed)
    minuwangoda_clust$Date<-as.Date(minuwangoda_clust$Date, format = "%m/%d/%y")
    
    #quick plot of data
    qplot(Date,Confirmed,data = minuwangoda_clust,
          main = 'Covid 19 confirmed cases in Brandix cluster')
    
    #forecasting
    ds<-minuwangoda_clust$Date
    y<-minuwangoda_clust$Confirmed
    df<-data.frame(ds, y)
    m<-prophet(df)
    
    #prediction
    future<-make_future_dataframe(m, periods = 20)
    forecast<-predict(m,future)
    
    #plot forecast
    #dyplot.prophet(m,forecast)
    
    #forecast components(trends/weekly pattern)
    plot(m,forecast,ylab='Total predicted cases',xlab='Date')
  })
  output$overseas_cluster<-renderPlotly({
    cases<-read.csv(text = getURL("https://raw.githubusercontent.com/ishancoderr/Covid_19_sri_lanka/main/confirmed_cases1.csv"))
    cases_new<-read.csv(text=getURL("https://raw.githubusercontent.com/ishancoderr/Covid_19_sri_lanka/main/confirmed_cases.csv"))
    overseas_clust<-cases[1:28,]
    overseas_clust<-overseas_clust[-c(2:27),]
    overseas_clust<-data.frame(t(overseas_clust))
    overseas_clust<-overseas_clust[-c(1),]
    overseas_clust<-cbind(rownames(overseas_clust),data.frame(overseas_clust, row.names = NULL))
    overseas_clust<-overseas_clust[-1]
    colnames(overseas_clust)<-c('Date','Confirmed')
    overseas_clust<-overseas_clust[-c(1),]
    #convert data formats
    overseas_clust$Confirmed<-as.numeric(overseas_clust$Confirmed)
    overseas_clust$Date<-as.Date(overseas_clust$Date, format = "%m/%d/%y")
    
    #forecasting
    ds<-overseas_clust$Date
    y<-overseas_clust$Confirmed
    df<-data.frame(ds,y)
    m<-prophet(df)
    
    #prediction
    future<-make_future_dataframe(m, periods = 20)
    forecast<-predict(m,future)
    
    
    
    #forecast components(trends/weekly pattern)
    plot(m,forecast)
  })
} # server
# Create Shiny object
shinyApp(ui = ui, server = server)