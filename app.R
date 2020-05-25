#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
require(lubridate)
require(RCurl)
library(dygraphs)
library(xts)
library(lubridate)
library(mapview)
library(sf)
library(leaflet)
library(DT)


load("countries.rda")
library(WDI)
wd <- WDI(country="all", indicator=c( "SP.POP.TOTL", "SP.POP.65UP.TO.ZS"), start=2018, end=2018, extra = TRUE)
wd<-wd[c(1,4,5)]
names(wd)[2:3] <-c("pop","p_over_65")

dt<-function(d) {DT::datatable(d, 
                           filter = "top",                         
                           extensions = c('Buttons'), options = list(
                             dom = 'Blfrtip',
                             buttons = c('copy', 'csv', 'excel'), colReorder = TRUE
                           ))}


jh_data <- function(){
  
  URL <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
  data <- read.csv(text = URL, check.names = F)
  
  pivot_longer(data,cols=5:dim(data)[2],names_to = "Date") ->d
  names(d)<-c("Province","Country","Lat","Long","Date","NCases")
  d$Date<-as.Date(d$Date,format="%m/%d/%y")
  
  Confirmed<-d
  
  URL <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
  data <- read.csv(text = URL, check.names = F)
  
  pivot_longer(data,cols=5:dim(data)[2],names_to = "Date") ->d
  names(d)<-c("Province","Country","Lat","Long","Date","NCases")
  
  d$Date<-as.Date(d$Date,format="%m/%d/%y")
  
  Deaths<-d
  
  URL <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
  data <- read.csv(text = URL, check.names = F)
  
  pivot_longer(data,cols=5:dim(data)[2],names_to = "Date") ->d
  names(d)<-c("Province","Country","Lat","Long","Date","NCases")
  
  d$Date<-as.Date(d$Date,format="%m/%d/%y")
  Recovered<-d
  
  
  Confirmed %>% group_by(Country, Date) %>% summarise(NCases=sum(NCases)) -> confirmed_country
  Deaths %>% group_by(Country, Date) %>% summarise(NDeaths=sum(NCases)) -> deaths_country
  Recovered %>% group_by(Country, Date) %>% summarise(NRecovered=sum(NCases)) -> recovered_country
  
  confirmed_country %>% left_join(deaths_country,by = c("Country", "Date")) %>% left_join(recovered_country, by = c("Country", "Date")) -> by_country
  by_country%>%arrange(Date) %>% mutate(New_cases = NCases - lag(NCases, default = first(NCases)), NActive=NCases-NDeaths-NRecovered) -> by_country
  by_country
}


df<-jh_data()

#save(df,file=sprintf("df%s.rda",Sys.Date() ))
df %>% mutate(Daily_deaths = NDeaths - lag(NDeaths, default = first(NDeaths))) %>% arrange(NDeaths) -> df

df %>% group_by(Country) %>% summarise( max=max(NDeaths)) %>% arrange(-max) %>% mutate(Country = factor(Country, Country)) -> tmp
c_options<-levels(tmp$Country)

df %>% mutate(Daily_deaths = NDeaths - lag(NDeaths, default = first(NDeaths))) %>% arrange(Daily_deaths) -> df
load("jh_iso.rda")
jh_iso$iso2c[jh_iso$Country=="Serbia"] <-"CS"
jh_iso$iso2c[jh_iso$Country=="Holy See"] <-NA
jh_iso$iso2c[jh_iso$Country=="France"]<-"FR"
jh_iso$iso2c[jh_iso$Country=="Canada"]<-"CA"
df<-merge(df,jh_iso)
df<-merge(wd,df)
df$p_over_65<-round(df$p_over_65,1)

library(zoo)
df %>% mutate (pop_over_65 = round(pop*p_over_65/100,1)) %>% mutate(NDeathsp65 = round( NDeaths/(pop_over_65/100000),0)) %>% arrange(Country,Date) %>% mutate(deaths_7day_mean=round(rollapply(Daily_deaths,7,mean,align='right',fill=NA),1)) ->df
d<-df
df2<-df[,c(1,4,5,6,7,9,10,11,12,13, 14)] 

df2 %>% filter(Date==max(Date)) ->dd
library(RColorBrewer)
pal1<-brewer.pal(8, "YlOrRd")
pal2<-brewer.pal(8, "YlGnBu")[8:1]
pal3<-brewer.pal(8, "YlOrRd")[8:1]
map_data<-merge(countries, dd)



# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Data from Johns Hopkins"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        h4("Select a country from the list"),
        h5("If the country does not appear in the drop down menu press backspace and type a few letters to find it."),
        h6("The figures show the running average of the reported daily incidence of death with Covid or confirmed new case."),
        selectInput("country", "Country", c_options, selected = 'United Kingdom', multiple = FALSE,
                    selectize = TRUE, width = NULL, size = NULL),
        sliderInput("ndays", "Number of days for running average:",
                    min = 1, max = 20, value =7, step=1
        ),
        checkboxInput("lg", "Logged y axis", value = FALSE, width = NULL),
        
        sliderInput("sdate",
                    "Start date:",
                    min = as.Date("2020-01-01","%Y-%m-%d"),
                    max = as.Date("2020-04-01","%Y-%m-%d"),
                    value=as.Date("2020-03-01"),
                    timeFormat="%Y-%m-%d")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Deaths",  dygraphOutput("deaths")),
                    tabPanel("Cases",  dygraphOutput("cases")),
                    tabPanel("Cases per 10k", h6(" According to one definition used by th weekly Returns Service (WRS) of the Royal College of General Practitioners in the UK,
                                                 
                                                 An epidemic is declared if the rate of consultations for influenza-like symptoms in a sample of reporting by general practice exceeds 40 per 10,000 population in one week."), 
                             dygraphOutput("cases10k"),
                             dygraphOutput("cumcases10k")),
                    
                    tabPanel("Deaths per 10k of the over 65s", 
                             dygraphOutput("deaths10k"),
                             dygraphOutput("cumdeaths10k")),
                    
                    
                    tabPanel("Time series for country",  DTOutput('data')),
                    tabPanel("Current data for all countries",  DTOutput('data2')),
                    tabPanel("Impact map",   
                             h6("The impact is measured as the total deaths to date divided by the number of people 65 or over in the population of the country"),
                             h6("The units are registered Covid deaths to date per 100 thousand people over 65"),
                             leafletOutput ('map1')))
        
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$deaths <- renderDygraph({
     countries<-input$country
     ndays<-input$ndays
     lscale<-input$lg
     sdate<-input$sdate
     d %>% filter(Country %in% countries) %>% arrange(Date) %>% filter(Date > sdate ) ->dd
     d_xts<-xts(x = dd$Daily_deaths, order.by = dd$Date)
     d_xts %>% dygraph(group = "Country")  %>%  dyRoller(rollPeriod = ndays) %>% dyRangeSelector() %>% dyAxis(name="y",logscale=lscale)
   })
   
   output$cases <- renderDygraph({
     countries<-input$country
     ndays<-input$ndays
     lscale<-input$lg
     sdate<-input$sdate
     d %>% filter(Country %in% countries) %>% arrange(Date) %>% filter(Date > sdate ) ->dd
     d_xts<-xts(x = dd$New_cases, order.by = dd$Date)
     d_xts %>% dygraph(group = "Country")  %>%  dyRoller(rollPeriod = ndays) %>% dyRangeSelector() %>% dyAxis(name="y",logscale=lscale)
   })
   
   output$cases10k <- renderDygraph({
     countries<-input$country
     ndays<-input$ndays
     lscale<-input$lg
     sdate<-input$sdate
     d %>% filter(Country %in% countries) %>% arrange(Date) %>% filter(Date > sdate ) ->dd
     d_xts<-xts(x = (dd$New_cases/(dd$pop/10000))*7, order.by = dd$Date)
     d_xts %>% dygraph(group = "Country",ylab = "Weekly cases per 10k of the population" )  %>%  dyRoller(rollPeriod = ndays) %>% dyRangeSelector() %>% dyAxis(name="y",logscale=lscale)
   })
   
   output$cumcases10k <- renderDygraph({
     countries<-input$country
     ndays<-input$ndays
     lscale<-input$lg
     sdate<-input$sdate
     d %>% filter(Country %in% countries) %>% arrange(Date) %>% filter(Date > sdate ) ->dd
     d_xts<-xts(x = (dd$NCases/(dd$pop/10000))*7, order.by = dd$Date)
     d_xts %>% dygraph(group = "Country",ylab = "Weekly cases per 10k of the population" )  %>%  dyRoller(rollPeriod = ndays) %>% dyRangeSelector() %>% dyAxis(name="y",logscale=lscale)
   })
   
   output$deaths10k <- renderDygraph({
     countries<-input$country
     ndays<-input$ndays
     lscale<-input$lg
     sdate<-input$sdate
     
     d %>% filter(Country %in% countries) %>% arrange(Date) %>% filter(Date > sdate ) ->dd
     d_xts<-xts(x = (dd$Daily_deaths/(dd$pop_over_65/10000))*7, order.by = dd$Date)
     d_xts %>% dygraph(group = "Country",ylab = "Weekly deaths per 10k of the population over 65" )  %>%  dyRoller(rollPeriod = ndays) %>% dyRangeSelector() %>% dyAxis(name="y",logscale=lscale)
   })
   
   output$cumdeaths10k <- renderDygraph({
     countries<-input$country
     ndays<-input$ndays
     lscale<-input$lg
     sdate<-input$sdate
     
     d %>% filter(Country %in% countries) %>% arrange(Date) %>% filter(Date > sdate ) ->dd
     d_xts<-xts(x = (dd$NDeaths/(dd$pop_over_65/10000))*7, order.by = dd$Date)
     d_xts %>% dygraph(group = "Country",ylab = "Cumulative deaths per 10k of the population over 65" )  %>%  dyRoller(rollPeriod = ndays) %>% dyRangeSelector() %>% dyAxis(name="y",logscale=lscale)
   })
   
   
   output$data = renderDT({
     countries<-input$country
     d %>% filter(Country %in% countries) %>% arrange(desc(Date)) %>% filter(Date > as.Date("2020-03-01") ) ->dd
     dt(dd)
   })
     
   
   output$data2 =renderDT({
     df2 %>% filter(Date==max(Date)) %>% 
       dt()
   })
   
   
   output$map1<-renderLeaflet({
    m<- mapview(map_data,zcol="NDeathsp65", at =c(0,10,20,100,150,200,250,300,500),legend=TRUE,col.regions = pal1) 
    m@map
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

