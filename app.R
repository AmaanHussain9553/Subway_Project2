#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#List of all libraries used
library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(rlang)
library(plyr)
library(dplyr)
library(tidyr)

# Reading in the main file and breaking it into chunks of 4.8 MB
# Only run this code once
my_file <- read.csv("CTAData.csv")
my_file <- subset(my_file, mdy(my_file$date) >= mdy("08/23/2021"))
# print(my_file)
my_file2 <- read.csv("CTADataWLoc.csv")
names(my_file2)[names(my_file2) == "MAP_ID"] <- "station_id"
# # # print(my_file2)
combined <- merge(x=my_file, y=my_file2, by=c("station_id"), all.x=T)
# print(combined)
# # combined_data <- merge(x = my_file, y= my_file2, by=c(""))
# 
possible_dates <- unique(combined$date)
# print(possible_dates)
# print(combined$date)

map_backgrounds <- c(providers$OpenStreetMap, providers$Esri.WorldTopoMap, providers$Esri.NatGeoWorldMap)

print(class(combined$Location))
locData <- read.table(text=gsub("[()]", "", combined$Location), header=FALSE, sep=",")
combined$Latitude <- locData$V1
combined$Longitude <- locData$V2
combined$Latitude <- as.numeric(combined$Latitude)
combined$Latitude <- as.numeric(combined$Latitude)
combined$stationname <- sort(combined$stationname)
print(combined)

date_count = 0;

latlongData <- combined[, c('stationname', 'Latitude', 'Longitude', 'date')]


#delete the main CTA_Data file and bind all the 
#other CSV files together
temp = list.files(pattern="*.csv")
allData2 <- lapply(temp, read.csv)
allData <- rbind.fill(allData2)







#converting data to internal format 
allData$newDate <- as.Date(allData$date, "%m/%d/%Y")

#creating new column for month and day of the week
allData$month <- floor_date(allData$newDate, "month")
allData$day <- weekdays(as.Date(allData$newDate))

#debug print to ensure all data is loaded well
#print(allData)

#removing the old date column
allData$Date <- NULL

#Creating a new grouped data table for monthly 
#aggregated stats
monthlyGroup <- allData %>%                        
  group_by(month) %>% 
  dplyr::summarize(No_of_Rides = mean(rides), station_name = stationname) %>% 
  as.data.frame()

#Creating a new grouped data table for 
#aggregated stats based on day of the week
weeklyGroup <- allData %>%                        
  group_by(day) %>% 
  dplyr::summarize(No_of_Rides = mean(rides), station_name = stationname, date = newDate) %>% 
  as.data.frame()

#debug print statements to look at new grouped data tables
#print(monthlyGroup)
#print(weeklyGroup)

#list to hold the options for years and stations
years<-c(2001:2021)
location<-unique(allData$stationname)
locationOrdered <- sort(location)




# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Project 1"),
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   
                   sidebarMenu(
                   menuItem("Date-Specific", tabName = "Date-Specific", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL)),
                   selectInput("Year", "Select the date to view data for (Left Side)", possible_dates, selected = mdy("08/23/2021")),
                   selectInput("Year1", "Select the date to view data for (Right Side)", possible_dates, selected = mdy("08/23/2021")),
                   selectInput("Map", "Select Map Background", map_backgrounds, selected = providers$OpenStreetMap),
                   selectInput("Order", "Select Chart Display Max/Min or Alphabetical", c("Alphabetical" , "Max/Min"), selected = "Alphabetical"),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   selectInput("Year12", "Select the year for UIC-Halsted", years, selected = 2021),
                   selectInput("Loc12", "Select location to display", locationOrdered, selected = "UIC-Halsted")
  ),
  dashboardBody(
    tabItems(
        tabItem(tabName="Date-Specific",
            fluidRow(
              column(8,
                fluidRow(
                    box(title = "Daily data of Rides per day", solidHeader = TRUE, status = "primary", width = 12,background = "orange",
                    plotOutput("hist1", height = 900))
                ),
                fluidRow(
                  box(title = "Daily data of Rides per day", solidHeader = TRUE, status = "primary", width = 12,background = "lime",
                      plotOutput("hist2", height = 900))
                )
              ),
              column(1,
                    fluidRow(
                       box( title = "Daily data of Rides per day", solidHeader = TRUE, status = "primary", width = 12,background = "orange",
                            dataTableOutput("tab1", height = 800))
                     ),
                     fluidRow(
                       box( title = "Daily data of Rides per day", solidHeader = TRUE, status = "primary", width = 12,background = "lime",
                            dataTableOutput("tab2", height = 800))
                     )
              ),
              column(2,
                     fluidRow(
                       box(title = "Daily data of Rides per day", solidHeader = TRUE, status = "primary", width = 12,
                           plotOutput("hist12", height = 550)
                       )
                     ),
                     
                     
                     fluidRow(
                       box(title = "Monthly data (mean) of Rides per day", solidHeader = TRUE, status = "primary", width = 12,
                           plotOutput("hist22", height = 550)
                       )
                     ),
                     
                     fluidRow(
                       box(title = "Number of rides based on days of the week", solidHeader = TRUE, status = "primary", width = 12,
                           plotOutput("hist32", height = 550)
                       )
                     ),
                     
              ),
              column(1,
                     fluidRow(
                       box( title = "Daily data of Rides per dayr", solidHeader = TRUE, status = "primary", width = 12,
                            dataTableOutput("tab12", height = 800)
                       )
                     ), 
                     fluidRow(
                       box( title = "Monthly data (mean) of Rides per day", solidHeader = TRUE, status = "primary", width = 12,
                            dataTableOutput("tab22", height = 350)
                       )
                     ),
                     fluidRow(
                       box( title = "Number of rides based on days of the week", solidHeader = TRUE, status = "primary", width = 12,
                            dataTableOutput("tab32", height = 280)
                       )
                     )
              )
              ,
            column(7,
                   fluidRow(
                     box(title = "Leaflet Map", solidHeader = TRUE, status = "primary", width = 12,
                         leafletOutput("leaf", height = 1080))
                   )
            )
              
            )
        )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # increase the default font size
  theme_set(theme_grey(base_size = 14))
  #creating reactive objects that can look at data yearly, monthly, and based on day of the week
  #it is reactive so that it may change as the sidebar options change
  justOneYearReactive <- reactive({subset(allData, year(allData$newDate) == input$Year12 & allData$stationname == input$Loc12)})
  monthlyReactive <- reactive({subset(monthlyGroup, year(monthlyGroup$month) == input$Year12 & monthlyGroup$station_name == input$Loc12)})
  weeklyReactive <- reactive({subset(weeklyGroup, year(weeklyGroup$date) == input$Year12 & weeklyGroup$station_name == input$Loc12)})
  
  #daily bar plot based on the year
  output$hist12 <- renderPlot({
    justOneYear = justOneYearReactive()
    ggplot(justOneYear, aes(x=newDate, y=rides)) +
      labs(x=paste("Day in", input$Year12), y = "No. Rides") + geom_bar(stat="identity", fill="steelblue") + ylim(0,10000) +
      scale_x_date(date_breaks = "1 month", date_labels =  "%b", expand = c(0, 0)) + scale_y_continuous()
  })
  
  #monthly bar plot based on the year
  output$hist22 <- renderPlot({
    monthly <- monthlyReactive()
    ggplot(monthly, aes(x=month, y=No_of_Rides)) +
      labs(x=paste("Month in", input$Year12), y = "No. Rides") + geom_bar(stat="identity", fill="steelblue") + ylim(0,10000) +
      scale_x_date(date_breaks = "1 month", date_labels =  "%b", expand = c(0, 0)) + scale_y_continuous()
  })
  
  #day of the week bar plot based on the year
  output$hist32 <- renderPlot({
    weekly <- weeklyReactive()
    ggplot(weekly, aes(x=day, y=No_of_Rides)) + labs(x=paste("Month in", input$Year), y = "No. Rides") + 
      geom_bar(stat="identity", fill="steelblue") + ylim(0,10000) + scale_y_continuous()
  })
  
  #daily data table based on the year
  output$tab12 <- DT::renderDataTable(
    DT::datatable({
      justOneYear <- justOneYearReactive()
      
      unique.data.frame(justOneYear[, c(Date = "newDate", Rides = "rides")])
    }, 
    options = list(searching = FALSE, pageLength = 20, lengthChange = FALSE
    ), rownames = FALSE 
    )
  )
  
  #monthly data table based on the year
  output$tab22 <- DT::renderDataTable(
    DT::datatable({
      monthly <- monthlyReactive()
      
      unique.data.frame(monthly[, c(Month = "month",Rides = "No_of_Rides")])
    }, 
    options = list(searching = FALSE, pageLength = 12, lengthChange = FALSE
    ), rownames = FALSE 
    )
  )
  
  #day of the week data table based on the year
  output$tab32 <- DT::renderDataTable(
    DT::datatable({
      weekly <- weeklyReactive()
      
      unique.data.frame(weekly[, c(Day_of_week = "day",Rides = "No_of_Rides")])
    }, 
    options = list(searching = FALSE, pageLength = 7, lengthChange = FALSE
    ), rownames = FALSE 
    )
  )
  dailyReactive <- reactive({subset(combined, mdy(combined$date) == mdy(input$Year))})
  dailyLeafletReactive <- reactive({subset(latlongData, mdy(latlongData$date) == mdy(input$Year))})
  dailyReactive1 <- reactive({subset(combined, mdy(combined$date) == mdy(input$Year1))})
  dailyLeafletReactive1 <- reactive({subset(latlongData, mdy(latlongData$date) == mdy(input$Year1))})
  backgroundReactive <- reactive({input$Map})
  chartTypeReactive <- reactive({input$Order})

  #daily bar plot based on the year
  output$hist1 <- renderPlot({
    dailyData <- dailyReactive()
    if(input$Order == "Alphabetical"){
    ggplot(dailyData, aes(x=sort(stationname), y=rides)) +
      labs(x=paste(input$Year), y = "No. Rides") + geom_bar(stat="identity", fill="steelblue")
    }
    else if(input$Order == "Max/Min"){
      ggplot(dailyData, aes(x=reorder(stationname, -rides), y=rides)) +
        labs(x=paste(input$Year), y = "No. Rides") + geom_bar(stat="identity", fill="steelblue")
    }
  })
  
  output$leaf <- renderLeaflet({
    dailyLeaflet = dailyLeafletReactive()
    map <- leaflet() %>% addMarkers(data=dailyLeaflet, lng = ~Longitude, lat = ~Latitude, popup = ~as.character(stationname), icon = list(
      iconUrl = 'https://icons.iconarchive.com/icons/icons8/windows-8/72/City-Railway-Station-icon.png',
      iconSize = c(80, 80)
    )) %>% 
      addTiles() %>%  setView(lng = -87.647998, lat = 41.870, zoom = 11)
    if(input$Map ==providers$OpenStreetMap){
      backGroundMap = backgroundReactive()
      leafletProxy(mapId = "leaf") %>% addProviderTiles(backGroundMap)
    }
    else if(input$Map == providers$Esri.WorldTopoMap){
      backGroundMap = backgroundReactive()
      leafletProxy(mapId = "leaf") %>% addProviderTiles(backGroundMap)
    }
    else if(input$Map == providers$Esri.NatGeoWorldMap){
      backGroundMap = backgroundReactive()
      leafletProxy(mapId = "leaf") %>% addProviderTiles(backGroundMap)
    }
    map
  })
  
  #daily data table based on the year
  output$tab1 <- DT::renderDataTable(
    DT::datatable({
      dailyData <- dailyReactive()
      unique.data.frame(dailyData[, c(Station_Name = "stationname", Rides = "rides")])
    }, 
    options = list(searching = FALSE, pageLength = 23, lengthChange = FALSE
    ), rownames = FALSE 
    )
  )
  
  output$hist2 <- renderPlot({
    dailyData <- dailyReactive1()
    if(input$Order == "Alphabetical"){
      ggplot(dailyData, aes(x=sort(stationname), y=rides)) +
        labs(x=paste(input$Year), y = "No. Rides") + geom_bar(stat="identity", fill="steelblue")
    }
    else if(input$Order == "Max/Min"){
      ggplot(dailyData, aes(x=reorder(stationname, -rides), y=rides)) +
        labs(x=paste(input$Year), y = "No. Rides") + geom_bar(stat="identity", fill="steelblue")
    }
  })
  
  output$leaf2 <- renderLeaflet({
    dailyLeaflet = dailyLeafletReactive1()
    map <- leaflet() %>% addMarkers(data=dailyLeaflet, lng = ~Longitude, lat = ~Latitude, popup = ~as.character(stationname), icon = list(
      iconUrl = 'https://icons.iconarchive.com/icons/icons8/windows-8/72/City-Railway-Station-icon.png',
      iconSize = c(20, 20)
    )) %>% 
      addTiles() %>%  setView(lng = -87.647998, lat = 41.870, zoom = 11)
    if(input$Map ==providers$OpenStreetMap){
      backGroundMap = backgroundReactive()
      leafletProxy(mapId = "leaf") %>% addProviderTiles(backGroundMap)
    }
    else if(input$Map == providers$Esri.WorldTopoMap){
      backGroundMap = backgroundReactive()
      leafletProxy(mapId = "leaf") %>% addProviderTiles(backGroundMap)
    }
    else if(input$Map == providers$Esri.NatGeoWorldMap){
      backGroundMap = backgroundReactive()
      leafletProxy(mapId = "leaf") %>% addProviderTiles(backGroundMap)
    }
    map
  })
  
  #daily data table based on the year
  output$tab2 <- DT::renderDataTable(
    DT::datatable({
      dailyData <- dailyReactive1()
      unique.data.frame(dailyData[, c(Station_Name = "stationname", Rides = "rides")])
    }, 
    options = list(searching = FALSE, pageLength = 23, lengthChange = FALSE
    ), rownames = FALSE 
    )
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
