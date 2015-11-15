setwd("/Users/pba/Simulacao/FortranRIntegration")
library(RCurl)
library(rjson)
library(FortranRIntegration)
library(shiny)


#load the data
w <- getWeatherDataFromTxt("/Users/pba/Simulacao/FortranRIntegration/data/")
i <- getIrrigDataFromTxt("/Users/pba/Simulacao/FortranRIntegration/data/")
soil <- getSoilDataFromTxt("/Users/pba/Simulacao/FortranRIntegration/data/")
plant <- getPlantDataFromTxt("/Users/pba/Simulacao/FortranRIntegration/data/")

dataCurl <- getURLContent("http://dev.sisalert.com.br/apirest/api/v1/data/station/model/5602a58c92c884831962ceeb/range/01-01-2013/01-01-2016", ssl.verifypeer = FALSE)
jsonWeather <- fromJSON(dataCurl)
weatherData <- do.call(rbind, lapply(jsonWeather, function(x) data.frame(x)))

##
weatherData$date <- as.character(weatherData$date)
weatherData <- weatherData[order(weatherData$date,decreasing = FALSE),]
row.names(weatherData) <- NULL
weatherData <- data.frame('date'=weatherData$date,'srad'=weatherData$srad,'tmax'=weatherData$tmax,'tmin'=weatherData$tmin,'rain'=weatherData$rain,'par'=weatherData$par)
##

runSimulation(weather = weatherData,plant = plant,soil = soil,irrig = i,doyp = 1, frop = 1)

#collect data
plantOut <- read.table("plant.out",skip = 9)
swOut <- read.table("sw.out",skip = 6)
wbalOut <- read.table("WBAL.OUT",skip = 4, sep = ":")
#change de colnames for wbalOut
colnames(wbalOut)[1] <- "Description"
colnames(wbalOut)[2] <- "Values"

#change de colnames for swOut
colnames(swOut)[1] <- "Dia do ano"


#collect stations
dataCurl <- getURLContent(paste("http://dev.sisalert.com.br/apirest/api/v1/stations",sep = ""))
json <- fromJSON(dataCurl)
dataStation <- do.call(rbind, lapply(json, function(x) data.frame(x)))
#change de colnames
colnames(dataStation)[1] <- "ID"
colnames(dataStation)[2] <- "Name"


#shiny server body
shinyServer(function(input, output, session) { 
  output$ui <- renderUI({
    sidebarPanel(width = 100,
      selectInput(inputId = "cbxStations", 
                  label = "Escolher a estação",
                  choices = c(dataStation),
                  selectize = TRUE)
    )
  })
  
  output$tableFinal <- renderDataTable({
    data <- wbalOut
  })
  
  output$tableResults <- renderDataTable({
    data <- swOut
  })
  
  output$stationsTable <- renderDataTable({
    stationsTable <- dataStation
  })
  
  output$stations <- renderText({
    stations <- dataStation
    paste(stations$name)
  })

})