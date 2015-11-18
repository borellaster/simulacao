path <- "/Users/pba/github/simulacao/FortranRIntegration"
setwd(path)
library(RCurl)
library(rjson)
library(FortranRIntegration)
library(shiny)


#load the data
w <- getWeatherDataFromTxt(paste(path,"/data/", sep = ""))
i <- getIrrigDataFromTxt(paste(path,"/data/", sep = ""))
soil <- getSoilDataFromTxt(paste(path,"/data/", sep = ""))
plant <- getPlantDataFromTxt(paste(path,"/data/", sep = ""))

#collect stations
dataCurl <- getURLContent("http://dev.sisalert.com.br/apirest/api/v1/stations")
json <- fromJSON(dataCurl)
dataStation <- do.call(rbind, lapply(json, function(x) data.frame(x)))

#change de colnames
colnames(dataStation)[1] <- "ID"
colnames(dataStation)[2] <- "Name"
dataStation$Name <- as.character(dataStation$Name)
dataStation$ID <- as.character(dataStation$ID)

dataStation$url <- 
  paste0(
    "http://dev.sisalert.com.br/apirest/api/v1/data/station/model/", 
    dataStation$ID, "/range/01-01-2013/01-01-2016")

shinyServer(function(input, output, session) {
  output$ui <- renderUI({
    sidebarPanel(
      selectInput(inputId = "cbxStations",
                  label = "Escolher a estação",
                  choices = dataStation$url,
                  selectize = TRUE),
      
      actionButton("goSimulation", "Rodar Modelo")
    )})
  
  getSelectionKey <- function() {
    return(as.character(input$cbxStations))
    #return(as.character(subset(dataStation, Name == input$cbxStations)$url))
  }
  
  observeEvent(input$goSimulation, {
    rodarSimulacao(getSelectionKey())
  })
  
  rodarSimulacao <- function(url){
    
    #url <- paste("http://dev.sisalert.com.br/apirest/api/v1/data/station/model/",
    #             input$cbxStations[1], "/range/01-01-2013/01-01-2016", sep="")
    
    dataCurl <- getURLContent(url, ssl.verifypeer = FALSE)
    jsonWeather <- fromJSON(dataCurl)
    weatherData <- do.call(rbind, lapply(jsonWeather, function(x) data.frame(x)))
    
    ##
    weatherData$date <- as.character(weatherData$date)
    weatherData <- weatherData[order(weatherData$date,decreasing = FALSE),]
    row.names(weatherData) <- NULL
    weatherData <- data.frame('date'=weatherData$date,'srad'=weatherData$srad,'tmax'=weatherData$tmax,'tmin'=weatherData$tmin,'rain'=weatherData$rain,'par'=weatherData$par)
    ##
    
    runSimulation(weather = weatherData,plant = plant,soil = soil,irrig = i,doyp = 1, frop = 1)
    
    output$tableResultsPlant <- renderDataTable({
      data <- read.table("plant.out",skip = 9)
    })
    
    output$tableResultsSoil <- renderDataTable({
      data <- read.table("sw.out",skip = 6)
    })
    
    output$tableFinal <- renderDataTable({
      data <- read.table("WBAL.OUT",skip = 4, sep = ":")
    })
    
    output$tableResultsStation <- renderDataTable({
      data <- dataStation
    })
  }
  
})
