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

runSimulation(weather = w,plant = plant,soil = soil,irrig = i,doyp = 1, frop = 1)

#collect data
plantOut <- read.table("plant.out",skip = 9)
swOut <- read.table("sw.out",skip = 9)
wbalOut <- read.table("WBAL.OUT",skip = 4, sep = ":")
#change de colnames
colnames(wbalOut)[1] <- "Description"
colnames(wbalOut)[2] <- "Values"

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
    sidebarPanel(
      selectInput(inputId = "cbxStations", 
                  label = "Escolher a estação",
                  choices = c(dataStation),
                  selectize = TRUE)
    )
  })
  
  output$table <- renderDataTable({
    data <- wbalOut
  })
  
  output$stationsTable <- renderDataTable({
    stationsTable <- dataStation
  })
  
  output$stations <- renderText({
    stations <- dataStation
    paste(stations$name)
  })

})