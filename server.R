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

#shiny server body
shinyServer(function(input, output, session) { 
  output$ui <- renderUI({
    sidebarPanel(
      selectInput(inputId = "cbxSoil", 
                  label = "Escolher o Solo",
                  choices = c("Solo 1", "Solo 2", "Solo 3"),
                  selectize = FALSE)
    )
  })
  
  output$table <- renderDataTable({
    data <- wbalOut
  })

})