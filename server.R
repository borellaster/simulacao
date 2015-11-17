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
dataCurl <- getURLContent(paste("http://dev.sisalert.com.br/apirest/api/v1/stations",sep = ""))
json <- fromJSON(dataCurl)
dataStation <- do.call(rbind, lapply(json, function(x) data.frame(x)))
station <- dataStation$X_id[1]

#change de colnames
colnames(dataStation)[1] <- "ID"
colnames(dataStation)[2] <- "Name"
url <- "http://dev.sisalert.com.br/apirest/api/v1/data/station/model/5602a58c92c884831962ceeb/range/01-01-2013/01-01-2016"
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

#collect data
plantOut <- read.table("plant.out",skip = 9)
swOut <- read.table("sw.out",skip = 6)
wbalOut <- read.table("WBAL.OUT",skip = 4, sep = ":")

#change the colnames for plantOut
colnames(plantOut) <- c("Dia do Ano", "Número de Folhas", "Acum.Temp. Reprod. (oC)", "Peso da Planta (g/m2)",
                        "Peso do Docel (g/m2)", "Peso da Raiz (g/m2)", "Peso da Fruta (g/m2)", 
                        "Ind. Area Foliar (m2/m2)")

#change the colnames for wbalOut
colnames(wbalOut) <- c("Descrição", "Valores")

#change the colnames for swOut
colnames(swOut) <- c("Dia do ano", "Rad. Solar(MJ/m2)", "Temp. Max(oC)", "Temp. Min(oC)", "Chuva(mm)",
                     "Irrig.(mm)", "Escoamento(mm)", "Infil.(mm)", "Drenagem(mm)", "Evapo. Transp(mm)",
                     "Evapo. Solo(mm)", "Evapo. Planta(mm)", "Agua no solo(mm)", "Agua no solo(mm3/mm3)",
                     "Estresse hídrico", "Excesso de estresse hídrico")

dataStation$choice <- paste("\"", dataStation$Name, "\"=\"", dataStation$ID, "\"",sep="")
dataStation$Name <- as.character(dataStation$Name)
dataStation$ID <- as.character(dataStation$ID)


#shiny server body
shinyServer(function(input, output, session) { 
  output$ui <- renderUI({
    sidebarPanel(
      selectInput(inputId = "cbxStations",
                  label = "Escolher a estação",
                  choices = dataStation$Name,
                  selectize = TRUE),
      
      actionButton("runModel", "Rodar Modelo")
    )
  })

  output$tableResultsPlant <- renderDataTable({
    data <- plantOut
  })
  
  output$tableResultsSoil <- renderDataTable({
    data <- swOut
  })
  
  output$tableFinal <- renderDataTable({
    data <- wbalOut
  })
  
  output$tableResultsStation <- renderDataTable({
    data <- dataStation
  })
  
})