path <- "/Users/pba/github/simulacao/FortranRIntegration"
setwd(path)
library(RCurl)
library(rjson)
library(FortranRIntegration)
library(shiny)
library(ggplot2)


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
      h4("Instruções"),
      p("Clique em Rodar Modelo após Escolher a Estação."),
      br(),
      selectInput(inputId = "cbxStations",
                  label = "Escolher a estação",
                  choices = dataStation$Name,
                  selectize = TRUE),
      
      actionButton("goSimulation", "Rodar Modelo"),
      uiOutput("mesg")
    )})
  
  getSelectionKey <- function() {
    #return(as.character(input$cbxStations))
    return(as.character(subset(dataStation, Name == input$cbxStations)$url))
  }
  
  observeEvent(input$goSimulation, {
    output$mesg <- renderText("")
    rodarSimulacao(getSelectionKey())
  })
  
  rodarSimulacao <- function(url){
    
    #url <- paste("http://dev.sisalert.com.br/apirest/api/v1/data/station/model/",
    #             input$cbxStations[1], "/range/01-01-2013/01-01-2016", sep="")
    
    dataCurl <- getURLContent(url, ssl.verifypeer = FALSE)
    jsonWeather <- fromJSON(dataCurl)
    weatherData <- do.call(rbind, lapply(jsonWeather, function(x) data.frame(x)))
    
    # As estações que não possem dados suficientes para uma simulação, ou 365 dias de dados da erro
    if (dim(weatherData)[1] > 365) {
      
      
      ##
      weatherData$date <- as.character(weatherData$date)
      weatherData <- weatherData[order(weatherData$date,decreasing = FALSE),]
      row.names(weatherData) <- NULL
      weatherData <- data.frame('date'=weatherData$date,'srad'=weatherData$srad,'tmax'=weatherData$tmax,'tmin'=weatherData$tmin,'rain'=weatherData$rain,'par'=weatherData$par)
      ##
      
      runSimulation(weather = weatherData,plant = plant,soil = soil,irrig = i,doyp = 1, frop = 1)
      
      output$tableResultsPlant <- renderDataTable({
        
        plantOut <- read.table("plant.out",skip = 9)
        #change the colnames for plantOut
        colnames(plantOut) <- c("Dia do Ano", "Número de Folhas", "Acum.Temp. Reprod. (oC)", "Peso da Planta (g/m2)",
                                "Peso do Docel (g/m2)", "Peso da Raiz (g/m2)", "Peso da Fruta (g/m2)", 
                                "Ind. Area Foliar (m2/m2)")
        data <- plantOut
      })
      
      output$tableResultsSoil <- renderDataTable({
        
        swOut <- read.table("sw.out",skip = 6)
        #change the colnames for swOut
        colnames(swOut) <- c("Dia do ano", "Rad. Solar(MJ/m2)", "Temp. Max(oC)", "Temp. Min(oC)", "Chuva(mm)",
                             "Irrig.(mm)", "Escoamento(mm)", "Infil.(mm)", "Drenagem(mm)", "Evapo. Transp(mm)",
                             "Evapo. Solo(mm)", "Evapo. Planta(mm)", "Agua no solo(mm)", "Agua no solo(mm3/mm3)",
                             "Estresse hídrico", "Excesso de estresse hídrico")
        data <- swOut
      })
      
      output$tableFinal <- renderDataTable({
        
        wbalOut <- read.table("WBAL.OUT",skip = 4, sep = ":")
        #change the colnames for wbalOut
        colnames(wbalOut) <- c("Descrição", "Valores")
        
        data <- wbalOut
      })
      
      output$tableResultsStation <- renderDataTable({
        data <- dataStation
      })
      
      output$distPlot <- renderPlot({
        
        plantOut <- read.table("plant.out",skip = 9)
        
        colnames(plantOut) <- c("doy", "Número de Folhas", "Acum.Temp. Reprod. (oC)", "Peso da Planta (g/m2)",
                                "Peso do Docel (g/m2)", "Peso da Raiz (g/m2)", "Peso da Fruta (g/m2)", 
                                "iaf")
        
        
        ggplot(data = plantOut, aes(x = doy)) +
          geom_line(aes(y = iaf)) +
          labs (title="Índice de Area Foliar X Dia", x="Dia do Ano", y="Ind. Area Foliar (m2/m2)")
      })
    } else {
      output$mesg <- renderUI(p("Estação não possui dados suficientes para uma simulação!", style = "color:red"))
    }
  }
  
})
