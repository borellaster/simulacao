path <- "/Users/pba/github/simulacao/FortranRIntegration"
setwd(path)
library(RCurl)
library(rjson)
library(FortranRIntegration)
library(shiny)
library(ggplot2)
library(reshape)
library(scales)


#load the data
w <- getWeatherDataFromTxt(paste(path,"/data/", sep = ""))
i <- getIrrigDataFromTxt(paste(path,"/data/", sep = ""))
soil <- getSoilDataFromTxt(paste(path,"/data/", sep = ""))
plant <- getPlantDataFromTxt(paste(path,"/data/", sep = ""))

#collect stations
dataCurl <- getURLContent("http://dev.sisalert.com.br/apirest/api/v1/stations/src/INMET/lobs/true", ssl.verifypeer = FALSE)
jsonStation <- fromJSON(dataCurl)
stations <- data.frame("id"=sapply(jsonStation, function(x) x[[1]]), "name"=sapply(jsonStation, function(x) x[[2]]), "code"=sapply(jsonStation, function(x) x$metaData$weather[[3]]), "maxdatetime"=as.character(sapply(jsonStation, function(x) x$lobs$datetime)),"timezone"=as.character(sapply(jsonStation, function(x) x$location$zoneName)))

dataStation <- subset(stations, id=='564f796a16af35ca3decd181' | id=='564f796916af35ca3decd180' | id=='564f7b0316af35ca3decd292' | id=='564f7ce316af35ca3decd3e1')

#change de colnames
colnames(dataStation)[1] <- "ID"
colnames(dataStation)[2] <- "Name"
dataStation$Name <- as.character(dataStation$Name)
dataStation$ID <- as.character(dataStation$ID)
dataStation$url <- 
  paste0(
    "http://dev.sisalert.com.br/apirest/api/v1/data/station/model/", 
    dataStation$ID, "/range/11-01-2013/12-31-2014")

# Tipo de solo, para seleção
dataSoils <- data.frame( c(1, 2, 3, 4), c("Arenoso", "Medio", "Argiloso", "Default") )
colnames(dataSoils) <- c("ID", "Name")
dataSoils$Name <- as.character(dataSoils$Name)

#
#
# Funções functionModelVanGenucthen e functionSolos foram fornecidas pelo GRUPO DE SOLOS
#

#parametros do modelo de Van Genucthen (1980) para solo arenoso com teor de argila de 152 g kg-1
#dados disponiveis em Carducci et al., (2011)
functionModelVanGenucthen<-function(thpmp,thsat,a,n,solo) {
  result<-thpmp+((thsat-thpmp)/(1+(a*solo)^n)^(1-1/n))
  return(result)
}

functionSolos <- function(tipoSolo){
  
  ####### solo arenoso com teor de argila de 152 g kg-1###############################
  if (tipoSolo == 1) {
    #Solos arenosos profundos com pouco silte e argila;
    CN<-64
    
    # DP--> atribuindo a profundidade do perfil do solo (cm)
    DP<-200
    
    # STp--> encontrando a umidade do solo saturado (thsat=0 kPa)
    STp<-functionModelVanGenucthen(0.046,0.331,0.719,1.841,0.0)
    
    # FCp--> encontrando a umidade do solo na capacidade de campo (thCC=10 kPa)
    FCp<-functionModelVanGenucthen(0.046,0.331,0.719,1.841,10)
    
    # WPp--> encontrando a umidade do solo no ponto de murcha permanente (thPMP=1500 kPa)
    WPp<-functionModelVanGenucthen(0.046,0.331,0.719,1.841,1500)
    WPp<- round(WPp, digits = 3)
    
    # DNRp--> encontrando o percentual de drenagem diaria (thsat-thCC)
    DRNp<-STp-FCp
    
    # SWC--> encontrando a lamina de agua disponivel no perfil de solo considerado (mm)
    SWC<-(STp-FCp)*DP*10
    SWC
    
    # Retorna um vetor contendo o TipoSolo,CN, DP, STp, FCp, WPp, DRNp, SWC
    dadosSolo<-data.frame(CN, DP, STp, FCp, WPp, DRNp, SWC)
    
    return(dadosSolo)
  }
  
  #######solo medio com teor de argila de 420 g kg-1###############################
  if (tipoSolo == 2) {
    #Solos arenosos profundos com pouco silte e argila;
    CN<-76
    
    # DP--> atribuindo a profundidade do perfil do solo (cm)
    DP<-200
    
    # STp--> encontrando a umidade do solo saturado (thsat=0 kPa)
    STp<-functionModelVanGenucthen(0.190,0.645,1.398,1.398,0.0)
    
    # FCp--> encontrando a umidade do solo na capacidade de campo (thCC=10 kPa)
    FCp<-functionModelVanGenucthen(0.190,0.645,1.398,1.398,10)
    
    # WPp--> encontrando a umidade do solo no ponto de murcha permanente (thPMP=1500 kPa)
    WPp<-functionModelVanGenucthen(0.190,0.645,1.398,1.398,1500)
    WPp<- round(WPp, digits = 3)
    
    # DNRp--> encontrando o percentual de drenagem diaria (thsat-thCC)
    DRNp<-STp-FCp
    
    # SWC--> encontrando a lamina de agua disponivel no perfil de solo considerado (mm)
    SWC<-(STp-FCp)*DP*10
    
    # Retorna um vetor contendo o TipoSolo,CN, DP, STp, FCp, WPp, DRNp, SWC
    dadosSolo<-data.frame(CN, DP, STp, FCp, WPp, DRNp, SWC)
    
    return(dadosSolo)
  }
  
  #######solo argiloso com teor de argila de 716 g kg-1#############################
  if (tipoSolo == 3) {
    #Solos arenosos profundos com pouco silte e argila;
    CN<-84
    
    # DP--> atribuindo a profundidade do perfil do solo (cm)
    DP<-200
    
    thpmp=0.250
    thsat=0.806
    a=1.786
    n=1.599
    # STp--> encontrando a umidade do solo saturado (thsat=0 kPa)
    STp<-functionModelVanGenucthen(0.250,0.806,1.786,1.599,0.0)
    
    # FCp--> encontrando a umidade do solo na capacidade de campo (thCC=10 kPa)
    FCp<-functionModelVanGenucthen(0.250,0.806,1.786,1.599,10)
    
    # WPp--> encontrando a umidade do solo no ponto de murcha permanente (thPMP=1500 kPa)
    WPp<-functionModelVanGenucthen(0.250,0.806,1.786,1.599,1500)
    WPp<- round(WPp, digits = 3)
    
    # DNRp--> encontrando o percentual de drenagem diaria (thsat-thCC)
    DRNp<-STp-FCp
    
    # SWC--> encontrando a lamina de agua disponivel no perfil de solo considerado (mm)
    SWC<-(STp-FCp)*DP*10
    
    # Retorna um vetor contendo o TipoSolo,CN, DP, STp, FCp, WPp, DRNp, SWC
    dadosSolo<-data.frame(CN, DP, STp, FCp, WPp, DRNp, SWC)
    
    return(dadosSolo)
  }
}

#
#
# Funções gráfico do Meterologico
# Função de PLOT multiplo
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,layout.pos.col = matchidx$col))
    }
  }
}

# Função para transformar fator em numeric
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}



#################
# Shiny Server
#################
shinyServer(function(input, output, session) {
  output$ui <- renderUI({
    sidebarPanel(
      h4("Instruções"),
      p("Clique em Rodar Modelo após Escolher a Estação e o Tipo de Solo."),
      br(),
      selectInput(inputId = "cbxStations",
                  label = "Escolher a estação",
                  choices = dataStation$Name,
                  selectize = TRUE),
      
      selectInput(inputId = "cbxSoils",
                  label = "Escolher o tipo de solo",
                  choices = dataSoils$Name,
                  selectize = TRUE),
      
      actionButton("goSimulation", "Rodar Modelo"),
      uiOutput("mesg")
    )})
  
  getSelectionKey <- function() {
    #return(as.character(input$cbxStations))
    return(as.character(subset(dataStation, Name == input$cbxStations)$url))
  }
  
  getSelectionSoil <- function() {
    soilId <- as.integer(subset(dataSoils, Name == input$cbxSoils)$ID)
    if (soilId == 4) {
      return(soil)
    } else {
      return(functionSolos(soilId))
    }
  }
  
  observeEvent(input$goSimulation, {
    output$mesg <- renderText("")
    rodarSimulacao(getSelectionKey(), getSelectionSoil())
  })
  
  rodarSimulacao <- function(url, soils){
    
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
      
      runSimulation(weather = weatherData,plant = plant,soil = soils,irrig = i,doyp = 1, frop = 1)
      
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
      
      
      output$plotMeteorologico <- renderPlot({
        dataCurlP <- getURLContent(getSelectionKey(), ssl.verifypeer = FALSE)
        jW <- fromJSON(dataCurlP)
        wD <- do.call(rbind, lapply(jW, function(x) data.frame(x)))
        
        wD$date <- strptime(wD$date,"%y%j")
        
        wD$par <- as.numeric.factor(wD$par)
        wD$rain <- as.numeric.factor(wD$rain)
        wD$srad <- as.numeric.factor(wD$srad)
        wD$tmax <- as.numeric.factor(wD$tmax)
        wD$tmin <- as.numeric.factor(wD$tmin)
        
        p1 <- ggplot(wD, aes(x=date, y=par)) +
          geom_line(color="red") +  scale_x_datetime() + 
          ggtitle("PAR") + ylab("(MJ/m²) * 2 =~ mol[photon]/m²-day") + xlab("Data") 
        
        p2 <- ggplot(wD, aes(x=date, y=rain)) +
          geom_line(color="red") + scale_x_datetime() + geom_point(colour="red", size=3, shape=21, fill="white") +
          ggtitle("Precipitação") + ylab("mm") + xlab("Data") 
        
        # Third plot
        p3 <- ggplot(wD, aes(x=date, y=srad)) +
          geom_line(color="red") + scale_x_datetime() + geom_point(colour="red", size=3, shape=21, fill="white") +
          ggtitle("Radiação Solar") + ylab("W/m²") + xlab("Data")
        
        # Fourth plot
        p4 <- ggplot(wD, aes(x=date, y=tmax)) +
          geom_line(color="red") + scale_x_datetime() + geom_point(colour="red", size=3, shape=21, fill="white") +
          ggtitle("Temperatura Máxima") + ylab("ºC") + xlab("Data")
        
        p5 <- ggplot(wD, aes(x=date, y=tmin)) +
          geom_line(color="red") + scale_x_datetime() + geom_point(colour="red", size=3, shape=21, fill="white") +
          ggtitle("Temperatura Mínima") + ylab("ºC") + xlab("Data")
        
        multiplot(p1, p2, p3, p4, p5, cols=2)
      })
      
    } else {
      output$mesg <- renderUI(p("Estação não possui dados suficientes para uma simulação!", style = "color:red"))
    }
  }
  
})
