setwd("/Users/pba/Simulacao/FortranRIntegration")
library(RCurl)
library(rjson)
library(FortranRIntegration)
###
#dataCurl <- getURLContent("http://dev.sisalert.com.br/apirest/api/v1/data/station/model/5602a58b92c884831962ceea/range/01-11-2014/01-01-2015", ssl.verifypeer = FALSE)
#jsonWeather <- fromJSON(dataCurl)
#weatherData <- do.call(rbind, lapply(jsonWeather, function(x) data.frame(x)))
##
dataCurl <- getURLContent(paste("http://dev.sisalert.com.br/apirest/api/v1/stations",sep = ""))
json <- fromJSON(dataCurl)
dataStation <- do.call(rbind, lapply(json, function(x) data.frame(x)))


w <- getWeatherDataFromTxt("/Users/pba/Simulacao/FortranRIntegration/data/")

i <- getIrrigDataFromTxt("/Users/pba/Simulacao/FortranRIntegration/data/")
soil <- getSoilDataFromTxt("/Users/pba/Simulacao/FortranRIntegration/data/")
plant <- getPlantDataFromTxt("/Users/pba/Simulacao/FortranRIntegration/data/")

runSimulation(weather = w,plant = plant,soil = soil,irrig = i,doyp = 1, frop = 1)

### coletar dados

plantOut <- read.table("plant.out",skip = 9)
swOut <- read.table("sw.out",skip = 9)
wbalOut <- read.table("WBAL.OUT",skip = 4, sep = ":")
