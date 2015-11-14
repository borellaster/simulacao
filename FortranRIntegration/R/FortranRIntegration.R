.onLoad <- function(lib, pkg) { 
        FortranRIntegrationLib<<-lib
	FortranRIntegrationPkg<<-pkg
	invisible(FortranRIntegrationLib)
	invisible(FortranRIntegrationPkg)
        cat("\nFortranRIntegration loaded...\n")
        cat("Read the Readme file for instructions on how to run the Simulation\n")
}

getDataFromDb<- function(dbConfig,query){
	library(DBI)
	library(RMySQL)
	drv <- dbDriver('MySQL')
	con <- dbConnect(drv, host=dbConfig$host, user=dbConfig$user, dbConfig$dbname, dbConfig$password)
	rs <- dbSendQuery(con,statement=query)
	result <- fetch(rs,n=-1)
	return(result)
}


getWeatherDataFromTxt <- function(path=getTextExamplePath()){
	return<-read.table(paste(path,"WEATHER.INP",sep=""),colClasses = "character")
	return(return)
}

getPlantDataFromTxt <- function(path=getTextExamplePath()){
	return<-read.table(paste(path,"PLANT.INP",sep=""),nrows=1,colClasses = "character")
	return(return)
}

getIrrigDataFromTxt <- function(path=getTextExamplePath()){
	return<-read.table(paste(path,"IRRIG.INP",sep=""),colClasses = "character")
	return(return)
}

getSoilDataFromTxt <- function(path=getTextExamplePath()){
	return<-read.table(paste(path,"SOIL.INP",sep=""),nrows=1,colClasses = "character")
	return(return)
}

runSimulation <- function(weather, plant, soil, irrig, doyp, frop){
	if (missing(weather) || missing(plant) || missing(irrig) || missing(soil) || missing(doyp) || missing(frop)) 
        stop("Missing Arguments")
	write.fwf(weather,"WEATHER.INP",colnames=FALSE,justify="right",width=c(5,5,5,5,5,17))
	write.fwf(plant,"PLANT.INP",colnames=FALSE,justify="right",width=c(8,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7))
	write.fwf(soil,"SOIL.INP",colnames=FALSE,justify="right",width=c(10,9,9,12,8,9,9))
	write.fwf(irrig,"IRRIG.INP",colnames=FALSE,justify="right",width=c(5,5))
	write.fwf(data.frame(doyp,frop),"SIMCTRL.INP",colnames=FALSE,justify="right",width=c(6,5))
        library.dynam("FortranRIntegration",FortranRIntegrationPkg,FortranRIntegrationLib)
	.Fortran("main",PACKAGE="FortranRIntegration")
	dyn.unload(unclass(.dynLibs())[[length(.dynLibs())]][2]$path)
        .dynLibs(.dynLibs()[c(1:length(.dynLibs())-1)])
	return(TRUE)
}

getTextExamplePath <- function()
{
	return(paste(FortranRIntegrationLib,"/Rworkshop/data/",sep=""))
}


getPlant <- function()
{
	result<-read.table("plant.out",skip=9,blank.lines.skip = TRUE)
        colnames(result)<-c("DOY", "Number of Leaf Nodes","Accum Temp during Reprod(oC)","Plant Weight(g/m2)","Canopy Weight (g/m2)","Root Weight (g/m2)","Fruit Weight (g/m2)","Leaf Area Index (m2/m2)")
	return(result)
}
getWbal <- function()
{
        result<-read.fwf("WBAL.OUT",skip=4,header=FALSE,width=c(33,7))
	return(result)
}
getSW <- function()
{
	result<-read.table("sw.out",skip=11)
	colnames(result)<-c("DOY", "Solar Rad. (MJ/m2)","Max Temp (oC)","Mix Temp (oC)","Rain (mm)","Irrig (mm)","Runoff (mm)", "Infil (mm)" , "Drain (mm)", "Pot. Evapo-Trans (mm)", "Actual Soil Evap. (mm)", "Actual Plant Trans. (mm)" ,"Soil Water Content (mm3/mm3)", "Drought Stress Factor", "Excess Water Stress Factor")
        return(result)
}

