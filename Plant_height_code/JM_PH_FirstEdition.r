#Protocol to evaluate Plant height 
#Authors: Jose Manuel Mendoza Reyes and Lorena Gonzalez Perez
library (sp)
library (raster)
library(rgdal)
library(data.table)
library(plyr)
library(dplyr)

# Load the raster base (DTM)
rasterBase<- raster("C:/Users/ITHOMPSON/Desktop/RAW_2018/RGB/phy_PsTails_RGB_180216/fake/LineaBase.tif")
#rasterBase<- raster("C:/Users/ITHOMPSON/Documents/Protocolo plant height/DTM linea base Falcon/RgoB_Lbase.tif")
#rasterBase<- raster("C:/Users/ITHOMPSON/Documents/Protocolo plant height/DTM linea base Falcon/Raster_base_Kriging_0107HIBAP.tif")
#rasterBase<- raster("C:/Users/ITHOMPSON/Documents/Protocolo plant height/DTM linea base Falcon/DH_lamp_LBase1.tif") #RgoA_Lbase
#rasterBase<- raster("C:/Users/ITHOMPSON/Documents/Protocolo plant height/DTM linea base Falcon/RgoA_Lbase.tif")
#rasterBase<- raster("C:/Users/ITHOMPSON/Documents/Protocolo plant height/DTM linea base Falcon/RgoC_Lbase.tif")
#rasterBase<- raster("C:/Users/ITHOMPSON/Documents/Protocolo plant height/DTM linea base Falcon/RgoD_Lbase.tif")
#rasterBase<- raster("C:/Users/ITHOMPSON/Documents/Protocolo plant height/DTM linea base Falcon/Sint_rgo.tif")
#rasterBase<- raster("C:/Users/ITHOMPSON/Documents/Protocolo plant height/DTM linea base Falcon/Sint_Sq.tif")
#rasterBase<- raster("C:/Users/ITHOMPSON/Documents/Protocolo plant height/DTM linea base Falcon/Sq_Drip_Lbase.tif") #Sq_grav_Lbase.tif
#rasterBase<- raster("C:/Users/ITHOMPSON/Documents/Protocolo plant height/DTM linea base Falcon/Sq_grav_Lbase.tif")


#Select the work directory wich are the raster file and shp
setwd(choose.dir(getwd(), "Selecciona tu archivo"))

#select the raster file (DSM)
raster <-raster(list.files(pattern = "\\.tif$"))

plot(raster)
plot(rasterBase, add= TRUE)

#Cut the DSM base on the DTM extent
raster_crop<-crop(raster, extent(rasterBase), snap="out")


#Change the spatial resolution of DTM (if it is necessary)
raster_recla <- resample(rasterBase, raster_crop, resample='ngb')

#Check that the two raster have the same attributes

#DTM
show(raster_recla)
#DSM
show(raster_crop)

#creacion de la funcion 
rest <-function(r1,base){(r1-base)}

#llamdo de la funcion anterior
plant.height <- rest(raster_crop, raster_recla)

plot(plant.height)


#select the shapefile (a shp with buffer genereted in Arcgis)
shp <- shapefile(list.files(pattern = "\\.shp$"))

#plot(plant.height)
#plot(shp, add= TRUE)

#Set the name of the outut csv table
name<-list.files(pattern = "\\.shp$")
name<-gsub(".shp", "", name)


#Write the new  plant height raster (DSM)
writeRaster(plant.height, filename= paste(name, "height_sincom.tif"),format = "GTiff",overwrite=TRUE)

plot(plant.height)
plot(shp, add= TRUE)


#Overlap and extract the pixel value in each polygon
pixelval <- extract(plant.height,shp) #actualizar con velox

#Add the plot name to the pixel value list
names(pixelval)<- shp$Name


#check the list
#str(pixelval)
numbr <- 0.05
#function to eliminate soil pixels 
soil.mask <- function(p){
	p.1 = subset(p,p > numbr)
	return(p.1)
	}

#Apply the mask to the list
PlantsValue <- lapply(pixelval,soil.mask)


#function to calculate and eliminate outliers
outliers <- function(dat){
n=2
a=median(dat)
b=sd(dat)
limitesuperior=(a+n*b)
limiteinferior=(a-n*b)
dat1= subset(dat,dat <= limitesuperior & dat >= limiteinferior)
return(dat1)
}

#extract the outliers
Tableclean = lapply(PlantsValue,outliers)


str(Tableclean)


#Calculate the descriptive statistics of the new list without outliers and convert it in a data base

allstats = lapply(Tableclean,summary)
sd = lapply(Tableclean, sd)
n = lapply(Tableclean, length)

#Join all the statistics calculated in one table
desc.stats.t = mapply(c,allstats,sd, n)

# Transpose the matrix
desc.stats <- t(desc.stats.t)


#Convert to data frame
df <- as.data.frame(desc.stats)

# Add the rownames as a new column
setDT(df, keep.rownames = TRUE)[]

#Rename the columns
colnames(df)[1:9]<-c("Plot", "Min", "Q1", "Median", "Mean", "Q3", "Max", "SD", "n")

#Rewrite again the data frame
df<-data.frame(df)

# Average the values using the unique ID (Plot names)
final.df=aggregate(cbind(Min, Q1, Median, Mean, Q3, Max, SD, n)~Plot,df, mean)

#write the final table
write.csv(final.df,paste(name,"_plant_Height.csv", sep=""), row.names=FALSE)

#write the readme file 
write.csv(paste("Mascara de suelo para ",name," es = ",numbr,", analisis de outliers es  = 2" ,sep="") ,paste("Readme.txt"), row.names=FALSE)






