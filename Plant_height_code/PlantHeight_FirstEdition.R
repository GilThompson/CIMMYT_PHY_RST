#Protocol to evaluate Plant height 
#Authors: Jose Manuel Mendoza Reyes and Lorena Gonzalez Perez
#Upgrade: Gil Thompson 19/11/22

#Call Libs and Funtions 
source("C:/Users/ITHOMPSON/Documents/CIMMYT/RS_Tools/01_libs.R")
source("C:/Users/ITHOMPSON/Documents/CIMMYT/RS_Tools/02_Fun.R")

# Load the raster base (DTM)
setwd(choose.dir(getwd(), "Select the directory wich are the BaseLine raster file"))
#select the BL raster file (DSM)
rasterBase <-raster(list.files(pattern = "\\.tif$"))
#Select the work directory wich are the raster file and shp
setwd(choose.dir(getwd(), "Select the work directory wich are the raster file and shp"))
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

#Subtract baseline from the height raster 
plant.height <- rest(raster_crop, raster_recla)

#select the shapefile (a shp with buffer genereted in Arcgis)
shp <- shapefile(list.files(pattern = "\\.shp$"))
#Set the name of the outut csv table
name<-list.files(pattern = "\\.shp$")
name<-gsub(".shp", "", name)

#Write the new  plant height raster (DSM)
writeRaster(plant.height, filename= paste(name, "height_.tif"),format = "GTiff",overwrite=TRUE)

plot(plant.height)
plot(shp, add= TRUE)

#Create a new enviroment using velox package
pixelval <- velox(plant.height)
#Extract the pixels values 
pixelval <- pixelval$extract(shp, small=TRUE) 

#Add the plot name to the pixel value list
names(pixelval)<- shp$Name 

#Testing the data 
print(sd(pixelval[[21]]))
print(length(pixelval[[21]]))

#The Soil.mask and outliers functions need these variables
numbr <- 0.05
sensor <- 1

#Apply the mask to the list
PlantsValue <- lapply(pixelval,soil.mask)
#Extract the outliers
Tableclean = lapply(na.omit(PlantsValue),outliers) # Here add the na.omit

str(Tableclean)

#Calculate data frame statistics
final <- estadisticos(Tableclean)

#write the final table
write.csv(final,paste(name,"_plant_Height.csv", sep=""), row.names=FALSE)
#write the readme file 
write.csv(paste("Mascara de suelo para ",name," es = ",numbr,", analisis de outliers es  = 2" ,sep="") ,paste("Readme.txt"), row.names=FALSE)






