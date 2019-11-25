
####################################################################################################
soil.mask <- function(p)
{				
		if(sensor == 1){
				# NDVI
				  p.1 = subset(p,p >numbr)
				  return(p.1)
		} else {
				#TEMPERATURA

				   p.1 = subset(p,p <numbr)
				   return(p.1)
				 }
	   # return(p.1)
}

####################################################################################################
#function to calculate and eliminate outliers
outliers <- function(dat){
n=2
a=median(dat)
b=sd(dat)
limitesuperior=(a+n*b)
limiteinferior=(a-n*b)
dat1= (subset(dat,dat <= limitesuperior & dat >= limiteinferior))
return(dat1)
}


####################################################################################################
estadisticos <- function(dd) {
  
  
  sd_ =      na.omit( as.list( lapply(dd, sd)))#dd2
  mean_ =   na.exclude( as.list( lapply(dd, mean  ))) 
  maxi_ =    na.exclude( as.list( lapply(dd, max)))
  mini_ =    na.exclude( as.list( lapply(dd, min)))
  mediani_=  na.exclude( as.list( lapply(dd,median )))
  num_ =     na.exclude( as.list( lapply(dd, length)))
  
  ###########ESTA FUNCIONA EN TER
  #nuevo22 <- as.list.data.frame(t(mapply(c, names(dd2),sd22, mean_22, maxi22, mini22, mediani22, num22)))
  ##Esta funciona en NDV
  nuevo22 <- as.data.frame(cbind(names(dd), sd_, mean_, maxi_, mini_, mediani_, num_))
  #Rename the columns
  colnames(nuevo22)[1:7]<-c( "Plot_",  "SD_", "Mean_", "Max_", "Min_", "Mediana_", "n_")
  
  
  nuevo22$Plot_    <-  unlist(nuevo22$Plot_) 
  nuevo22$SD_      <-  unlist(nuevo22$SD_)
  nuevo22$Mean_    <-  unlist(nuevo22$Mean_)
  nuevo22$Max_     <-  unlist(nuevo22$Max_)
  nuevo22$Min_     <-  unlist(nuevo22$Min_)
  nuevo22$Mediana_ <-  unlist(nuevo22$Mediana_)####ERROR AQUI 
  nuevo22$n_       <-  unlist(nuevo22$n_)
  
  
  final=aggregate(cbind(nuevo22$SD_, nuevo22$Mean_ , nuevo22$Max_, nuevo22$Min_, nuevo22$Mediana_, nuevo22$n_ )~(nuevo22$Plot_), (nuevo22), mean) #tipo paa ter probar #
  
  colnames(final)[1:7]<-c( "Plot_",  "SD_", "Mean_", "Max_", "Min_", "Median_", "n_")
  
  return(final)
}


####################################################################################################
### Function for resample te raster if the scale is too big ###
reduce_scale <- function(Rstr, ext) {
  r<-nrow(Rstr) 
  c<-ncol(Rstr)
  #ext <- (corte2)
  s <- raster(nrow=r*2, ncol= c*2, ext)
  out <- resample(Rstr,s )#, na.omit=T
}
###################################################################################################


#Function for make de diference betwean Baseline and Raster 
rest <-function(r1,base){(r1-base)}




