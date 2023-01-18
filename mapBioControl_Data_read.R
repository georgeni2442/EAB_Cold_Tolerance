#Need to read in all of the datasets
#Filter out all the datasets for just Michigan 
#Create plots and visualizations for data-as much as possible-also look into cleaning and editing 
library(dplyr)
library(stringr)
library(maptools)
library(rgeos)
library(tidyverse)
library(rgdal)
library(ggthemes)
library(chron)
library(ggmap)
library(sf)


no_recovery<-read.csv("No_Recovery.csv")
yes_recovery<-read.csv("Yes_Recovery.csv")
forest_type<-read.csv("Forest_Type.csv")
para_release<-read.csv("Parasitoid_Release.csv")
release_sites<-read.csv("Release_Sites.csv")
release_trees<-read.csv("Release_Trees.csv")
eab<-read.csv("EAB Detections.csv")
unique(yes_recovery$STATE)

#filtering to Michigan 
mi_no_recov<-no_recovery%>%
 # filter(no_recovery$ï..STATE=="Michigan")%>%
  separate(DATE_TRAP_SAMPLED, into=c("Date", "Time"), sep=",",remove=TRUE)




# Reformat date/time to date
mi_no_recov$Date<-as.Date(mi_no_recov$Date, format='%m/%d/%Y')





mi_yes_recov<-yes_recovery%>%
 # filter(yes_recovery$ï..STATE=="Michigan", ORIGIN=="Research")%>%
  separate(DATE_TRAP_SAMPLED, into=c("Date", "Time"), sep=",",remove=TRUE)



mi_yes_recov$Date<-as.Date(mi_yes_recov$Date, format= '%m/%d/%Y')


mi_yes_recov$NUMBER_T.PLANIPENNISI_INDIVIDUALS<-as.numeric(as.character(mi_yes_recov$NUMBER_T.PLANIPENNISI_INDIVIDUALS))

ggplot(data=mi_yes_recov, aes(x=Date, y=NUMBER_S.AGRILI_INDIVIDUALS))+geom_point()

ggplot(data=mi_yes_recov, aes(x=Date, y=NUMBER_T.PLANIPENNISI_INDIVIDUALS))+geom_point(aes(color=SITE_NAME))
  
ggplot(data=mi_yes_recov, aes(x=Date, y=NUMBER_O.AGRILI))+geom_point()

#Maybe change points symbology based on location? 

class(mi_yes_recov$Number)

#Creating map of Michigan 
#mi_shp<-readOGR("Documents/EAB/Michigan_Shape_File", "Counties__v17a_") 
#mi_shp<-tidy(mi_shp)
#file.exists("Michigan_Shape_File/Counties__v17a_.shp")

mi_shp<-read_sf("Michigan_Shape_File/Counties__v17a_.shp")
sites<-st_as_sf(mi_yes_recov,coords=c("POINT_X", "POINT_Y"), crs=4326,agr="constant")

ggplot(mi_shp)+geom_sf()+xlab("Longitude")+ylab("Latitude")+geom_sf(data=sites,size=2,shape=21, fill="blue")
  
plot(mi_shp)
#Release Trees 

