##Make a very basic map of Grand Canyon
##Anya Metcalfe 5/23/2023

##Load packages (install with install.packages("") if needed)
library(tidyverse)
library(ggmap)
library(sf)
library(maps)
library(ggsn)
library(ggspatial)

#Import shapefiles, change address as needed
#Shapefiles created by Anya Metcalfe in arcGIS
Mainstem<-sf::st_read("C:/Users/ametcalfe/Desktop/GIS/GrandCanyon/RiverMileCenterline_UPDATED/RiverMileCenterline_UPDATED.shp")
Mainstem<-st_zm(Mainstem)
Tribs<-sf::st_read("C:/Users/ametcalfe/Desktop/GIS/GrandCanyon/GC_tributaries/Tributaries_KM_Line_Dissolve.shp")
UBResShapefile<-sf::st_read("C:/Users/ametcalfe/Desktop/GIS/CRB_shapefiles_water/LTres/LTres.shp")


##Add park shapefiles
library(maptools)
area <- sf::st_read ("C:/Users/ametcalfe/Desktop/GIS/ne_10m_parks_and_protected_lands/ne_10m_parks_and_protected_lands_area.shp")
GCNP<-area[area$unit_name=="Grand Canyon NP",]
GLCA<-area[area$unit_name=="Glen Canyon NRA",]
MEAD<-area[area$unit_name=="Lake Mead NRA",]


##Create a state layer using sf
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
	states <- cbind(states, st_coordinates(st_centroid(states)))
	states$ID <- toTitleCase(states$ID)
	head(states)

#Labeled map of all states
ggplot() +
    geom_sf(data = states, fill = "white") + 
    geom_text(data = states, aes(X, Y, label = ID), size = 5) 

##Basic unlabeled map of Colorado River, trib, and reservoirs
##Sample locations can be added from a seperate dataframe using geom_point()/geom_jitter() using lat and long as x and y coordinates
		##Example: 	geom_jitter(data=GCHY,aes(x=Long,y=Lat, size=HYcatchrate, fill=Season), pch=21, width=0.01, height=0.01, alpha=0.6)
##Text labels can be added here using geom_text or manually in an image editor (I use InkScape)							
GCmap<-ggplot() + ggtitle("Colorado River in Grand Canyon") +
    geom_sf(data = states, fill = "white") + 
	geom_sf(data=GCNP,colour="darkgreen",fill="darkgreen", cex=1.2) +
	geom_sf(data=GLCA,colour="darkgreen",fill="darkgreen", cex=1.2) +
	geom_sf(data=MEAD,colour="darkgreen",fill="darkgreen",cex=1.2) +
	geom_sf(data=Mainstem,colour="deepskyblue2", cex=1.2) +
	geom_sf(data=Tribs,colour="deepskyblue2", cex=1.2) +
	geom_sf(data=UBResShapefile,colour="deepskyblue2",fill="deepskyblue2", cex=1.2) +
	ggspatial::annotation_scale(location = 'tl') +
	coord_sf(xlim = c(-115, -111), ylim = c(35.5, 37.5))+
	annotation_north_arrow(which_north = "true",  
		  height = unit(1.5, "cm"),
		  width = unit(1.5, "cm"),
		  pad_x = unit(1.25, "cm"),
		  pad_y = unit(12, "cm")) 


GCmap


##Save with dimensions proportional to this ratio to keep projection from getting compressed
ggsave("GCblankmap_parks.pdf", GCmap, height=12.9, width=10, units='in',dpi=300)



##Add temp data 
#Temps compiled by Kate Behn May 2023
Temps21<-as.data.table(read.csv("C:/Users/ametcalfe/Desktop/ParasiteMaps/MapTemps20Yearfilled.csv"))
Temps22<-as.data.table(read.csv("C:/Users/ametcalfe/Desktop/ParasiteMaps/MapTemps2022filled.csv"))
GrandRM<-as.data.table(read.csv("C:/Users/ametcalfe/Desktop/ParasiteMaps/GrandRM.csv"))

RM<-GrandRM[, list(Lat=min(Lat), Long=min(Long)), by=round(RM)]
colnames(RM)<-c("RM","Lat","Long")
Temps21<-merge(Temps21,RM, by="RM")
Temps22<-merge(Temps22,RM, by="RM")

library(wesanderson)
 
ggplot(Temps21) + 
	geom_point(aes(RM,MaxTempDegCInterpolated, col=MaxTempDegCInterpolated)) +
	scale_color_gradientn(colors = wes_palette("Zissou1", type = "continuous")) +
	geom_point(data=Temps22, aes(RM,MaxTempDegCInterpolated, col=MaxTempDegCInterpolated)) + 
	theme_classic()
	


GCmap<-ggplot() + ggtitle("Colorado River in Grand Canyon") +
    geom_sf(data = states, fill = "white") + 
	geom_sf(data=GCNP,colour="darkgreen",fill="darkgreen", cex=1.2) +
	geom_sf(data=GLCA,colour="darkgreen",fill="darkgreen", cex=1.2) +
	geom_sf(data=MEAD,colour="darkgreen",fill="darkgreen",cex=1.2) +
	geom_sf(data=Mainstem,colour="deepskyblue2", cex=1.2) +
	geom_sf(data=Tribs,colour="deepskyblue2", cex=1.2) +
	geom_sf(data=UBResShapefile,colour="deepskyblue2",fill="deepskyblue2", cex=1.2) +
	ggspatial::annotation_scale(location = 'tl') +
	coord_sf(xlim = c(-115, -111), ylim = c(35.5, 37.5))+
	annotation_north_arrow(which_north = "true",  
		  height = unit(1.5, "cm"),
		  width = unit(1.5, "cm"),
		  pad_x = unit(1.25, "cm"),
		  pad_y = unit(12, "cm")) 
GCmap


##Temps21
T21<-GCmap + labs(col="TempC") + ggtitle("Temps 2021") +
	scale_color_gradientn(colors = wes_palette("Zissou1", type = "continuous"), limits=c(13,26)) +
	geom_point(data=Temps21, aes(x=Long, y=Lat, col=Temps21$MaxTempDegCInterpolated))

##Temps22
	
T22<-GCmap + labs(col="TempC") + ggtitle("Temps 2022") +
	scale_color_gradientn(colors = wes_palette("Zissou1", type = "continuous"), limits=c(13,26)) +
	geom_point(data=Temps22, aes(x=Long, y=Lat, col=Temps22$MaxTempDegCInterpolated))

##Save with dimensions proportional to this ratio to keep projection from getting compressed
ggsave("GC_Temps2000to2021.pdf", T21, height=12.9, width=10, units='in',dpi=300)
ggsave("GC_Temps2022.pdf", T22, height=12.9, width=10, units='in',dpi=300)