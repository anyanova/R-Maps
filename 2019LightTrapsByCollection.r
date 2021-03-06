##Grand Canyon sample map, WIREs

setwd("C:/Users/ametcalfe/Desktop/WiRES_CitSci/GabiMendezFigures/")


##########################################################
library(maps)
library(mapdata)
library(maptools)
library(rgeos)
library(ggplot2)
library(lubridate)
library(sf)
library(tools)
library("rnaturalearth")
library("rnaturalearthdata")


#Import and format data from database
LTsample<-as.data.frame(read.csv(url("https://raw.githubusercontent.com/jmuehlbauer-usgs/Database/master/LightTrapSample.csv")))
##Add Year, Month, Day using library(lubridate)
	LTsample$Date<-as.Date(LTsample$Date, format = "%m/%d/%Y")
	LTsample$Year<-year(LTsample$Date)
	LTsample$Month<-month(LTsample$Date)
	LTsample$Jul<-yday(LTsample$Date)		
#Remove shitty samples
LTsample<-LTsample[which(LTsample$FlagDelete!=1),]


#Make dataset of GC LTsamples from 2019
LTGC<-LTsample[LTsample$Reach=="CRGrandCanyon" | LTsample$Reach=="CRLeesFerry",]
LTGC<-LTGC[LTGC$Year==2019,]
summary(LTGC)
head(LTGC)

#Categorize collectors (Metcalfe March trip was a private)
unique(LTGC$Collector)	
LTGC$Collection<-ifelse(LTGC$Collector=="SzydloC"| LTGC$Collector=="KennedyT"|LTGC$Collector=="GCMRC"|LTGC$Collector=="GoodenoughD"|LTGC$Collector=="MuehlbauerJ"|LTGC$Collector=="DaubertM", "USGS","CitSci")
LTGC[LTGC$BarcodeID=="L11363",29]<-"USGS"

#Add LatLong
GCRM<-read.csv("C:/Users/ametcalfe/Desktop/GIS/GrandCanyon/GrandCanyonLatLongRM.csv")
LTGC<-merge(LTGC,GCRM, by="RiverMile", no.dups=TRUE)

colnames(LTGC)

table(LTGC$Collection)



##MAKE MAP###############################################
Mainstem<-sf::st_read("C:/Users/ametcalfe/Desktop/GIS/GrandCanyon/RiverMileCenterline_UPDATED/RiverMileCenterline_UPDATED.shp")
Tribs<-sf::st_read("C:/Users/ametcalfe/Desktop/GIS/GrandCanyon/GC_tributaries/Tributaries_KM_Line_Dissolve.shp")
UBResShapefile<-sf::st_read("U:/LTres/LTres.shp")

all_states <- map_data("state")
AZ <- subset(all_states, region %in% "arizona")


##State layer using sf
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
	states <- cbind(states, st_coordinates(st_centroid(states)))
	states$ID <- toTitleCase(states$ID)
	head(states)

#Labeled map of all states!
ggplot() +
    geom_sf(data = states, fill = "white") + 
    geom_text(data = states, aes(X, Y, label = ID), size = 5) 

#Narrow it down to N AZ
	+
#    coord_sf(xlim = c(-115, -109), ylim = c(35, 38), expand = FALSE)



#Scalebars

		createScaleBar <- function(lon,lat,distanceLon,distanceLat,
								   distanceLegend, dist.units = "km"){
			# First rectangle
			bottomRight <- gcDestination(lon = lon, lat = lat, bearing = 90, 
										 dist = distanceLon, dist.units = dist.units,
										 model = "WGS84")
			
			topLeft <- gcDestination(lon = lon, lat = lat, bearing = 0, 
									 dist = distanceLat, dist.units = dist.units, 
									 model = "WGS84")
			rectangle <- cbind(lon=c(lon, lon, bottomRight[1,"long"],
									 bottomRight[1,"long"], lon),
							   lat = c(lat, topLeft[1,"lat"], topLeft[1,"lat"],
									   lat, lat))
			rectangle <- data.frame(rectangle, stringsAsFactors = FALSE)
			
			# Second rectangle t right of the first rectangle
			bottomRight2 <- gcDestination(lon = lon, lat = lat, bearing = 90, 
										  dist = distanceLon*2, dist.units = dist.units,
										  model = "WGS84")
			rectangle2 <- cbind(lon = c(bottomRight[1,"long"], bottomRight[1,"long"],
										bottomRight2[1,"long"], bottomRight2[1,"long"],
										bottomRight[1,"long"]),
								lat=c(lat, topLeft[1,"lat"], topLeft[1,"lat"], 
									  lat, lat))
			rectangle2 <- data.frame(rectangle2, stringsAsFactors = FALSE)
			
			# Now let's deal with the text
			onTop <- gcDestination(lon = lon, lat = lat, bearing = 0, 
								   dist = distanceLegend, dist.units = dist.units, 
								   model = "WGS84")
			onTop2 <- onTop3 <- onTop
			onTop2[1,"long"] <- bottomRight[1,"long"]
			onTop3[1,"long"] <- bottomRight2[1,"long"]
			
			legend <- rbind(onTop, onTop2, onTop3)
			legend <- data.frame(cbind(legend, text = c(0, distanceLon, distanceLon*2)),
								 stringsAsFactors = FALSE, row.names = NULL)
			return(list(rectangle = rectangle, rectangle2 = rectangle2, 
						legend = legend))
		}

		scaleBar <- function(lon, lat, distanceLon, distanceLat, 
							 distanceLegend, dist.unit = "km", rec.fill = "white",
							 rec.colour = "black", rec2.fill = "black", 
							 rec2.colour = "black", legend.colour = "black", 
							 legend.size = 3, orientation = TRUE, arrow.length = 500,
							 arrow.distance = 300, arrow.North.size = 6){
			laScaleBar <- createScaleBar(lon = lon, lat = lat, 
										 distanceLon = distanceLon, 
										 distanceLat = distanceLat, 
										 distanceLegend = distanceLegend, 
										 dist.unit = dist.unit)
			# First rectangle
			rectangle1 <- geom_polygon(data = laScaleBar$rectangle, 
									   aes(x = lon, y = lat), fill = rec.fill, 
									   colour = rec.colour)
			
			# Second rectangle
			rectangle2 <- geom_polygon(data = laScaleBar$rectangle2, 
									   aes(x = lon, y = lat), fill = rec2.fill, 
									   colour = rec2.colour)
			
			# Legend
			scaleBarLegend <- annotate("text", label = paste(laScaleBar$legend[,"text"],
															 dist.unit, sep=""), 
									   x = laScaleBar$legend[,"long"], 
									   y = laScaleBar$legend[,"lat"], 
									   size = legend.size, 
									   colour = legend.colour, fontface="bold")
			
			res <- list(rectangle1, rectangle2, scaleBarLegend)
			
			if(orientation){# Add an arrow pointing North
				coordsArrow <- createOrientationArrow(scaleBar = laScaleBar, 
													  length = arrow.length, 
													  distance = arrow.distance,
													  dist.unit = dist.unit)
				arrow <- list(geom_segment(data = coordsArrow$res, 
										   aes(x = x, y = y, xend = xend, yend = yend)),
							  annotate("text", label = "N", 
									   x = coordsArrow$coordsN[1,"x"], 
									   y = coordsArrow$coordsN[1,"y"], 
									   size = arrow.North.size, colour = "black"))
				res <- c(res, arrow)
			}
			return(res)
		}


#Make map
GCmap2019<-ggplot() + ggtitle("Grand Canyon Light Trap Samples 2019") +
    geom_sf(data = states, fill = "white") + 
	geom_sf(data=Mainstem,colour="gray", cex=1.2) +
	geom_sf(data=Tribs,colour="lightgray", cex=1.2) +
	geom_sf(data=UBResShapefile,colour="gray",fill="gray", cex=1.2) +
	coord_sf(xlim = c(-115, -111), ylim = c(35.5, 37.5), expand = FALSE) +
	geom_point(data=LTGC[LTGC$Collection=="CitSci",],aes(x=Long,y=Lat), fill="white", size=3, pch=21, alpha=0.6) +
	geom_jitter(data=LTGC[LTGC$Collection=="USGS",],aes(x=Long,y=Lat), fill="darkorchid4", size=3, pch=21, width=0.01, height=0.01, alpha=0.8) +
	scaleBar(lon = -114.8, lat = 37.2, distanceLon = 25, distanceLat = 10, distanceLegend = 25, dist.unit = "km", orientation = FALSE,legend.size=3) 
GCmap2019
ggsave("WIREsCitSci_BGmap.pdf", GCmap2019, height=12.9, width=10, units='in',dpi=800)
 
 
#Temporal Figure, spread by month 2019
#Stacked barplot
head(LTGC)
CSbar<-ggplot(LTGC, aes(x=Month, fill=Collection)) + 
	geom_bar(col="black") + scale_fill_manual(values=c("azure","forestgreen")) +
	theme_classic() + ylab(expression(~italic(n)~ 'samples')) +
	scale_x_discrete(limit=c(2:10))
CSbar
ggsave("WIREsCitSci_Bar.pdf", CSbar, height=4, width=4.5, units='in',dpi=800)


#Sample accumulation figure
#Make 2 dataframes with total n/dayhead(LTGC)
ACC<-as.data.frame(with(LTGC, tapply(BarcodeID,list(Jul, Collection), FUN = function(x) length(x))))
ACC$Jul<-as.numeric(rownames(ACC))
ACC[is.na(ACC)]=0
ACC$CSrollsum<-cumsum(ACC$CitSci)
ACC$GSrollsum<-cumsum(ACC$USGS)
head(ACC)

#Plot
AccPlot<-ggplot(ACC,aes(x=Jul,y=CSrollsum))+ 
		geom_smooth(col="black", size=2, se=FALSE)+
		geom_smooth(aes(x=Jul,y=GSrollsum), col="darkorchid3", size=2, se=FALSE) +
		xlab("Day of year") + ylab(expression(~italic(n)~ 'samples')) +
		theme_classic() + theme(text = element_text(size=20))
AccPlot
ggsave("WIREsCitSci_AccumulationFig.pdf", AccPlot, height=4, width=6, units='in',dpi=800)


