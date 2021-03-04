##UPPER BASIN MAP#########################################
##########################################################
library(maps)
library(mapdata)
library(maptools)
library(rgeos)
library(ggplot2)



###CreateDataSet from loaded LTsample query
LTS<-LTsample
LT<-LTS[,c(1:4,7,30)]
LT1567<-LT[LT$Year=="2019",]   
length(unique(LT1567$BarcodeID))

#######LINK WITH LATTITUDE#######
LatLongCheater<-read.csv(paste('C:/Users/ametcalfe/Desktop/LatLongCheater',file.append='.csv',sep=''),header=T)
LTMAP<-merge(LT1567,LatLongCheater, by="BarcodeID")
length(unique(LTMAP$BarcodeID)) #4163...omits tributary samples and samples with no river miles
summary(LT1567$Reach)
summary(LTMAP$Reach)


##MAKE MAP###############################################
###UPDATE FEB 2018##############
##Import Reachs and res shapefiles, states ###
##Import rivers and res shapefiles, states ###
UBRiversShapefile<-readShapeLines("U:/LTrivers/LTriversthick.shp")
UBRivers_df<-fortify(UBRiversShapefile)
ParksShapefile<-readShapeLines("P:/BIOLOGICAL/Foodbase/LIGHT_TRAPS/GIS/Upper basin/Shapefiles/NCPNparks_And_GRCA.shp")
Parks<-fortify(ParksShapefile)

UBResShapefile<-readShapeLines("U:/LTres/LTres.shp")
UBRes_df<-fortify(UBResShapefile)

all_states <- map_data("state")
states <- subset(all_states, region %in% c("utah","colorado","nevada","arizona","wyoming","new mexico","idaho"))


scaleBar <- function(lon, lat, distanceLon, distanceLat, distanceLegend, dist.unit = "km", rec.fill = "white", rec.colour = "black", rec2.fill = "black", rec2.colour = "black", legend.colour = "black", legend.size = 5, orientation = TRUE, arrow.length = 500, arrow.distance = 300, arrow.North.size = 6){
	laScaleBar <- createScaleBar(lon = lon, lat = lat, distanceLon = distanceLon, distanceLat = distanceLat, distanceLegend = distanceLegend, dist.unit = dist.unit)
	# First rectangle
	rectangle1 <- geom_polygon(data = laScaleBar$rectangle, aes(x = lon, y = lat), fill = rec.fill, colour = rec.colour)
	
	# Second rectangle
	rectangle2 <- geom_polygon(data = laScaleBar$rectangle2, aes(x = lon, y = lat), fill = rec2.fill, colour = rec2.colour)
	
	# Legend
	scaleBarLegend <- annotate("text", label = paste(laScaleBar$legend[,"text"], dist.unit, sep=""), x = laScaleBar$legend[,"long"], y = laScaleBar$legend[,"lat"], size = legend.size, colour = legend.colour)
	
	res <- list(rectangle1, rectangle2, scaleBarLegend)
	
	if(orientation){# Add an arrow pointing North
		coordsArrow <- createOrientationArrow(scaleBar = laScaleBar, length = arrow.length, distance = arrow.distance, dist.unit = dist.unit)
		arrow <- list(geom_segment(data = coordsArrow$res, aes(x = x, y = y, xend = xend, yend = yend)), annotate("text", label = "N", x = coordsArrow$coordsN[1,"x"], y = coordsArrow$coordsN[1,"y"], size = arrow.North.size, colour = "black"))
		res <- c(res, arrow)
	}
	return(res)
}



##MAPPING!!!##
s<-ggplot() +
geom_polygon( data=states, aes(x=long, y=lat, group=group),colour="black", fill="white" ) + ggtitle("2019 Light Trap Samples") +
geom_path(data=UBRivers_df,aes(x=long,y=lat, group=group),colour="gray", cex=1.2) +
geom_polygon(data=UBRes_df,aes(x=long,y=lat, group=group),colour="gray",fill="gray", cex=1.2) +
#geom_polygon(data=Parks,aes(x=long,y=lat, group=group),colour="black",fill="cyan", cex=1.2, alpha=0.7) +
coord_cartesian(xlim = c(-113.9,-107.35), ylim = c(42.5,35.9), expand = TRUE) +
theme(axis.text.y = element_blank(), axis.ticks.x = element_blank(),axis.ticks.y = element_blank(), axis.title = element_blank(),axis.text.x = element_blank(),) +
geom_jitter(data=LTMAP[LTMAP$Year=="2019",],aes(x=Long,y=Lat),fill="red", pch=21, width=0.05, height=0.03, cex=2, alpha=0.5)
s


ggsave("SampleMap2019_Feb2020.pdf", s, height=12.9, width=10, units='in',dpi=800) #set CEX to 3 or 4 and scale legend to 7



