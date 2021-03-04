##GC TRIB DISTANCE FROM LT SAMPLEl
setwd('P:/BIOLOGICAL/Foodbase/LIGHT_TRAPS/DATA')
head(LTQP)
LTQP$Year<-year(LTQP$Date)
###MAKE A DATASET WITH ZEROES#######################################################
##LIMIT to 2015 - 2016##############################################################
HYzero<-droplevels(LTQP[LTQP$Year>2014 & LTQP$Year<2017,])
HYzero<-as.data.frame(with(HYzero, tapply(Count,list(BarcodeID,speciesID), FUN = function(x) sum(x))))
HYzero[is.na(HYzero)]<-0
head(HYzero)
##LIMIT TO TAXA I CARE ABOUT
HYzero<-HYzero[,c("HYCO","HYOC","HYOS","HCOC","HYCA","HYQU","HYSE","HYSP")]
HYzero$BarcodeID<-rownames(HYzero)
head(HYzero) 
##MERGE WITH LTSAMPLE INFO
HYZ<-merge(HYzero,LTP,by="BarcodeID")
HYZ<-HYZ[!is.na(HYZ$SEG),]
HYZ<-HYZ[!is.na(HYZ$SampleDuration),]
HYZ<-HYZ[HYZ$SampleDuration>49 & HYZ$SampleDuration<71,]
head(HYZ)
summary(HYZ$SEG) 
length(HYZ$BarcodeID) #1632
lt3<-HYZ
lt3<-lt3[lt3$SEGCODE=="C3" | lt3$SEGCODE=="C4",]
head(lt3)
length(lt3$BarcodeID)
lt3$RM<-lt3$RiverMile

#tribwater<-read.csv('P:/BIOLOGICAL/Foodbase/LIGHT_TRAPS/DATA/GCtribs.csv',header=T)
tribwater<-tribwater[order(tribwater[,2]),]
tribwater

#tribwater<-read.csv('P:/BIOLOGICAL/Foodbase/LIGHT_TRAPS/DATA/GC Tributaries Area for R.csv',header=T)
tribwater<-subset(tribwater,tribwater$water==1)
bigtrib<-tribwater[rev(order(tribwater[,6])),]
bigtrib<-rbind(bigtrib[c(1:8,12:14),])
bigtrib<-bigtrib[order(bigtrib[,7]),]
bigtrib

disttribwater<-
  function(x){
    t1<-match(x,tribwater[,2])
    t2<-ifelse(x<0.9,NA,1)
    t3<-ifelse(x>274.8,NA,1)
    if (is.na(t1)==F){up<-0
                      down<-0
                      up.near<-t1
                      down.near<-t1} else (if(is.na(t2)==F&is.na(t3)==F) {
                        temp<-findInterval(x,tribwater[,2])
                        up<-x-tribwater[temp,2]
                        down<-x-tribwater[(temp+1),2]
                        up.near<-temp
                        down.near<-temp+1
                      } else (if(is.na(t2)==T) {
                        down<-x-0.9
                        up<-NA
                        up.near<-NA
                        down.near<-1} else {
                          down<-NA
                          up<-x-274.8
                          up.near<-21
                          down.near<-NA}))
    return(cbind(up,down,up.near,down.near))}

tribwaterdata<-matrix(unlist(lapply(lt3$RM,disttribwater)),ncol=4,byrow=T)
tribwaterdata[,2]<-abs(tribwaterdata[,2])
bigmindist<-apply(tribwaterdata[,1:2],1,min,na.rm=T)
lt3$tribwaterdist<-bigmindist
#rm(bigmindist,tribwaterdata,tribwater,tribwater,disttribwater)

par(mfrow=c(1,2))
hist(lt3$tribwaterdist, main="all tribs")

head(lt3)
summary(lt3$bigtribdist,2)
summary(lt3$tribwaterdist)

lt3$pa<-ifelse(lt3$HYOS>0,1,0)
plot( lt3$HYOS,lt3$tribwaterdist, pch=21, bg="cyan4")


