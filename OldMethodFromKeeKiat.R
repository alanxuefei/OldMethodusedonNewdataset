setwd("C:/Users/Alan/Desktop/SingtelProject/RProgramming/FromOldMethod/Script")

tbllabel <- read.csv(file="Mano1.5_windowfile.csv",head=TRUE)
tbllocation <- read.csv(file="Mano1.5_tblLocation.csv",head=TRUE)
tblsensor<- read.csv(file="Mano1.5_tblSensor.csv",head=TRUE)

tblsensor<-tblsensor[order(as.Date(tblsensor$txtDate, format="%Y-%m-%d %H:%M:%S")),]
tbllabel<-tbllabel[order(as.Date(tbllabel$starttime, format="%Y-%m-%d %H:%M:%S")),]
modeset<-c("Walking","MRT","Taxi","Cycling","Bus","NA") 

# Calculate distance in kilometers between two points
earth.dist <- function (long1, lat1, long2, lat2)
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d*1000)
}

result<-data.frame();

apply(tbllabel, 1, function(row) {
   
  startDate <- as.POSIXct(as.character(row["starttime"]));
  endDate <- as.POSIXct(as.character(row["endtime"]));
  
  
  ##cal speed and acc
  
  LocationClocationtime<-as.POSIXct(as.character(tbllocation[["txtDate"]]))
  subsetlocation<-subset(tbllocation,(LocationClocationtime>startDate)&(LocationClocationtime<endDate))
  sumacc<-0
  sumspeed<-0
  if (nrow(subsetlocation)>4){
    
    for (i in 3:nrow(subsetlocation)) {
      
      thedistance1<-earth.dist(subsetlocation[i-1,][["dblLon"]],subsetlocation[i-1,][["dblLat"]],subsetlocation[i-2,][["dblLon"]],subsetlocation[i-2,][["dblLat"]])
      thetime1<-difftime(strptime(subsetlocation[i-1,][["txtDate"]],"%Y-%m-%d %H:%M:%S"), strptime(subsetlocation[i-2,][["txtDate"]],"%Y-%m-%d %H:%M:%S"),units="secs")
      thespeed1<-as.numeric(thedistance1)/as.numeric(thetime1)
      if (is.nan(thespeed1)) thespeed1<-0
      
      thedistance2<-earth.dist(subsetlocation[i,][["dblLon"]],subsetlocation[i,][["dblLat"]],subsetlocation[i-1,][["dblLon"]],subsetlocation[i-1,][["dblLat"]])
      thetime2<-difftime(strptime(subsetlocation[i,][["txtDate"]],"%Y-%m-%d %H:%M:%S"), strptime(subsetlocation[i-1,][["txtDate"]],"%Y-%m-%d %H:%M:%S"),units="secs")
      thespeed2<-as.numeric(thedistance2)/as.numeric(thetime2)
      if (is.nan(thespeed2)|is.infinite(thespeed2)) thespeed2<-0
      
      theacc=(thespeed2-thespeed1)/as.numeric(thetime2)
      if (is.nan(theacc)|is.infinite(theacc)) {
        theacc<-0
        
      }
      ## print (paste("theacc ",theacc,"thetime2", thetime2))
      sumacc<-sumacc+theacc
      sumspeed<-sumspeed+thespeed2
    }
  }
  avgspeed<-sumspeed/(nrow(subsetlocation)-3)
  avgacc<-sumacc/(nrow(subsetlocation)-3)  

  ##cal variance
  Sensortime<-as.POSIXct(as.character(tblsensor[["txtDate"]]))
  subsetsensor<-subset(tblsensor,(Sensortime>startDate)&(Sensortime<endDate))
  ##print(subsetsensor)
   thedx=var(subsetsensor$dblX)
  thedy=var(subsetsensor$dblY)
  thedz=var(subsetsensor$dblZ)
   thedblmag=var(subsetsensor$dblMag)
  
  ## print (startDate)
 ## print(paste("startDate ",startDate,"endDate ",endDate,"thedx ",thedx,"thedy", thedy,"thedz", thedz,"thedblmag",thedblmag))

   if (is.na(thedx)|is.na(thedy)|is.na(thedz)|is.na(thedblmag)|is.na(avgacc)|is.na(avgspeed)){
       }
    else{    
          therecord1<-data.frame(userid="Mano1.5",starttime=row[["starttime"]],endtime=row[["endtime"]],labels=modeset[as.numeric(row[["mode"]])+1], 
                               dx=thedx, dy=thedy,dz=thedz,dmag=thedblmag,
                             avgspeed=avgspeed,avgacc=avgacc)
          therecord2<-data.frame(lon=subsetlocation["dblLon"],lat=subsetlocation["dblLat"])
          
      
          
          therecord3<-do.call("rbind", replicate(nrow(therecord2), therecord1, simplify = FALSE))
          
           newrecord<-cbind(therecord3,therecord2)
           print(newrecord)
          write.table(newrecord, file = "Mano1.5_processeddata.csv", col.names=F, append=T,sep = ",") 
          }
    
})
  
 


 