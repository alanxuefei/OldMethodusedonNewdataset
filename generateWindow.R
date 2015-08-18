setwd("C:/Users/Alan/Desktop/SingtelProject/RProgramming/FromOldMethod/Script")
tblMode <- read.csv(file="Mano1.5_tblMode.csv",head=TRUE,stringsAsFactors=FALSE)
tblMode <- rbind(tblMode,tblMode[nrow(tblMode),])
tblMode$txtDate[nrow(tblMode)] <- strftime((as.POSIXlt(as.character(tblMode$txtDate[nrow(tblMode)]))+60*120),"%Y-%m-%d %H:%M:%S")
tempfinal<-as.POSIXlt(as.POSIXlt (as.character(tblMode$txtDate[nrow(tblMode)]))+60*120)
str(tempfinal)




tbllocation <- read.csv(file="Mano1.5_tblLocation.csv",head=TRUE,stringsAsFactors=FALSE)


startdatetime<-as.POSIXlt(as.character(tblMode[1,][["txtDate"]]))
finaldatetime<-as.POSIXlt(as.character(tblMode[nrow(tblMode),][["txtDate"]]))



LocationClocationtime<-as.POSIXct(as.character(tbllocation[["txtDate"]]))
i<-1
windowfile<-data.frame();
while (startdatetime<finaldatetime)  
{ 
  nextdatetime<-as.POSIXlt(startdatetime+600)
  nextrowdatetime<-as.POSIXlt(as.character(tblMode[i+1,][["txtDate"]]))
  if (nextdatetime>nextrowdatetime){
    nextdatetime<-nextrowdatetime
  }
  
  
  
  print(paste("start ",startdatetime,"end ",nextdatetime))
  subsetlocation<-subset(tbllocation,(LocationClocationtime>startdatetime)&(LocationClocationtime<nextdatetime))
  str(subsetlocation)
  if (nrow(subsetlocation)>4){
    theRecord<-data.frame(starttime=startdatetime,endtime=nextdatetime,mode=tblMode[i,][["intMode"]]);
    windowfile<-rbind(windowfile,theRecord)
  }
  
  nextstarttime<-as.POSIXlt(startdatetime+300)
  if (nextstarttime>nextrowdatetime){
    nextstarttime<-nextrowdatetime
    i<-i+1
  }
  startdatetime<<-nextstarttime
}  


write.csv(windowfile, "Mano1.5_windowfile.csv")

