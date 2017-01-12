getopprice<-function(x,d,ed,p,o){
  while(match(d,x$Date,nomatch = 0)==0){
    d<-format(as.Date(d,format = "%d-%b-%Y")+1,format="%d-%b-%Y")
    #print(d)
  }
  if(o==1){
    #print(p)
    #print(x[match(ed,x$Date),17])
    if(p>x[match(ed,x$Date),17]){
      val<-x[match(d,x$Date),10]
    }
    else{
      val<-(p-x[match(ed,x$Date),17])
    }
    print(val)
  }
  else{
    if(p<x[match(ed,x$Date),17]){
      val<-x[match(d,x$Date),10]
    }
    else{
      val<-(x[match(ed,x$Date),17]-p)
    }
    print(val)
  }
  val
}
datapull <- function(x,d,p){
  while(match(d,x$Date,nomatch = 0)==0){
    d<-format(as.Date(d,format = "%d-%b-%Y")+1,format="%d-%b-%Y")
    #print(d)
  }
  op<-c(round(x[match(d,x$Date),2]*(1 + (p/100)),-2),round(x[match(d,x$Date),2]*(1- (p/100)),-2))
  #print(op)
  return(op)
}
calcpnl<-function(x){
  dates<-c("15-Jan-2016","15-Feb-2016","15-Mar-2016","15-Apr-2016","15-May-2016","15-Jun-2016","15-Jul-2016","15-Aug-2016","15-Sep-2016","15-Oct-2016","15-Nov-2016")
  expdates<-c("25-Feb-2016","31-Mar-2016","28-Apr-2016","26-May-2016","30-Jun-2016","28-Jul-2016","25-Aug-2016","29-Sep-2016","27-Oct-2016","24-Nov-2016","29-Dec-2016")
  #c7800Feb<-read.csv("C:/github local reps/niftyanalysis-8-1-17/nifty2016feb.csv")
  #print(c7800Feb)
  p<-0
  for(i in 1:length(dates)){
    #print(i)
    sp<-datapull(x,dates[i],4.25)
    p<-p+getopprice(read.csv(paste("C:/github local reps/niftyanalysis-8-1-17/","c",as.character(sp[1]),substring(expdates[i],4,6),".csv",sep="")),dates[i],expdates[i],sp[1],1)+getopprice(read.csv(paste("C:/github local reps/niftyanalysis-8-1-17/","p",as.character(sp[2]),substring(expdates[i],4,6),".csv",sep="")),dates[i],expdates[i],sp[2],2)
    print(p)
  }
  #p<-getopprice(read.csv(paste("C:/github local reps/niftyanalysis-8-1-17/nifty2016feb.csv")),dates[1],7800,1)
  return(p)
}