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
    #print(val)
  }
  else{
    if(p<x[match(ed,x$Date),17]){
      val<-x[match(d,x$Date),10]
    }
    else{
      val<-(x[match(ed,x$Date),17]-p)
    }
    #print(val)
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
calcpnl<-function(x,d="15",pe=4.25){
  dates<-c(paste(d,"-Jan-2016",sep=""),paste(d,"-Feb-2016",sep=""),paste(d,"-Mar-2016",sep=""),paste(d,"-Apr-2016",sep=""),paste(d,"-May-2016",sep=""),paste(d,"-Jun-2016",sep=""),paste(d,"-Jul-2016",sep=""),paste(d,"-Aug-2016",sep=""),paste(d,"-Sep-2016",sep=""),paste(d,"-Oct-2016",sep=""),paste(d,"-Nov-2016",sep=""))
  expdates<-c("25-Feb-2016","31-Mar-2016","28-Apr-2016","26-May-2016","30-Jun-2016","28-Jul-2016","25-Aug-2016","29-Sep-2016","27-Oct-2016","24-Nov-2016","29-Dec-2016")
  p<-0
  c<-0
  for(i in 1:length(dates)){
    sp<-datapull(x,dates[i],pe)
    p<-p+getopprice(read.csv(paste("c",as.character(sp[1]),substring(expdates[i],4,6),".csv",sep="")),dates[i],expdates[i],sp[1],1)
    c<-c+getopprice(read.csv(paste("p",as.character(sp[2]),substring(expdates[i],4,6),".csv",sep="")),dates[i],expdates[i],sp[2],2)
    print(paste(substring(expdates[i],4,6),"put profit",getopprice(read.csv(paste("c",as.character(sp[1]),substring(expdates[i],4,6),".csv",sep="")),dates[i],expdates[i],sp[1],1)))
    print(paste(substring(expdates[i],4,6),"call profit",getopprice(read.csv(paste("p",as.character(sp[2]),substring(expdates[i],4,6),".csv",sep="")),dates[i],expdates[i],sp[2],2)))
  }
  return(p+c)
}