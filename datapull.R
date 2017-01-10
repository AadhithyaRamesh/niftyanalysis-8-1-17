datapull <- function(x,d,p){
  while(match(d,x$Date,nomatch = 0)==0){
    d<-format(as.Date(d,format = "%d-%b-%Y")+1,format="%d-%b-%Y")
  }
  cp<-round(x[match(d,x$Date),17]*(1 + (p/100)),-2)
  pp<-round(x[match(d,x$Date),17]*(1- (p/100)),-2)
  print(cp)
  print(pp)
  getopprice(y,d,cp,1)
  getopprice(z,d,pp,2)
}
getopprice(x,d,p,o){
  if(o==1){
    if(p>x[match(d,x$Date),17]){
      val<-x[match(d,x$Date),10]
      val
    }
    else{
       val<-(p-x[match(d,x$Date),17])
       val
    }
  }
  if(o==2){
    if(p<x[match(d,x$Date),17]){
      val<-x[match(d,x$Date),10]
      val
    }
    else{
      val<-(x[match(d,x$Date),17]-p)
      val
    }
  }
}