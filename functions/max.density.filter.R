 max.density.filter <- function(Data, name, act.opts,...) {
  # Data=temp
  # name="GCC";
  # act.opts<-data.frame(w=3)
  
  Data$daily.time<-as.POSIXct(strftime(Data$Date, format="%Y-%m-%d"))
  Data$doy<-format(Data$daily.time,format = '%j')
  
  w <- act.opts$w
  max.density.fun <- function(x) {
   # x=Data[,name][i:c(i+true.window-1)]
    if(sum(!is.na(x))>10){
      d=density(x, na.rm = TRUE)
      pos=match(max(d$y),d$y)
      max.x=d$x[pos]
      # close.x=min(abs(max.x-x))
      # pos.x=match(close.x,abs(max.x-x))
      # max.x=x[pos.x]
    }
    if(sum(!is.na(x))<=10){
      max.x=NA
    }
    return(max.x)
  }
  
  computed.frequency <- median(diff(as.numeric(Data$Date)), 
                               na.rm = TRUE)
  Data.per.day <- median(aggregate(Data, by = list(Data$daily.time), 
                                   FUN = length)[, 2], na.rm = T)
  true.window <- Data.per.day * w
  #n<-length(name)
  filtered.VIs<-data.frame(time=Data$Date,doy=Data$doy,daily.time=Data$daily.time);
  name_filtered<-names(filtered.VIs)
  
  ##The results from the rolling function
  # new.max<-rollapply(Data[, name],width = true.window, function(x) max.density.fun(x),  
  #                   fill = c('extend',NA,'extend'))
  # library(rowr)
  # new.max<-rollApply(Data[, name],max.density.fun,window = true.window,align = 'left')
  new.max<-c()
  for(i in 1:c(length(Data[,name])-true.window+1)){
    z=Data[,name][i:c(i+true.window-1)]
    max.x<-max.density.fun(z)
    new.max<-c(new.max,max.x)
  }
  new.max<-c(new.max,rep(new.max[length(new.max)],true.window-1))
  
  #
  filtered.VIs<-cbind(filtered.VIs,new.max)
  name_filtered<-c(name_filtered,paste(name,'.max_den.','filtered',sep=''))
  names(filtered.VIs)<-name_filtered
  rm(new.max)
  
  pos.time <- which(names(filtered.VIs) == "time" | names(filtered.VIs) == 
                      "doy" | names(filtered.VIs) == "daily.time")
  daily.agg <- aggregate(filtered.VIs[,-pos.time], by = list(filtered.VIs$daily.time), 
                         FUN = "median", na.rm = T)
  names(daily.agg) <- c("Date",paste0(name,'.max_den.','filtered'))
  return(daily.agg)
}
