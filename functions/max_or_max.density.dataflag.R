##Bascially, the results of quality flag of the max.density or max filter is the same
##Hence, we only need one script to calculate the flag
max.dataflag <- function(Data, name, act.opts,...) {
   # Data=Data_subset;
   # name="NDVI";
  ###Actually,the (max/max.density).filter use 4 days data
  ###Hence use the data within four days to set the dataflag
  act.opts<-data.frame(w=4)
  Data$daily.time<-as.POSIXct(strftime(Data$Date, format="%Y-%m-%d"))
  Data$doy<-format(Data$daily.time,format = '%j')
  
  w <- act.opts$w
  computed.frequency <- median(diff(as.numeric(Data$Date)), 
                               na.rm = TRUE)
  Data.per.day <- median(aggregate(Data, by = list(Data$daily.time), 
                                   FUN = length)[, 2], na.rm = T)
  true.window <- Data.per.day * w
  
  #####count how many days
  nday<-length(aggregate(Data, by = list(Data$daily.time), 
            FUN = length)[,1])

  data_flag<-c()
  for(i in 1:c(nday-w+1)){
    z=Data[,name][c((i-1)*10+1):c((i-1)*10+true.window)]
    n.na<-sum(is.na(z))
    ratio<-n.na/true.window
    if(ratio<0.25){flag<-'0'}
    if(c(ratio>=0.25)&c(ratio<0.5)){flag<-'1'}
    if(c(ratio>=0.5)&c(ratio<0.75)){flag<-'2'}
    if(ratio>=0.75){flag<-'3'}
    data_flag<-c(data_flag,flag)
  }
  data_flag<-c(data_flag,rep(data_flag[length(data_flag)],w-1))
  
  #summary the datasets
  Daily.date<-aggregate(Data, by = list(Data$daily.time), 
                         FUN = length)[,1]
  
  daily.agg <- data.frame(Daily.date,data_flag)  
  names(daily.agg) <- c("Date",'qfCamVIs')
  return(daily.agg)
}
