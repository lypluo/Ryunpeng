SplineFilter_thengap_f<-function(P_Ori){
  library(phenopix)
  ##do primary gapfilling in order to enable SplineFit function afterwards
  ##do not use SSA method as sometimes it extrapolates too much
  #P_Ori<-N_GRA_rescal$NDVI.max.filtered
  
  #in case some NA at the beginning of the time series,use na.fill for the beginning of gapfilling
  temp1<-na.fill(P_Ori[1:30],fill='extend')
  temp2<-na.spline(P_Ori[31:length(P_Ori)])
  P<-c(temp1,temp2)
  
  ##########transfer the date format to zoo
  for(i in 1:20){
    P<-zoo(P,order.by = index(1:length(P)))
    ####delete the data which bigger than (SplineFit-sd*2,SplineFit+sd*4)
    fitResult<-SplineFit(P,nrep=100,df.factor = 0.05)
    residuals<-as.numeric(P)-as.vector(fitResult$fit$predicted)
    sd.res<-sd(residuals,na.rm=TRUE)
    selectcriterion_above<-mean(residuals)+4*sd.res
    selectcriterion_below<-mean(residuals)-2*sd.res
    noise<-residuals[residuals>=selectcriterion_above|residuals<=selectcriterion_below]
    matchNum<-match(noise,residuals)
    if(length(matchNum)==0){
      break
    }
    P[matchNum]<-NA
    P_NA1<-as.numeric(P)
    ###gapfilling with na.fill function
    temp1<-na.fill(P_NA1,fill='extend')
    P_new<-temp1
    P<-P_new
  }
  
  return(P)
}

