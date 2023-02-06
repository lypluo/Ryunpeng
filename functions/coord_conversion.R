##-------------------------------------
#Aim:make the conversion functions for the coordinates
##-------------------------------------
#longitude and latitude:deg,min,sec-->deg
coord_fun<-function(deg,min,sec){
  coord_final<-deg+min/60+sec/3600
  return(coord_final)
}

#example:
coord_fun(19,47,29.1)
