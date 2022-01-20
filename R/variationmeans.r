Getshiftsperrotation<- function() {
  g<- seq(from = 50, to = 480, by = 12)
  h<- seq(from = 61, to = 481, by = 12)
  h[36]<- h[36]-1
  rotation<- c()
  stuff<- c()
  
  
  for (i in 1:length(g)) {
    
    stuff[i]<- mean(VP_Data$meanreaches[g[i]:h[i]], na.rm = TRUE)
    rotation[i]<- variation_reaches$distortion[g[i]]
  }
  
  return(variation_prop<- data.frame(rotation, stuff))
}



Getreachesperrotation<- function() {
  g<- seq(from = 50, to = 480, by = 12)
  h<- seq(from = 61, to = 481, by = 12)
  h[36]<- h[36]-1
  rotation<- c()
  stuff<- c()
  
  
  for (i in 1:length(g)) {
    
    stuff[i]<- mean(VR_Data$meanreaches[g[i]:h[i]], na.rm = TRUE)
    rotation[i]<- variation_reaches$distortion[g[i]]
  }
  
  return(variation_reach<- data.frame(rotation, stuff))
}