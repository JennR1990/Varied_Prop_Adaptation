


Getshiftsperrotationperp<- function(data) {
  g<- seq(from = 50, to = 480, by = 12)
  h<- seq(from = 61, to = 481, by = 12)
  h[36]<- h[36]-1
  rotation<- c()
  localizations<- c()
  VP_Data<- data
  
  for (i in 1:length(g)) {
    
    localizations[i]<- mean(VP_Data$meanreaches[g[i]:h[i]], na.rm = TRUE)
    rotation[i]<- data$distortion[g[i]]
  }
  
  return(variation_prop<- data.frame(rotation, localizations))
}


Getreachesperrotationperp<- function(data) {
  g<- seq(from = 50, to = 480, by = 12)
  h<- seq(from = 61, to = 481, by = 12)
  h[36]<- h[36]-1
  rotation<- c()
  stuff<- c()
  VR_Data<- data
  
  for (i in 1:length(g)) {
    
    stuff[i]<- mean(VR_Data$meanreaches[g[i]:h[i]], na.rm = TRUE)
    rotation[i]<- data$distortion[g[i]]
  }
  
  return(variation_reach<- data.frame(rotation, stuff))
}



plotvariationperp<- function (){
  source('E:/Jenn/Documents/Varied_Prop_Adaptation/R/shared.R')
  variation_localization<- read.csv("data/Localizations_Baselined.csv", header = TRUE)
  variation_reaches<- read.csv("data/Reaches_Baselined.csv", header = TRUE) 
  
  pdf("figs/Participant Curves.pdf", height = 10, width = 16)
  #layout(matrix(1:32,nrow = 8, byrow = TRUE))

  for (q in 2:33){
  
  loc_data<- data.frame(variation_localization$distortion, variation_localization[,q])
  names(loc_data)<- c("distortion", "meanreaches")
  
  reach_data<- data.frame(variation_reaches$distortion, variation_reaches[,q])
  names(reach_data)<- c("distortion", "meanreaches")
  
  
  
  vprop<- Getshiftsperrotationperp(loc_data)
  vreac<- Getreachesperrotationperp(reach_data)
  localizations<-vprop$localizations
  Variation_means<- cbind(vreac,localizations)
  
  
  g<- seq(from = 50, to = 480, by = 12)
  h<- seq(from = 61, to = 481, by = 12)
  h[36]<- h[36]-2
  
  z<-c(0,50)
  for (i in 1:36) {
    
    z<- c(z, g[i], h[i]+1)
  }
  
  
  sizes<- c(0,0)
  
  for (i in 1:36){
    
    sizes<- c(sizes,Variation_means$rotation[i],Variation_means$rotation[i]) 
    sizes[sizes == 360] <- NA
  }
  g<- seq(from = 50, to = 480, by = 12)
  g<- c(1,g,480)
  for (i in 1:length(sizes)){
    
    
    if (is.na(sizes[i])){
      sizes[i]<- 0
    }
    
  }
  
  title<- sprintf("p%d", q-1)
  plot(variation_localization[,q], col = "red", axes = F,cex.lab = 1.5,
       cex.main = 1.5,    xlab = "Trial",
       ylab = "Hand Location [°]", ylim = c(-30, 30), xlim = c(1,480), type = "l", main = title)
  lines(variation_reaches[,q]*-1, type = 'l', col = 'Blue')
  
  lines(x = z[1:25], y = sizes[1:25], type = 'l', lwd = 2)
  lines(x = z[25:26], y = c(0,0), lty = 2, lwd = 2)
  lines(x = z[26:33], y = sizes[26:33], type = 'l', lwd = 2)
  lines(x = z[33:36], y = c(0,0,0,0), lty = 2, lwd = 2)
  lines(x = z[36:51], y = sizes[36:51], type = 'l', lwd = 2)
  lines(x = z[51:52], y = c(0,0), lty = 2, lwd = 2)
  lines(x = z[52:61], y = sizes[52:61], type = 'l', lwd = 2)
  lines(x = z[61:62], y = c(0,0), lty = 2, lwd = 2)
  lines(x = z[62:71], y = sizes[62:71], type = 'l', lwd = 2)
  lines(x = z[71:72], y = c(0,0), lty = 2, lwd = 2)
  lines(x = z[73:74], y = sizes[73:74], type = 'l', lwd = 2)
  
  #lines(variation_reaches[,q]*-1, type = 'l', col = 'Blue')
  #lines(variation_localization[,q], type = 'l', col = 'red')
  
  legend(
    -5,
    30,
    legend = c(
      'Reaches',
      'Localizations'),
    col = c('blue', 'red'),
    lty = c(1),
    
    
    lwd = c(2),
    bty = 'n', 
    cex = 1.2
  )
  axis(2, at = c(-30, -15, 0, 15, 30), cex.axis = 1.5,
       las = 2)
  axis(1, at = g, cex.axis = .75, las = 2)
}
dev.off()
  
}