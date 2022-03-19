Getshiftsperrotation<- function() {
  g<- seq(from = 50, to = 480, by = 12)
  h<- seq(from = 61, to = 481, by = 12)
  h[36]<- h[36]-1
  rotation<- c()
  localizations<- c()
  VP_Data<- getreachesformodel(variation_localization)
  
  for (i in 1:length(g)) {
    
    localizations[i]<- mean(VP_Data$meanreaches[g[i]:h[i]], na.rm = TRUE)
    rotation[i]<- variation_reaches$distortion[g[i]]
  }
  
  return(variation_prop<- data.frame(rotation, localizations))
}



Getreachesperrotation<- function() {
  g<- seq(from = 50, to = 480, by = 12)
  h<- seq(from = 61, to = 481, by = 12)
  h[36]<- h[36]-1
  rotation<- c()
  stuff<- c()
  VR_Data<- getreachesformodel(variation_reaches)
  
  for (i in 1:length(g)) {
    
    stuff[i]<- mean(VR_Data$meanreaches[g[i]:h[i]], na.rm = TRUE)
    rotation[i]<- variation_reaches$distortion[g[i]]
  }
  
  return(variation_reach<- data.frame(rotation, stuff))
}


plotvariation<- function (){
  vprop<- Getshiftsperrotation()
  vreac<- Getreachesperrotation()
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
  plot(NULL, col = 'white', axes = F,cex.lab = 1.5,
       cex.main = 1.5,    xlab = "Trial",
       ylab = "Hand Location [°]", ylim = c(-30, 30), xlim = c(1,480))
  
  lines(x = z[1:25], y = sizes[1:25], type = 'l')
  lines(x = z[25:26], y = c(0,0), lty = 2)
  lines(x = z[26:33], y = sizes[26:33], type = 'l')
  lines(x = z[33:36], y = c(0,0,0,0), lty = 2)
  lines(x = z[36:51], y = sizes[36:51], type = 'l')
  lines(x = z[51:52], y = c(0,0), lty = 2)
  lines(x = z[52:61], y = sizes[52:61], type = 'l')
  lines(x = z[61:62], y = c(0,0), lty = 2)
  lines(x = z[62:71], y = sizes[62:71], type = 'l')
  lines(x = z[71:72], y = c(0,0), lty = 2)
  lines(x = z[73:74], y = sizes[73:74], type = 'l')
  
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
  reachdata<- getreachesformodel(variation_reaches)
  lines(reachdata$meanreaches*-1, type = 'l', col = 'Blue')
  locdata<- getreachesformodel(variation_localization)
  lines(locdata$meanreaches, type = 'l', col = 'red')
  dataCIs <- trialCI(data = variation_localization)
  dataCIs <- dataCIs
  x <-  c(c(1:480), rev(c(1:480)))
  y <- c(dataCIs[, 1], rev(dataCIs[, 2]))
  polygon(x, y, col = rgb(1,0,0,.2), border = NA)
  
  dataCIs <- trialCI(data = variation_reaches)
  dataCIs <- dataCIs*-1
  x <-  c(c(1:480), rev(c(1:480)))
  y <- c(dataCIs[, 1], rev(dataCIs[, 2]))
  polygon(x, y, col = rgb(0,0,1,.2), border = NA)
  
}