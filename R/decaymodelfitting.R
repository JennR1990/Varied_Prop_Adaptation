decaymodelfitting<-function(){
  
  data<- getreachesformodel(variation_reaches)
  rotation<- data$distortion
  blocks<- c(rep(0, times = 48), sort(rep(1:36, times = 12)))
  transition<- colorRampPalette(c("green", "green4", "dark green"))
  colors<- transition(36) 
  schedule<- rep(-1,12)
  
  for (t in 1:480){
    
    if (rotation[t] == 15 | rotation[t] == 30){
      data[t,1]<- data[t,1]*-1
    } else if (rotation[t] == 360){
      #data[t,1]<- NA
    }
    
  }
  
  endpoints<-c()
  rotationsize<- c()
  for (i in 0:36){
    stop<- max(which(blocks==i))
    endpoints[i+1]<-as.numeric(unlist(data[stop,1]))
    rotationsize[i+1]<- as.numeric(unlist(data[stop,2]))
  }
  
  

  
  start<- min(which(blocks==1))
  stop<- max(which(blocks==1))
  plot(data[start:stop,1], type = "l", ylim = c(-20,30), xlab = "trials", ylab = "Hand Direction", col = colors[1], lwd = 2, main = rotationsize[2])
  
  reachsig<-data[start:stop,1]
  print(asymptoticDecayFit(schedule = schedule, signal = reachsig))
  
  
  for (i in 2:36){
    start<- min(which(blocks==i))
    stop<- max(which(blocks==i))
    plot(data[start:stop,1], type = "l", col = colors[i], ylim = c(-20,30), xlab = "trials in rotation", ylab = "Hand Direction", lwd = 2,main = rotationsize[i+1]) 
    reachsig<-data[start:stop,1]
    print(asymptoticDecayFit(schedule = schedule, signal = reachsig))
    print(endpoints[i+1])
  }

  
}