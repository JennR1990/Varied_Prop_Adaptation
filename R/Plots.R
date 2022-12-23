VariationTcombine<- function(data, give = "RM"){
  Demos<- read.csv("data/All_demographics.csv", header = TRUE)
  start<- seq(from = 50, to = 480, by = 12)
  start<- start+8 #this means i am taking the last 4 trials from each rotation, making this smaller means i take more trials.
  stop<- seq(from = 61, to = 481, by = 12)
  stop[36]<- stop[36]-2
  
  V_RM<- data.frame(rep(NA, times = 33))
  
  for (i in 1:36){
    V_RM[,i]<-colMeans(data[start[i]:stop[i],], na.rm = TRUE)
  }
  
  
  aligned<- colMeans(data[46:49,2:33], na.rm = TRUE)
  rotation<-c(0,as.numeric(V_RM[1,]))
  colnames(V_RM)<- c(as.character(V_RM[1,]))
  V_RM<- V_RM[-1,]
  V_RM<- cbind(aligned, V_RM, Demos[Demos$Experiment == "Variation",1:2])
  
  print(t.test(V_RM$aligned[V_RM$Sex == "M"],V_RM$aligned[V_RM$Sex == "F"]))
  
  sprintf("There are %.f Males out of %.f subjects", sum(data$Sex == "M"),sum(nrow(data)))
  
  
  if (give == "RM") {
    return(V_RM)
  }else {
    
    return(rotation)
  }
  
}


plotblocks<- function (){
  
  variation_reaches<- read.csv("data/Reaches_Baselined.csv", header = TRUE)   
  
  rotation<- variation_reaches$distortion
  for (t in 1:480){
    
    if (rotation[t] > 0){
      variation_reaches[t,2:33]<- variation_reaches[t,2:33]*-1
    } else if (rotation[t] == -360){
      variation_reaches[t,2:33]<- 0
    }
    
    
  }
  
  blocks<- c(rep(0, times = 48), sort(rep(1:36, times = 12)))
  
  
  start<- min(which(blocks==1))
  stop<- max(which(blocks==1))
  plot(rowMeans(variation_reaches[start:stop,2:33]/30*100, na.rm = TRUE), type = "l", ylim = c(-30,100), xlab = "trials", ylab = "Hand Direction")
  
  for (i in 2:36){
    endpoint<-as.numeric(unlist(rowMeans(variation_reaches[stop,2:33], na.rm = TRUE)))
    start<- min(which(blocks==i))
    stop<- max(which(blocks==i))
    
    if (abs(rotation[stop]) == 15| abs(rotation[stop]) == 30 ){
      lines((((rowMeans(variation_reaches[start:stop,2:33], na.rm = TRUE)-endpoint)*-1)/abs(rotation[stop])*100), type = "l", col = "Blue") 
      print(abs(rotation[stop]))
      print((((rowMeans(variation_reaches[start:stop,2:33], na.rm = TRUE)-endpoint)*-1)/abs(rotation[stop])*100))
      
    } else if (abs(rotation[stop]) == 0) {
      lines((((rowMeans(variation_reaches[start:stop,2:33], na.rm = TRUE)-endpoint)*-1)/abs(rotation[stop-12])*100), type = "l", col = "Blue")
      print(abs(rotation[stop]))
      print((((rowMeans(variation_reaches[start:stop,2:33], na.rm = TRUE)-endpoint)*-1)/abs(rotation[stop-12])*100))
      
    } else{
      print("clamp trials")
    }
  }
  
}
plotblocks<- function (alldata){
  
  data<- getreachesformodel(alldata)
  data$x<- c(1:49, rep(1:12,times=35), 1:11)
  rotation<- data$distortion
  blocks<- c(rep(0, times = 48), sort(rep(1:36, times = 12)))
  transition<- colorRampPalette(c("green", "green4", "dark green"))
  colors<- transition(36) 
  
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
  for (rot in 1:37){
    
    if (rotationsize[rot] == rotationsize[rot+1])
      print("same")
  }
  
  svglite(file='figs/blocks.svg', width=15, height=21, system_fonts=list(sans = "Arial"))
  layout(matrix(c(1:40), nrow=8, byrow=TRUE), heights=c(1,1), widths = c(1,1))
  start<- min(which(blocks==1))
  stop<- max(which(blocks==1))
  plot(data[start:stop,1], type = "l", ylim = c(-20,30), xlab = "trials", ylab = "Hand Direction", col = colors[1], lwd = 2, main = rotationsize[2])
  
  
  for (i in 2:36){
    start<- min(which(blocks==i))
    stop<- max(which(blocks==i))
    plot(data[start:stop,1], type = "l", col = colors[i], ylim = c(-20,30), xlab = "trials in rotation", ylab = "Hand Direction", lwd = 2,main = rotationsize[i+1]) 
    print(endpoints[i+1])
  }
  dev.off()
  
  
}


Getshiftsperrotation<- function(data) {
  g<- seq(from = 50, to = 480, by = 12)
  h<- seq(from = 61, to = 481, by = 12)
  h[36]<- h[36]-1
  rotation<- c()
  localizations<- c()
  VP_Data<- getreachesformodel(data)
  
  for (i in 1:length(g)) {
    
    localizations[i]<- mean(VP_Data$meanreaches[g[i]:h[i]], na.rm = TRUE)
    rotation[i]<- data$distortion[g[i]]
  }
  
  return(variation_prop<- data.frame(rotation, localizations))
}


Getreachesperrotation<- function(data) {
  g<- seq(from = 50, to = 480, by = 12)
  h<- seq(from = 61, to = 481, by = 12)
  h[36]<- h[36]-1
  rotation<- c()
  stuff<- c()
  VR_Data<- getreachesformodel(data)
  
  for (i in 1:length(g)) {
    
    stuff[i]<- mean(VR_Data$meanreaches[g[i]:h[i]], na.rm = TRUE)
    rotation[i]<- data$distortion[g[i]]
  }
  
  return(variation_reach<- data.frame(rotation, stuff))
}


plotvariation<- function (){
  source('R/shared.R')
  variation_localization<- read.csv("data/variation_localization.csv", header = TRUE)
  variation_reaches<- read.csv("data/variation_reaches.csv", header = TRUE) 
  
  vprop<- Getshiftsperrotation(variation_localization)
  vreac<- Getreachesperrotation(variation_reaches)
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
       cex.main = 1.5,
       ylab = "Hand Location [°]", ylim = c(-30, 30), xlim = c(1,480), xlab = "", xaxt = 'n')
  
  
  
  lines(x = z[1:25], y = sizes[1:25], type = 'l')
  lines(x = z[25:26], y = c(0,0), lty = 2, col = "Dark Grey")
  lines(x = z[26:33], y = sizes[26:33], type = 'l')
  lines(x = z[33:36], y = c(0,0,0,0), lty = 2, col = "Dark Grey")
  lines(x = z[36:51], y = sizes[36:51], type = 'l')
  lines(x = z[51:52], y = c(0,0), lty = 2, col = "Dark Grey")
  lines(x = z[52:61], y = sizes[52:61], type = 'l')
  lines(x = z[61:62], y = c(0,0), lty = 2, col = "Dark Grey")
  lines(x = z[62:71], y = sizes[62:71], type = 'l')
  lines(x = z[71:72], y = c(0,0), lty = 2, col = "Dark Grey")
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
  
  # legend(
  #   -5,
  #   30,
  #   legend = c(
  #     'Participant controls cursor',
  #     'Clamped cursor'),
  #   col = c('black', 'dark grey'),
  #   lty = c(1,2),
  #   lwd = c(2),
  #   bty = 'n', 
  #   cex = 1.2
  # )
  # 
  axis(2, at = c(-30, -15, 0, 15, 30), cex.axis = 1.5,
       las = 2)
  g<- c(seq(from = 50, to = 480, by = 48), 480)
  axis(1, at = c(1,g), cex.axis = 1.25)
  locations<- seq(from = 50, to=470, by=12)
  locations[]
  
  axis(1, at =locations, labels = c(1:36), cex.axis = 1, line = 2.5, las = 2 )
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


Plotlearningovertimebyblock<- function(){
  variation_localization<- read.csv("data/Localizations_Baselined.csv", header = TRUE)
  variation_reaches<- read.csv("data/Reaches_Baselined.csv", header = TRUE)
  
  ##pull in the necessary data to make the plots
  Vrot<-VariationTcombine(variation_reaches,1)
  V_RM<-VariationTcombine(variation_reaches)
  V_PM<-VariationTcombine(variation_localization)
  locs<-colMeans(V_PM[,1:37], na.rm = TRUE)
  reachs<-colMeans(V_RM[,1:37], na.rm = TRUE)
  
  #plots reaches ~ prop for last four trials of each rotation
  plot(reachs, locs, main = "Last four trials of each rotation", xlab = "Reaches", ylab = "Localizations")
  
  
  
  #Make all the data go in the same direction, some rotations are positive
  
  for (i in 1:length(reachs)){
    if (reachs[i] <0){
      
      reachs[i]<-reachs[i]*-1
    } else{
      #print("Done")
    }
    
  }
  
  for (i in 1:length(reachs)){
    if (locs[i] <0){
      
      locs[i]<-locs[i]*-1
    } else{
      # print("Done")
    }
    
  }
  
  for (i in 1:length(reachs)){
    if (Vrot[i] <0){
      
      Vrot[i]<-Vrot[i]*-1
    } else{
      #print("Done")
    }
    
  }
  
  #plots reaches ~ prop for last four trials of each rotation, but everything is positive
  plot(reachs, locs, main = "Last four trials of each rotation", xlab = "Reaches", ylab = "Localizations")
  
  #Remove "INF" and O compensation trials, this wy it only includes trials that had a rotation
  
  for (i in 1:length(reachs)) locs[i]<-round((locs[i]/Vrot[i])*100)
  percents<-as.numeric(unlist(locs[locs>5]))
  percents<-as.numeric(unlist(percents[percents<100]))
  for (i in 1:length(reachs)) reachs[i]<-round((reachs[i]/Vrot[i])*100)
  REpercents<-as.numeric(unlist(reachs[reachs>5]))
  REpercents<-as.numeric(unlist(REpercents[REpercents<100]))
  
  
  
  #Plot the two separate lines that show how much they compensate over time. 
  
  plot(x = seq(from = 1, to = length(percents)), y = percents, type = "l", 
       xlab = "Time", ylab = "Compensation [%]", ylim = c(0,100), col = "Red",
       axes = FALSE, cex.lab = 1.5, main = "Last four trials of each rotation")
  axis(2, at = c(0, 20, 40, 60, 80, 100), cex.axis = 1.5,
       las = 2)
  #axis(1,labels = c(50, 265, 480) ,at = c(1,12,24), cex.axis = 1.5 )
  lines(x = seq(from = 1, to = length(REpercents)), y = REpercents, col= "Blue")
  legend(
    1,
    15,
    legend = c(
      'Reaches',
      'Localizations'),
    col = c('blue', 'red'),
    lty = c(1),
    
    
    lwd = c(2),
    bty = 'n', 
    cex = 1.2
  )
  
  
  
  #Run regression comparing the percent compensation to the values 1:24. 
  reachreg<-lm(REpercents~seq(from = 1, to = length(REpercents)))
  print(summary(reachreg))
  locreg<-lm(percents~seq(from = 1, to = length(percents)))
  print(summary(locreg))
  
  ##adding lines of best fit to plot
  abline(lm(REpercents~seq(from = 1, to = length(REpercents))), col = "Blue", lty = 3)
  abline(lm(percents~seq(from = 1, to = length(percents))), col = "Red", lty = 3 )
}



Plotlearningovertimealltrials<- function(){
  variation_localization<- read.csv("data/Localizations_Baselined.csv", header = TRUE)
  variation_reaches<- read.csv("data/Reaches_Baselined.csv", header = TRUE)  
  
  ##pull in the necessary data to make the plots
  Vrot<- variation_reaches[,1]
  reachs<-rowMeans(variation_reaches[,2:33], na.rm = TRUE)
  locs<-rowMeans(variation_localization[,2:33], na.rm = TRUE)
  
  
  #plots reaches ~ prop for last four trials of each rotation
  plot(reachs, locs, main = "All trials", xlab = "Reaches", ylab = "Localizations")
  
  
  
  #Make all the data go in the same direction, some rotations are positive
  
  for (i in 1:length(reachs)){
    if (reachs[i] <0){
      
      reachs[i]<-reachs[i]*-1
    } else{
      #print("Done")
    }
    
  }
  
  for (i in 1:length(reachs)){
    if (locs[i] <0){
      
      locs[i]<-locs[i]*-1
    } else{
      # print("Done")
    }
    
  }
  
  for (i in 1:length(reachs)){
    if (Vrot[i] <0){
      
      Vrot[i]<-Vrot[i]*-1
    } else{
      #print("Done")
    }
    
  }
  
  #plots reaches ~ prop for last four trials of each rotation, but everything is positive
  plot(reachs, locs, main = "Last four trials of each rotation", xlab = "Reaches", ylab = "Localizations")
  
  #Remove 360 and O compensation trials, this way it only includes trials that had a rotation
  
  rot<- c()
  reach<- c()
  loc<- c()
  for (i in 1:length(reachs)){
    
    if (Vrot[i] == 360 || Vrot[i] == 0){
      
      print("bad") 
    } else {
      rot[i]<- Vrot[i]
      reach[i]<- reachs[i]
      loc[i]<- locs[i]
    }
  }
  rot<- rot[!is.na(rot)]
  loc<- loc[!is.na(loc)]
  reach<- reach[!is.na(reach)]
  
  for (i in 1:length(reach)) loc[i]<-round((loc[i]/rot[i])*100)
  percents<-as.numeric(unlist(loc[!is.na(loc)]))
  percents<-loc
  #percents<-as.numeric(unlist(percents[percents<100]))
  for (i in 1:length(reach)) reach[i]<-round((reach[i]/rot[i])*100)
  REpercents<- reach
  REpercents<-as.numeric(unlist(reach[!is.na(reach)]))
  #REpercents<-as.numeric(unlist(REpercents[REpercents<100]))
  
  
  
  #Plot the two separate lines that show how much they compensate over time. 
  
  plot(x = seq(from = 1, to = length(percents)), y = percents, type = "l", 
       xlab = "Time", ylab = "Percent Compensation", ylim = c(0,100), col = "Red",
       axes = FALSE, cex.lab = 1.5)
  
  axis(2, at = c(0, 20, 40, 60, 80, 100), cex.axis = 1.5,
       las = 2)
  #axis(1,labels = c(50, 265, 480) ,at = c(1,12,24), cex.axis = 1.5 )
  lines(x = seq(from = 1, to = length(REpercents)), y = REpercents, col= "Blue")
  legend(
    1,
    15,
    legend = c(
      'Reaches',
      'Localizations'),
    col = c('blue', 'red'),
    lty = c(1),
    
    
    lwd = c(2),
    bty = 'n', 
    cex = 1.2
  )
  
  
  #Run regression comparing the percent compensation to the values 1:24. 
  reachreg<-lm(REpercents~seq(from = 1, to = length(REpercents)))
  print(summary(reachreg))
  locreg<-lm(percents~seq(from = 1, to = length(percents)))
  print(summary(locreg))
  
  ##adding lines of best fit to plot
  abline(lm(REpercents~seq(from = 1, to = length(REpercents))), col = "Blue", lty = 3)
  abline(lm(percents~seq(from = 1, to = length(percents))), col = "Red", lty = 3 )
}


##  Plotting reaches and localization data separately for all changes in the rotation size (including sign) ##
prepdataforplotting<- function(data){
  
  data<-data.frame(data$distortion, data)
  
  data$distortion[data$distortion == 360] <- 0
  
  start<- seq(from = 50, to = 470, by=12)
  stop<- c(seq(from = 62, to = 480, by=12), 480)
  
  t_1<- c()
  t0<- c()
  tdif<- c()
  
  t_1<- 0
  t0<- data$distortion[50]
  trials<- NA
  
  
  for (i in 1:length(start) ) {
    t_1<- c(t_1, data$distortion[start[i]])
    t0<- c(t0,data$distortion[stop[i]])
    
    trials<-c(trials, sprintf("%d to %d", start[i], stop[i]-1))
  }
  
  
  tdif<- t_1 - t0
  trials<- c(trials[-1])
  rotdif<- data.frame(t_1[-37],t0[-37],tdif[-37], trials)
  
  for (i in 1:nrow(rotdif)) {
    if (rotdif$t0..37.[i] == 360)
      rotdif[i, ]<- NA
  }
  
  idx<-which(is.na(rotdif$trials))
  rotdif<- rotdif[-idx,]
  difs<-unique(rotdif$tdif..37.)
  
  diftrials<- data.frame(matrix(NA, nrow = 4, ncol = length(difs)))
  colnames(diftrials)<- difs
  
  diftrials[,1]<- c(rotdif$trials[rotdif$tdif..37. == difs[1]], NA)
  
  for (i in 2:3){
    diftrials[,i]<-rotdif$trials[rotdif$tdif..37. == difs[i]]
  }
  
  diftrials[,4]<- c(rotdif$trials[rotdif$tdif..37. == difs[4]], NA)
  
  zerodiftrials<<- c(rotdif$trials[rotdif$tdif..37. == difs[5]], NA)
  
  for (i in 6:length(difs)){
    diftrials[,i]<-rotdif$trials[rotdif$tdif..37. == difs[i]]
  }
  return(diftrials)
}
plotreachesperchange<-function(task = "reaches"){
  
  
  variation_localization<- read.csv("data/Localizations_Baselined.csv", header = TRUE)
  variation_reaches<- read.csv("data/Reaches_Baselined.csv", header = TRUE) 
  
  if (task == "prop"){
    
    alldata<- variation_localization
    scale = 1
    scale2 = -1
    scale3 = 1
    
  } else {
    
    alldata<- variation_reaches
    scale = -1
    scale2 = 1
    scale3 = -1
  }
  
  diftrials<- prepdataforplotting(variation_reaches)
  title<- sprintf("%s based on rotation size change", task)
  
  startloc<-as.numeric(unlist(strsplit(diftrials[1,1], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[1,1], " to ")))[2]
  data<- alldata[startloc:stoploc,2:33]
  
  startloc<-as.numeric(unlist(strsplit(diftrials[2,1], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[2,1], " to ")))[2]
  data<- cbind(data,alldata[startloc:stoploc,2:33])
  
  startloc<-as.numeric(unlist(strsplit(diftrials[3,1], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[3,1], " to ")))[2]
  data<- cbind(data,alldata[startloc:stoploc,2:33])
  
  plot(rowMeans(data, na.rm = TRUE)*scale2, type = "l", col = "chartreuse",xlim = c(0,12.5),ylim = c(-15,25), xlab = "Trials in a Block", ylab = "Hand Deviation", main = title, lwd = 2)
  text(x = 12.5, y = ((rowMeans(data, na.rm = TRUE))*scale2)[12], "30",col = "chartreuse")
  
  
  startloc<-as.numeric(unlist(strsplit(diftrials[1,4], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[1,4], " to ")))[2]
  data<- alldata[startloc:stoploc,2:33]
  
  startloc<-as.numeric(unlist(strsplit(diftrials[2,4], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[2,4], " to ")))[2]
  data<- cbind(data,alldata[startloc:stoploc,2:33])
  
  startloc<-as.numeric(unlist(strsplit(diftrials[3,4], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[3,4], " to ")))[2]
  data<- cbind(data,alldata[startloc:stoploc,2:33])
  
  lines((rowMeans(data, na.rm = TRUE))*scale, type = "l", col = "chartreuse4", lty = 5, lwd = 2)
  text(x = 12.5, y = ((rowMeans(data, na.rm = TRUE))*scale)[12], "-30",col = "chartreuse4")
  
  
  startloc<-as.numeric(unlist(strsplit(diftrials[1,2], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[1,2], " to ")))[2]
  data<- alldata[startloc:stoploc,2:33]
  
  startloc<-as.numeric(unlist(strsplit(diftrials[2,2], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[2,2], " to ")))[2]
  data<- cbind(data,alldata[startloc:stoploc,2:33])
  
  startloc<-as.numeric(unlist(strsplit(diftrials[3,2], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[3,2], " to ")))[2]
  data<- cbind(data,alldata[startloc:stoploc,2:33])
  
  startloc<-as.numeric(unlist(strsplit(diftrials[4,2], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[4,2], " to ")))[2]
  data<- cbind(data,alldata[startloc:stoploc,2:33])
  
  lines((rowMeans(data, na.rm = TRUE))*scale, type = "l", col = "blue", lty = 5, lwd = 2)
  text(x = 12.5, y = ((rowMeans(data, na.rm = TRUE))*scale)[12], "-15", col = "blue")
  
  startloc<-as.numeric(unlist(strsplit(diftrials[1,3], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[1,3], " to ")))[2]
  data<- alldata[startloc:stoploc,2:33]
  
  startloc<-as.numeric(unlist(strsplit(diftrials[2,3], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[2,3], " to ")))[2]
  data<- cbind(data,alldata[startloc:stoploc,2:33])
  
  startloc<-as.numeric(unlist(strsplit(diftrials[3,3], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[3,3], " to ")))[2]
  data<- cbind(data,alldata[startloc:stoploc,2:33])
  
  startloc<-as.numeric(unlist(strsplit(diftrials[4,3], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[4,3], " to ")))[2]
  data<- cbind(data,alldata[startloc:stoploc,2:33])
  
  lines((rowMeans(data, na.rm = TRUE))*scale2, type = "l", col = "cyan", lwd = 2)
  text(x = 12.5, y = ((rowMeans(data, na.rm = TRUE))*scale2)[12], "15", col = "cyan")
  
  
  startloc<-as.numeric(unlist(strsplit(diftrials[1,6], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[1,6], " to ")))[2]
  data<- alldata[startloc:stoploc,2:33]
  
  startloc<-as.numeric(unlist(strsplit(diftrials[2,6], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[2,6], " to ")))[2]
  data<- cbind(data,alldata[startloc:stoploc,2:33])
  
  lines((rowMeans(data, na.rm = TRUE))*scale2, type = "l", col = "darkorchid1", lwd = 2)
  text(x = 12.5, y = ((rowMeans(data, na.rm = TRUE))*scale2)[12], "45", col = "darkorchid1")
  
  startloc<-as.numeric(unlist(strsplit(diftrials[1,9], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[1,9], " to ")))[2]
  data<- alldata[startloc:stoploc,2:33]
  
  startloc<-as.numeric(unlist(strsplit(diftrials[2,9], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[2,9], " to ")))[2]
  data<- cbind(data,alldata[startloc:stoploc,2:33])
  
  lines((rowMeans(data, na.rm = TRUE))*scale, type = "l", col = "darkorchid4", lty = 5, lwd = 2)
  text(x = 12.5, y = ((rowMeans(data, na.rm = TRUE))*scale)[12], "-45", col = "darkorchid4")
  
  startloc<-as.numeric(unlist(strsplit(diftrials[1,13], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[1,13], " to ")))[2]
  data<- alldata[startloc:stoploc,2:33]
  lines((rowMeans(data, na.rm = TRUE))*scale2, type = "l", col = "deeppink4", lwd = 2)
  text(x = 12.5, y = ((rowMeans(data, na.rm = TRUE))*scale2)[12], "60", col = "deeppink4")
  
  startloc<-as.numeric(unlist(strsplit(diftrials[1,11], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[1,11], " to ")))[2]
  data<- alldata[startloc:stoploc,2:33]
  lines((rowMeans(data, na.rm = TRUE))*scale, type = "l", col = "deeppink", lty = 5, lwd = 2)
  text(x = 12.5, y = ((rowMeans(data, na.rm = TRUE))*scale)[12], "-60", col = "deeppink")
  
  
  
  startloc<-as.numeric(unlist(strsplit(zerodiftrials[2], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(zerodiftrials[2], " to ")))[2]
  data<- alldata[startloc:stoploc,2:33]
  #data<- alldata[startloc:stoploc,2:33]- modelendpoints[startloc-1 ==trialpoints]
  print(mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))
  lines((rowMeans(data, na.rm = TRUE)*scale), type = "l", col = "cadetblue1", lty = linetype2, lwd = 2)
  text(x = 12.5, y = ((rowMeans(data, na.rm = TRUE))*scale)[12], "15", col = "cadetblue1")
  
  startloc<-as.numeric(unlist(strsplit(zerodiftrials[3], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(zerodiftrials[3], " to ")))[2]
  data<- alldata[startloc:stoploc,2:33]
  #data<- alldata[startloc:stoploc,2:33]- modelendpoints[startloc-1 ==trialpoints]
  print(mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))
  lines((rowMeans(data, na.rm = TRUE)*scale2), type = "l", col = "chartreuse", lty = linetype2, lwd = 2)
  text(x = 12.5, y = ((rowMeans(data, na.rm = TRUE))*scale2)[12], "-30", col = "chartreuse")
  
  startloc<-as.numeric(unlist(strsplit(zerodiftrials[4], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(zerodiftrials[4], " to ")))[2]
  data<- alldata[startloc:stoploc,2:33]
  #data<- alldata[startloc:stoploc,2:33]- modelendpoints[startloc-1 ==trialpoints]
  print(mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))
  lines((rowMeans(data, na.rm = TRUE)*scale2), type = "l", col = "cadetblue1", lty = linetype2, lwd = 2)
  text(x = 12.5, y = ((rowMeans(data, na.rm = TRUE))*scale2)[12], "-15", col = "cadetblue1")
  
  startloc<-as.numeric(unlist(strsplit(zerodiftrials[5], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(zerodiftrials[5], " to ")))[2]
  data<- alldata[startloc:stoploc,2:33]
  #data<- alldata[startloc:stoploc,2:33]- modelendpoints[startloc-1 ==trialpoints]
  print(mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))
  lines((rowMeans(data, na.rm = TRUE)*scale), type = "l", col = "chartreuse", lty = linetype2, lwd = 2)
  text(x = 12.5, y = ((rowMeans(data, na.rm = TRUE))*scale)[12], "30", col = "chartreuse")
  
  #legend(x = 8, y = 0, legend = c("30", "-30", "15","-15","45", "-45","60", "-60"), col = c("chartreuse", "chartreuse4", "cyan", "blue", "darkorchid1", "darkorchid4", "deeppink", "deeppink4"), lty = c(1,5,1,5,1,5,1,5), lwd = (1), bty = 'n', ncol = 2)
  
}



##  Plotting reaches and localization data separately for absolute changes in the rotation size (regardless of sign) ##
prepdataforabsplotting<- function(data){
  
  data<-data.frame(data$distortion, data)
  
  data$distortion[data$distortion == 360] <- 0
  
  start<- seq(from = 50, to = 470, by=12)
  stop<- c(seq(from = 62, to = 480, by=12), 480)
  
  t_1<- c()
  t0<- c()
  tdif<- c()
  
  t_1<- 0
  t0<- data$distortion[50]
  trials<- NA
  oldrot<-data$data.distortion[50]
  
  
  for (i in 1:length(start) ) {
    t_1<- c(t_1, data$distortion[start[i]])
    t0<- c(t0,data$distortion[stop[i]])
    oldrot<- c(oldrot,data$data.distortion[stop[i]])
    
    trials<-c(trials, sprintf("%d to %d", start[i], stop[i]-1))
  }
  
  
  tdif<- t_1 - t0
  trials<- c(trials[-1])
  rotdif<- data.frame(t_1[-37],t0[-37],tdif[-37], trials, oldrot[-37])
  
  # for (i in 1:nrow(rotdif)) {
  #   if (rotdif$t0..37.[i] == 360)
  #     rotdif[i, ]<- NA
  # }
  # 
  # idx<-which(is.na(rotdif$trials))
  # rotdif<- rotdif[-idx,]
  
  rotdif$Abstdif<- abs(rotdif$tdif..37.)
  
  difs<-unique(rotdif$Abstdif)
  
  diftrials<- data.frame(matrix(NA, nrow = 12, ncol = 4))
  colnames(diftrials)<- c("15","30","45","60")
  
  rottrials<- data.frame(matrix(NA, nrow = 12, ncol = 4))
  colnames(rottrials)<- c("15","30","45","60")
  
  diftrials[,1]<- rotdif$trials[rotdif$Abstdif == 15]
  rottrials[,1]<- rotdif$tdif..37.[rotdif$Abstdif == 15]
  
  
  diftrials[,2]<-c(rotdif$trials[rotdif$Abstdif == 30], NA,NA)
  rottrials[,2]<- c(rotdif$tdif..37.[rotdif$Abstdif == 30], NA,NA)
  
  
  diftrials[,3]<- c(rotdif$trials[rotdif$Abstdif == 45], NA,NA, NA,NA, NA,NA, NA,NA)
  rottrials[,3]<- c(rotdif$tdif..37.[rotdif$Abstdif == 45], NA,NA, NA,NA, NA,NA, NA,NA)
  
  zerodiftrials<<-rotdif$trials[rotdif$Abstdif == 0]
  
  diftrials[,4]<-c(rotdif$trials[rotdif$Abstdif == 60], NA,NA, NA,NA,NA,NA, NA,NA, NA,NA)
  rottrials[,4]<- c(rotdif$tdif..37.[rotdif$Abstdif == 60], NA,NA, NA,NA,NA,NA, NA,NA, NA,NA)
  rottrials<<-rottrials
  return(diftrials)
}
plotdataperABSchange<-function(){
  
  
  variation_localization<- read.csv("data/variation_localization.csv", header = TRUE)
  variation_reaches<- read.csv("data/variation_reaches.csv", header = TRUE) 
  diftrials<- prepdataforabsplotting(variation_reaches)
  
  
  task = "prop"
  title<- sprintf("%s per ABSOLUTE rotation change_endpoint removed", task)
  alldata<- variation_localization
  
  
  # for (i in 1:nrow(alldata)){
  #   if (alldata$distortion[i] < 0)
  #     alldata[i,2:ncol(alldata)]<- alldata[i,2:ncol(alldata)]*-1
  # } 
  
  linetype<- 5
  linetype2<- 3
  scale<- 1
  scale2<- -1
  plotabschanges(task, diftrials,alldata,linetype, zerodiftrials,linetype2, title, scale, scale2)
  
  
  
  
  
  task = "reaches"
  
  alldata<- variation_reaches
  
  linetype <- 1
  linetype2<- 3
  title<- sprintf("%s per ABSOLUTE rotation change_endpoint removed ", task)
  scale<- -1
  scale2<- 1
  plotabschanges(task, diftrials,alldata,linetype, zerodiftrials, linetype2, title, scale, scale2)
  
  
  
}
plotabschanges<- function(task, diftrials,alldata, linetype, zerodiftrials, linetype2, title,scale, scale2){
  
  
  # 15degree changes
  
  startloc<-as.numeric(unlist(strsplit(diftrials[1,1], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[1,1], " to ")))[2]
  #data<- alldata[startloc:stoploc,2:33]
  data<- (alldata[startloc:stoploc,2:33] - mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))*scale
  #print(startloc)
  #print(mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))
  #print(rowMeans(data, na.rm = TRUE))
  
  
  for (i in 2:length(diftrials$"15")){
    
    startloc<-as.numeric(unlist(strsplit(diftrials[i,1], " to ")))[1]
    stoploc<-as.numeric(unlist(strsplit(diftrials[i,1], " to ")))[2]
    if (rottrials[i,1] < 0){
      data<- cbind(data,(alldata[startloc:stoploc,2:33] - mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))*scale)
    } else{
      data<- cbind(data,(alldata[startloc:stoploc,2:33] - mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))*scale2)
    }
    #data<- cbind(data,(alldata[startloc:stoploc,2:33] - mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE)))
    #print(startloc)
    #print(mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))
    #print(rowMeans(data, na.rm = TRUE))
    
  }
  
  plot((rowMeans(data, na.rm = TRUE)), type = "l", col = "chartreuse",xlim = c(1,12.5),ylim = c(-5,45), xlab = "Trials in a Block", ylab = "Hand Deviation", main = title, lwd = 2, lty = linetype)
  text(x = 12.5, y = ((rowMeans(data, na.rm = TRUE)))[12], "15",col = "chartreuse")
  
  
  
  
  #30 degree changes
  
  startloc<-as.numeric(unlist(strsplit(diftrials[1,2], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[1,2], " to ")))[2]
  #data<- alldata[startloc:stoploc,2:33]
  data<- (alldata[startloc:stoploc,2:33] - mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))*scale2
  #print(startloc)
  #print(mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))
  #print(rowMeans(data, na.rm = TRUE))
  
  for (i in 2:(length(diftrials$"30") - sum(is.na(diftrials$"30")))){
    
    startloc<-as.numeric(unlist(strsplit(diftrials[i,2], " to ")))[1]
    stoploc<-as.numeric(unlist(strsplit(diftrials[i,2], " to ")))[2]
    if (rottrials[i,2] < 0){
      data<- cbind(data,(alldata[startloc:stoploc,2:33] - mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))*scale)
    } else{
      data<- cbind(data,(alldata[startloc:stoploc,2:33] - mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))*scale2)
    }
    #data<- cbind(data,(alldata[startloc:stoploc,2:33] - mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE)))
    #print(startloc)
    #print(mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))
    #print(rowMeans(data, na.rm = TRUE))
    
    
  }
  
  
  lines((rowMeans(data, na.rm = TRUE)), type = "l", col = "cyan", lwd = 2, lty = linetype)
  text(x = 12.5, y = ((rowMeans(data, na.rm = TRUE)))[12], "30", col = "cyan")
  
  
  
  # 45 degree changes
  startloc<-as.numeric(unlist(strsplit(diftrials[1,3], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[1,3], " to ")))[2]
  #data<- alldata[startloc:stoploc,2:33]
  data<- (alldata[startloc:stoploc,2:33]- mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))*scale2
  #data<- alldata[startloc:stoploc,2:33]- modelendpoints[startloc-1 ==trialpoints]
  #print(startloc)
  #print(mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))
  #print(rowMeans(data, na.rm = TRUE))
  
  
  for (i in 2:(length(diftrials$"45") - sum(is.na(diftrials$"45")))){
    
    startloc<-as.numeric(unlist(strsplit(diftrials[i,3], " to ")))[1]
    stoploc<-as.numeric(unlist(strsplit(diftrials[i,3], " to ")))[2]
    if (rottrials[i,3] < 0){
      data<- cbind(data,(alldata[startloc:stoploc,2:33] - mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))*scale)
    } else{
      data<- cbind(data,(alldata[startloc:stoploc,2:33] - mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))*scale2)
    }
    #data<- cbind(data,(alldata[startloc:stoploc,2:33] - mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE)))
    #print(startloc)
    #print(mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))
    #print(rowMeans(data, na.rm = TRUE))
  }
  lines((rowMeans(data, na.rm = TRUE)), type = "l", col = "darkorchid4", lty = linetype, lwd = 2)
  text(x = 12.5, y = ((rowMeans(data, na.rm = TRUE)))[12], "45", col = "darkorchid4")
  
  
  
  #60 degree changes
  startloc<-as.numeric(unlist(strsplit(diftrials[1,4], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[1,4], " to ")))[2]
  #data<- alldata[startloc:stoploc,2:33])
  data<- (alldata[startloc:stoploc,2:33] - mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))*scale
  #data<- alldata[startloc:stoploc,2:33]- modelendpoints[startloc-1 ==trialpoints]
  #print(startloc)
  #print(mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))
  #print(rowMeans(data, na.rm = TRUE))
  
  
  startloc<-as.numeric(unlist(strsplit(diftrials[2,4], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[2,4], " to ")))[2]
  #data<- cbind(data,(alldata[startloc:stoploc,2:33]))
  data<- cbind(data,(alldata[startloc:stoploc,2:33]- mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))*scale2)
  #data<- cbind(data,(alldata[startloc:stoploc,2:33]- modelendpoints[startloc-1 ==trialpoints]))
  #print(startloc)
  #print(mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))
  lines((rowMeans(data, na.rm = TRUE)), type = "l", col = "deeppink", lty = linetype, lwd = 2)
  text(x = 12.5, y = ((rowMeans(data, na.rm = TRUE)))[12], "60", col = "deeppink")
  
  
  
  
  ## repeated blocks
  
  
  startloc<-as.numeric(unlist(strsplit(zerodiftrials[2], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(zerodiftrials[2], " to ")))[2]
  data<- alldata[startloc:stoploc,2:33]*scale
  #data<- (alldata[startloc:stoploc,2:33]- mean(as.numeric(unlist(alldata[startloc-13,2:33])), na.rm = TRUE))*scale
  #print(startloc)
  #print(mean(as.numeric(unlist(alldata[startloc-13,2:33])), na.rm = TRUE))
  #print(rowMeans(data, na.rm = TRUE))
  # lines((rowMeans(data, na.rm = TRUE)), type = "l", col = "cadetblue1", lty = linetype2, lwd = 2)
  # text(x = 12.5, y = ((rowMeans(data, na.rm = TRUE)))[12], "15", col = "cadetblue1")
  # 
  
  startloc<-as.numeric(unlist(strsplit(zerodiftrials[5], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(zerodiftrials[5], " to ")))[2]
  data<- cbind(data,(alldata[startloc:stoploc,2:33])*scale2)
  #data<- cbind(data,(alldata[startloc:stoploc,2:33]- mean(as.numeric(unlist(alldata[startloc-13,2:33])), na.rm = TRUE)))
  #print(startloc)
  #print(mean(as.numeric(unlist(alldata[startloc-13,2:33])), na.rm = TRUE))
  #print(rowMeans(data, na.rm = TRUE))
  lines((rowMeans(data, na.rm = TRUE)), type = "l", col = "chartreuse", lty = linetype2, lwd = 2)
  text(x = 12.5, y = ((rowMeans(data, na.rm = TRUE)))[12], "15", col = "chartreuse")
  
  startloc<-as.numeric(unlist(strsplit(zerodiftrials[3], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(zerodiftrials[3], " to ")))[2]
  data<- alldata[startloc:stoploc,2:33]*scale2
  #data<- alldata[startloc:stoploc,2:33]- mean(as.numeric(unlist(alldata[startloc-13,2:33])), na.rm = TRUE)
  #print(startloc)
  #print(mean(as.numeric(unlist(alldata[startloc-13,2:33])), na.rm = TRUE))
  #print(rowMeans(data, na.rm = TRUE))
  # lines((rowMeans(data, na.rm = TRUE)), type = "l", col = "chartreuse", lty = linetype2, lwd = 2)
  # text(x = 12.5, y = ((rowMeans(data, na.rm = TRUE)))[12], "-30", col = "chartreuse")
  
  
  startloc<-as.numeric(unlist(strsplit(zerodiftrials[6], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(zerodiftrials[6], " to ")))[2]
  data<- cbind(data,(alldata[startloc:stoploc,2:33])*scale)
  #data<- cbind(data,(alldata[startloc:stoploc,2:33]- mean(as.numeric(unlist(alldata[startloc-13,2:33])), na.rm = TRUE))*scale)
  #print(startloc)
  #print(mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))
  lines((rowMeans(data, na.rm = TRUE)), type = "l", col = "cadetblue1", lty = linetype2, lwd = 2)
  text(x = 12.5, y = ((rowMeans(data, na.rm = TRUE)))[12], "30", col = "cadetblue1")
  
  text(x = 5, y = -3, "dotted lines are second 12 trials \n when rotation was repeated")
  
}




## creating decay parameter files and plotting those parameters
decaymodelfitting<-function(data, task){
  library(svglite)
  source('E:/Jenn/Documents/Varied_Prop_Adaptation/R/shared.R')
  source('E:/Jenn/Documents/Varied_Prop_Adaptation/R/asymptoticDecayModel.R')
  data<- getreachesformodel(data)
  data$distortionNew<-data$distortion
  data$distortion[data$distortion == 360]<- 0
  rotation<- data$distortion
  blocks<- c(rep(0, times = 49), sort(rep(1:35, times = 12)), rep(36,11))
  transition<- colorRampPalette(c("green", "green4", "dark green"))
  colors<- transition(36) 
  schedule<- rep(-1,12)
  
  if (task == "reaches"){
    scale = -1
    scale2 = 1
    
  } else {
    scale = 1
    scale2 = -1
  }
  

  
  
  Allpars<- c()
  modeloutputs<- c()
  endpoints<-c()
  rotationsize<- c()
  SDs<- c()
  clamp<- c()

  for (i in 0:36){
    stop<- max(which(blocks==i))
    start<- stop-4
    endpoints[i+1]<-mean(as.numeric(unlist(data[start:stop,1])), na.rm = TRUE)
    rotationsize[i+1]<- as.numeric(unlist(data[stop,2]))
    clamp[i+1]<- as.numeric(unlist(data[stop,3]))
    SDs[i+1]<- sd(as.numeric(unlist(data[start:stop,1])), na.rm = TRUE)
  
  }

  Info<<- data.frame(endpoints,SDs,rotationsize)
  output<-sprintf("ana/endpoint & SDs %s.csv", task)
  write.csv(Info, file = output, quote = FALSE, row.names = FALSE)
  
  outputfile<-sprintf('figs/DecayModel_PerBlock_%s.svg', task)
  svglite(file=outputfile, width=15, height=21, system_fonts=list(sans = "Arial"))
  layout(matrix(c(1:40), nrow=8, byrow=TRUE), heights=c(1,1), widths = c(1,1))
  
  start<- min(which(blocks==1))
  stop<- max(which(blocks==1))
  
  if (data$distortion[max(which(blocks==1))] == 15 | data$distortion[max(which(blocks==1))] == 30){
    plot((data[start:stop,1]-endpoints[1])*scale, type = "l", ylim = c(-20,30),xlim = c(1,12), xlab = "trials", ylab = "Hand Direction", bty = 'n', col = colors[1], lwd = 2, main = rotationsize[2])
    reachsig<-(data[start:stop,1]-endpoints[1])*scale    
  } else {
  plot((data[start:stop,1]-endpoints[1])*scale2, type = "l", ylim = c(-20,30),xlim = c(1,12), xlab = "trials", ylab = "Hand Direction", bty = 'n', col = colors[1], lwd = 2, main = rotationsize[2])
  reachsig<-(data[start:stop,1]-endpoints[1])*scale2
  }
  pars<-asymptoticDecayFit(schedule = schedule, signal = reachsig)
  modeloutput<- asymptoticDecayModel(pars,schedule)
  lines(modeloutput$output, type = "l", col = "Blue")
  #legend(2,30, legend = c("data", "decay model"),bty = 'n', col = c(colors[1], "blue"))
  text(8,-14, sprintf('asymptote = %f', as.numeric(pars[2])))
  text(8,-17, sprintf('curve-endpoint = %f', modeloutput$output[12]))
  text(8,-20, sprintf('last trial of previous block = %f', endpoints[1]))
  print(pars)
  Allpars<- pars
  print(modeloutput$output[12])
  modeloutputs<- modeloutput$output[12]
  print(endpoints[2])
  
  
  for (i in 2:35){
    start<- min(which(blocks==i))
    stop<- max(which(blocks==i))
    
    if (data$distortion[max(which(blocks==i-1))] - data$distortion[max(which(blocks==i))] <0){
      
      plot((data[start:stop,1]-endpoints[i])*scale, type = "l", col = colors[i], ylim = c(-20,30), xlab = "trials in rotation", ylab = "Hand Direction",bty = 'n', lwd = 2,main = rotationsize[i+1]) 
      reachsig<-(data[start:stop,1]-endpoints[i])*scale
    } else if (data$distortion[max(which(blocks==i-1))] - data$distortion[max(which(blocks==i))] >0) {
      plot((data[start:stop,1]-endpoints[i])*scale2, type = "l", col = colors[i], ylim = c(-20,30), xlab = "trials in rotation", ylab = "Hand Direction",bty = 'n', lwd = 2,main = rotationsize[i+1]) 
      reachsig<-(data[start:stop,1]-endpoints[i])*scale2
    } else if (data$distortion[max(which(blocks==i-1))] - data$distortion[max(which(blocks==i))] == 0){
      start<- min(which(blocks==i-1))
      if (data$distortion[max(which(blocks==i-2))] - data$distortion[max(which(blocks==i-1))] <0){
        plot((data[start:stop,1]-endpoints[i-1])*scale, type = "l", col = colors[i], ylim = c(-20,30), xlab = "trials in rotation", ylab = "Hand Direction",bty = 'n', lwd = 2,main = rotationsize[i+1]) 
        reachsig<-(data[start:stop,1]-endpoints[i-1])*scale
      } else if(data$distortion[max(which(blocks==i-2))] - data$distortion[max(which(blocks==i-1))] >0) {
        plot((data[start:stop,1]-endpoints[i-1])*scale2, type = "l", col = colors[i], ylim = c(-20,30), xlab = "trials in rotation", ylab = "Hand Direction",bty = 'n', lwd = 2,main = rotationsize[i+1]) 
        reachsig<-(data[start:stop,1]-endpoints[i-1])*scale2
      }
      
    }
    schedule<- rep(-1, length(reachsig))
    
    pars<-asymptoticDecayFit(schedule = schedule, signal = reachsig)
    modeloutput<- asymptoticDecayModel(pars,schedule)
    lines(modeloutput$output, type = "l", col = "Blue")
    text(8,-14, sprintf('asymptote = %f', as.numeric(pars[2])))
    text(8,-17, sprintf('curve-endpoint = %f', modeloutput$output[12]))
    text(8,-20, sprintf('last trial of current block = %f', endpoints[i+1]))
    print(pars)
    Allpars<- rbind(Allpars,pars)
    print(modeloutput$output[12])
    modeloutputs<- c(modeloutputs, modeloutput$output[12])
    print(endpoints[i+1])
  }
  
  i = 36
  start<- min(which(blocks==i))
  stop<- max(which(blocks==i))
  
  if (data$distortion[max(which(blocks==i-1))] - data$distortion[max(which(blocks==i))] <0){
    plot((data[start:stop,1]-endpoints[i])*scale, type = "l", col = colors[i], ylim = c(-20,30), xlab = "trials in rotation", ylab = "Hand Direction",bty = 'n', lwd = 2,main = rotationsize[i+1]) 
    reachsig<-(data[start:stop,1]-endpoints[i])*scale
  } else if (data$distortion[max(which(blocks==i-1))] - data$distortion[max(which(blocks==i))] >0) {
    plot((data[start:stop,1]-endpoints[i])*scale2, type = "l", col = colors[i], ylim = c(-20,30), xlab = "trials in rotation", ylab = "Hand Direction",bty = 'n', lwd = 2,main = rotationsize[i+1]) 
    reachsig<-(data[start:stop,1]-endpoints[i])*scale2
  } else if (data$distortion[max(which(blocks==i-1))] - data$distortion[max(which(blocks==i))] == 0){
    start<- min(which(blocks==i-1))
    if (data$distortion[max(which(blocks==i-2))] - data$distortion[max(which(blocks==i-1))] <0){
      plot((data[start:stop,1]-endpoints[i-1])*scale, type = "l", col = colors[i], ylim = c(-20,30), xlab = "trials in rotation", ylab = "Hand Direction",bty = 'n', lwd = 2,main = rotationsize[i+1]) 
      reachsig<-(data[start:stop,1]-endpoints[i-1])*scale
    } else if (data$distortion[max(which(blocks==i-2))] - data$distortion[max(which(blocks==i-1))] >0){
    plot((data[start:stop,1]-endpoints[i-1])*scale2, type = "l", col = colors[i], ylim = c(-20,30), xlab = "trials in rotation", ylab = "Hand Direction",bty = 'n', lwd = 2,main = rotationsize[i+1]) 
    reachsig<-(data[start:stop,1]-endpoints[i-1])*scale2
    } 
    
  }
  schedule<- rep(-1, length(reachsig))
  pars<-asymptoticDecayFit(schedule = schedule, signal = reachsig)
  modeloutput<- asymptoticDecayModel(pars,schedule)
  lines(modeloutput$output, type = "l", col = "Blue")
  text(8,-14, sprintf('asymptote = %f', as.numeric(pars[2])))
  text(8,-17, sprintf('curve-endpoint = %f', modeloutput$output[12]))
  text(8,-20, sprintf('last trial of current block = %f', endpoints[i+1]))
  print(pars)
  Allpars<- rbind(Allpars,pars)
  print(modeloutput$output[12])
  modeloutputs<- c(modeloutputs, modeloutput$output[12])
  print(endpoints[i+1])
  
  dev.off()
  
  info<-data.frame(Allpars,modeloutputs,endpoints[2:37],rotationsize[2:37], clamp[2:37], abs((endpoints[1:36]*scale2) + rotationsize[2:37] ), SDs[2:37])
  colnames(info)<- c('learning Rate', "Asymptote", "ModelOutput", "Endpoint", "Rotation", "Clamp_Trials", "Change", "SDs")
  outputfile<- sprintf("ana/Decay Parameters %s Data.csv", task)
  write.csv(info, file =outputfile , quote = FALSE, row.names = FALSE)
  
}

plotdecayparamaters<- function (task, yloc = 1){

if (task == "reach"){
data<- read.csv('ana/Decay Parameters Reaches Data.csv', header = TRUE)
} else {
  data<- read.csv('ana/Decay Parameters Prop Data.csv', header = TRUE)
}



rots<-c("-30", "-15", "0", "15", "30", "360")
asymptotes<- data.frame(matrix(NA, nrow = 6, ncol = length(rots)))
LR<- data.frame(matrix(NA, nrow = 6, ncol = length(rots)))

for (i in 1:length(rots)){
 
asymptotes[,i]<-data$Asymptote[data$Clamp_Trials == rots[i]]
LR[,i]<-data$learning.Rate[data$Clamp_Trials == rots[i]]
  
colors<-  c("chartreuse4","blue" , "Grey" ,"cyan" , "chartreuse", "Black") 
}

output<- sprintf("figs/Asymptotes & LR for %s Data.png", task)
png(output, height = 500, width = 1000)
layout(matrix(c(1,2), nrow=1, byrow=TRUE), heights=c(1,1), widths = c(1,1))
title<- sprintf("Asymptotes for %s Data", task)
plot(x = c(1:6), y = asymptotes[,1], type = 'p', xlim = c(.5,6.5), ylim = c(0,35), col = colors[1], axes = FALSE, xlab = "Blocks", ylab = "Hand Direction", main = title)
lines(x = c(1:6), y = asymptotes[,1], type = 'l', col = colors[1])
axis(2, at = c(0,10,20,30), las = 2)
axis(1, at = c(1:6))
for (i in 2:6){
lines(x = c(1:6), y = asymptotes[,i], type = 'p', col = colors[i])
lines(x = c(1:6), y = asymptotes[,i], type = 'l', col = colors[i])
}


title<- sprintf("Learning Rates for %s Data", task)
plot(x = c(1:6), y = LR[,1], type = 'p', xlim = c(.5,6.5), ylim = c(0,1), col = colors[1], axes = FALSE, xlab = "Blocks", ylab = "Learning Rate", main = title)
lines(x = c(1:6), y = LR[,1], type = 'l', col = colors[1])
axis(2, at = c(0,.25,.5,.75,1), las = 2)
axis(1, at = c(1:6))
for (i in 2:6){
  lines(x = c(1:6), y = LR[,i], type = 'p', col = colors[i])
  lines(x = c(1:6), y = LR[,i], type = 'l', col = colors[i])
}


legend(x = 1, y = yloc, legend = rots, col = colors, lty = 1, bty = 'n', ncol = 2)
dev.off()
}

plotproportionaldecayparamatersseparately<- function (task = 'reach', yloc = 1){
  
  if (task == "reach"){
    data<- read.csv('ana/Decay Parameters Reaches Data.csv', header = TRUE)
  } else {
    data<- read.csv('ana/Decay Parameters Prop Data.csv', header = TRUE)
  }
  
  
  
  rots<-c("-30", "-15", "15", "30")
  asymptotes<- data.frame(matrix(NA, nrow = 6, ncol = length(rots)))
  LR<- data.frame(matrix(NA, nrow = 6, ncol = length(rots)))
  
  for (i in 1:length(rots)){
    
    asymptotes[,i]<-data$Asymptote[data$Clamp_Trials == rots[i]]/abs(as.numeric(rots[i]))
    LR[,i]<-data$learning.Rate[data$Clamp_Trials == rots[i]]
    
    colors<-  c("chartreuse4","blue"  ,"cyan" , "chartreuse") 
  }
  
  output<- sprintf("figs/Asymptotes & LR for %s Data.png", task)
  png(output, height = 500, width = 1000)
  layout(matrix(c(1,2), nrow=1, byrow=TRUE), heights=c(1,1), widths = c(1,1))
  title<- sprintf("Asymptotes for %s Data", task)
  plot(x = c(1:length(asymptotes[,1])),y = asymptotes[,1], type = 'p', xlim = c(.5, 6.5), ylim = c(0,2), col = colors[1], axes = FALSE, xlab = "Blocks", ylab = "Percent Correction", main = title)
  lines(x = c(1:6), y = asymptotes[,1], type = 'l', col = colors[1])
  axis(2, at = c(0,.5,1, 1.5,2), labels = c("0%", "50%", "100%", "150%", "200%"), las = 2)
  axis(1, at = c(1:6))
  for (i in 2:4){
    lines(x = c(1:6), y = asymptotes[,i], type = 'p', col = colors[i])
    lines(x = c(1:6), y = asymptotes[,i], type = 'l', col = colors[i])
  }
  
  
  title<- sprintf("Learning Rates for %s Data", task)
  plot(x = c(1:6), y = LR[,1], type = 'p', xlim = c(.5,6.5), ylim = c(0,1), col = colors[1], axes = FALSE, xlab = "Blocks", ylab = "Learning Rate", main = title)
  lines(x = c(1:6), y = LR[,1], type = 'l', col = colors[1])
  axis(2, at = c(0,.25,.5,.75,1), las = 2)
  axis(1, at = c(1:6))
  for (i in 2:4){
    lines(x = c(1:6), y = LR[,i], type = 'p', col = colors[i])
    lines(x = c(1:6), y = LR[,i], type = 'l', col = colors[i])
  }
  
  
  legend(x = 1, y = yloc, legend = rots, col = colors, lty = 1, bty = 'n', ncol = 2)
  dev.off()
}

plotproportionaldecayparamaterstogether<- function() {
  png("figs/Asymptotes & Learning Rates across time.png", height = 500, width = 1000)
  layout(matrix(c(1,2), nrow=1, byrow=TRUE), heights=c(1,1), widths = c(1,1))
  
  
  data<- read.csv('ana/Decay Parameters Reaches Data.csv', header = TRUE)
  plot(data$Asymptote[data$Rotation != 0]/abs(data$Change[data$Rotation !=0]), type = 'l', xlab = "Time", ylab = "Compensation", main = "Asymptotes", col = "Blue", ylim = c(-0,2), axes = FALSE)
  
  axis(2, at = c(0,.5,1,1.5,2), labels = c("0%", "50%", "100%", "150%", "200%"), las = 2)
  
  data<- read.csv('ana/Decay Parameters Prop Data.csv', header = TRUE)
  lines(data$Asymptote[data$Rotation != 0]/abs(data$Change[data$Rotation !=0]), col = "red")
  
  legend(2, 1.85, legend = c("reaches", "localization"), bty = "n", lty = 1, col = c("blue", "red"))
  
  
  
  data<- read.csv('ana/Decay Parameters Reaches Data.csv', header = TRUE)
  plot(data$learning.Rate[data$Rotation != 0], type = 'l', xlab = "Time", ylab = "Compensation", main = "Learning Rates", col = "Blue", ylim = c(0,2), axes = FALSE)
  axis(2, at = c(0,.5,1,1.5,2), labels = c("0%", "50%", "100%", "150%", "200%"), las = 2)
  
  data<- read.csv('ana/Decay Parameters Prop Data.csv', header = TRUE)
  lines(data$learning.Rate[data$Rotation != 0], col = "red")
  
  legend(2, 1.85, legend = c("reaches", "localization"), bty = "n", lty = 1, col = c("blue", "red"))
  
  dev.off()
  
}



# Get decay parameters per participant to make confidence intervals

decaymodelfittingperp<-function(){
  library(svglite)
  source('E:/Jenn/Documents/Varied_Prop_Adaptation/R/asymptoticDecayModel.R')
  
  
  
  
  dataset<- read.csv("data/variation_reaches.csv.csv", header = TRUE) 
  task = 'reaches'

for (i in 2:ncol(dataset)) {
  data<- data.frame(dataset[,i],dataset$distortion)
  colnames(data)<- c('meanreaches',"distortion" )
  print(i-1)
  DecayParameters(data, task, pnum = i-1)
  
}

  dataset<- read.csv("data/variation_localization.csv.csv", header = TRUE)
  task = 'prop'
  
  for (i in 2:ncol(dataset)) {
    data<- data.frame(dataset[,i],dataset$distortion)
    colnames(data)<- c('meanreaches',"distortion" )
    print(i-1)
    DecayParameters(data, task, pnum = i-1, rotate = -1)
    
  }
  
}

DecayParameters<- function(data, task, pnum, rotate = -1){
  
  distortionNew<-data$distortion
  data<-data.frame(data,distortionNew)
  data$distortion[data$distortion == 360]<- 0
  rotation<- data$distortion
  blocks<- c(rep(0, times = 49), sort(rep(1:35, times = 12)), rep(36,11))
  transition<- colorRampPalette(c("green", "green4", "dark green"))
  colors<- transition(36) 
  schedule<- rep(-1,12)
  
  if (task == "reaches"){
    scale = -1
    scale2 = 1
    
  } else {
    scale = 1
    scale2 = -1
  }
  
  
  
  
  Allpars<- c()
  modeloutputs<- c()
  endpoints<-c()
  rotationsize<- c()
  clamp<- c()
  
  
  
  for (i in 0:36){
    stop<- max(which(blocks==i))
    start<- stop - 4
    endpoints[i+1]<-mean(as.numeric(unlist(data[start:stop,1])), na.rm = TRUE)
    rotationsize[i+1]<- as.numeric(unlist(data[stop,2]))
    clamp[i+1]<- as.numeric(unlist(data[stop,3]))
  }
  
  outputfile<-sprintf('figs/DecayParameters/DecayModel_PerBlock_%s_p%d.pdf', task, pnum)
  pdf(file=outputfile, width=15, height=21)
  layout(matrix(c(1:40), nrow=8, byrow=TRUE), heights=c(1,1), widths = c(1,1))
  
  start<- min(which(blocks==1))
  stop<- max(which(blocks==1))
  
  if (data$distortion[max(which(blocks==1))] == 15 | data$distortion[max(which(blocks==1))] == 30){
    plot((data[start:stop,1]-endpoints[1])*scale, type = "l", ylim = c(-20,30),xlim = c(1,12), xlab = "trials", ylab = "Hand Direction", bty = 'n', col = colors[1], lwd = 2, main = rotationsize[2])
    reachsig<-(data[start:stop,1]-endpoints[1])*scale    
  } else {
    plot((data[start:stop,1]-endpoints[1])*scale2, type = "l", ylim = c(-20,30),xlim = c(1,12), xlab = "trials", ylab = "Hand Direction", bty = 'n', col = colors[1], lwd = 2, main = rotationsize[2])
    reachsig<-(data[start:stop,1]-endpoints[1])*scale2
  }
  pars<-asymptoticDecayFit(schedule = schedule, signal = reachsig)
  modeloutput<- asymptoticDecayModel(pars,schedule)
  lines(modeloutput$output, type = "l", col = "Blue")
  #legend(2,30, legend = c("data", "decay model"),bty = 'n', col = c(colors[1], "blue"))
  text(8,-14, sprintf('asymptote = %f', as.numeric(pars[2])))
  text(8,-17, sprintf('curve-endpoint = %f', modeloutput$output[12]))
  text(8,-20, sprintf('last trial of previous block = %f', endpoints[1]))
  #print(pars)
  Allpars<- pars
  #print(modeloutput$output[12])
  modeloutputs<- modeloutput$output[12]
  #print(endpoints[2])
  
  
  for (i in 2:35){
    start<- min(which(blocks==i))
    stop<- max(which(blocks==i))
    
    if (data$distortion[max(which(blocks==i-1))] - data$distortion[max(which(blocks==i))] <0){
      
      plot((data[start:stop,1]-endpoints[i])*scale, type = "l", col = colors[i], ylim = c(-20,30), xlab = "trials in rotation", ylab = "Hand Direction",bty = 'n', lwd = 2,main = rotationsize[i+1]) 
      reachsig<-(data[start:stop,1]-endpoints[i])*scale
    } else if (data$distortion[max(which(blocks==i-1))] - data$distortion[max(which(blocks==i))] >0) {
      plot((data[start:stop,1]-endpoints[i])*scale2, type = "l", col = colors[i], ylim = c(-20,30), xlab = "trials in rotation", ylab = "Hand Direction",bty = 'n', lwd = 2,main = rotationsize[i+1]) 
      reachsig<-(data[start:stop,1]-endpoints[i])*scale2
    } else if (data$distortion[max(which(blocks==i-1))] - data$distortion[max(which(blocks==i))] == 0){
      start<- min(which(blocks==i-1))
      if (data$distortion[max(which(blocks==i-2))] - data$distortion[max(which(blocks==i-1))] <0){
        plot((data[start:stop,1]-endpoints[i-1])*scale, type = "l", col = colors[i], ylim = c(-20,30), xlab = "trials in rotation", ylab = "Hand Direction",bty = 'n', lwd = 2,main = rotationsize[i+1]) 
        reachsig<-(data[start:stop,1]-endpoints[i-1])*scale
      } else if(data$distortion[max(which(blocks==i-2))] - data$distortion[max(which(blocks==i-1))] >0) {
        plot((data[start:stop,1]-endpoints[i-1])*scale2, type = "l", col = colors[i], ylim = c(-20,30), xlab = "trials in rotation", ylab = "Hand Direction",bty = 'n', lwd = 2,main = rotationsize[i+1]) 
        reachsig<-(data[start:stop,1]-endpoints[i-1])*scale2
      }
      
    }
    schedule<- rep(rotate, length(reachsig))
    
    pars<-asymptoticDecayFit(schedule = schedule, signal = reachsig)
    modeloutput<- asymptoticDecayModel(pars,schedule)
    lines(modeloutput$output, type = "l", col = "Blue")
    text(8,-14, sprintf('asymptote = %f', as.numeric(pars[2])))
    text(8,-17, sprintf('curve-endpoint = %f', modeloutput$output[12]))
    text(8,-20, sprintf('last trial of current block = %f', endpoints[i+1]))
    #print(pars)
    Allpars<- rbind(Allpars,pars)
    #print(modeloutput$output[12])
    modeloutputs<- c(modeloutputs, modeloutput$output[12])
    #print(endpoints[i+1])
  }
  
  i = 36
  start<- min(which(blocks==i))
  stop<- max(which(blocks==i))
  
  if (data$distortion[max(which(blocks==i-1))] - data$distortion[max(which(blocks==i))] <0){
    plot((data[start:stop,1]-endpoints[i])*scale, type = "l", col = colors[i], ylim = c(-20,30), xlab = "trials in rotation", ylab = "Hand Direction",bty = 'n', lwd = 2,main = rotationsize[i+1]) 
    reachsig<-(data[start:stop,1]-endpoints[i])*scale
  } else if (data$distortion[max(which(blocks==i-1))] - data$distortion[max(which(blocks==i))] >0) {
    plot((data[start:stop,1]-endpoints[i])*scale2, type = "l", col = colors[i], ylim = c(-20,30), xlab = "trials in rotation", ylab = "Hand Direction",bty = 'n', lwd = 2,main = rotationsize[i+1]) 
    reachsig<-(data[start:stop,1]-endpoints[i])*scale2
  } else if (data$distortion[max(which(blocks==i-1))] - data$distortion[max(which(blocks==i))] == 0){
    start<- min(which(blocks==i-1))
    if (data$distortion[max(which(blocks==i-2))] - data$distortion[max(which(blocks==i-1))] <0){
      plot((data[start:stop,1]-endpoints[i-1])*scale, type = "l", col = colors[i], ylim = c(-20,30), xlab = "trials in rotation", ylab = "Hand Direction",bty = 'n', lwd = 2,main = rotationsize[i+1]) 
      reachsig<-(data[start:stop,1]-endpoints[i-1])*scale
    } else if (data$distortion[max(which(blocks==i-2))] - data$distortion[max(which(blocks==i-1))] >0){
      plot((data[start:stop,1]-endpoints[i-1])*scale2, type = "l", col = colors[i], ylim = c(-20,30), xlab = "trials in rotation", ylab = "Hand Direction",bty = 'n', lwd = 2,main = rotationsize[i+1]) 
      reachsig<-(data[start:stop,1]-endpoints[i-1])*scale2
    } 
    
  }
  schedule<- rep(rotate, length(reachsig))
  pars<-asymptoticDecayFit(schedule = schedule, signal = reachsig)
  modeloutput<- asymptoticDecayModel(pars,schedule)
  lines(modeloutput$output, type = "l", col = "Blue")
  text(8,-14, sprintf('asymptote = %f', as.numeric(pars[2])))
  text(8,-17, sprintf('curve-endpoint = %f', modeloutput$output[12]))
  text(8,-20, sprintf('last trial of current block = %f', endpoints[i+1]))
  #print(pars)
  Allpars<- rbind(Allpars,pars)
  #print(modeloutput$output[12])
  modeloutputs<- c(modeloutputs, modeloutput$output[12])
  #print(endpoints[i+1])
  
  dev.off()
  
  info<-data.frame(Allpars,modeloutputs,endpoints[2:37],rotationsize[2:37], clamp[2:37])
  colnames(info)<- c('learning Rate', "Asymptote", "ModelOutput", "Endpoint", "Rotation", "Clamp_Trials")
  outputfile<- sprintf("ana/DecayParameters/Decay Parameters %s Data p%d.csv", task, pnum)
  write.csv(info, file =outputfile , quote = FALSE, row.names = FALSE)
  
  
}

Combineparameters<- function(){
filenames<-c()
for (i in 1:32)
  filenames[i]<-sprintf("ana/DecayParameters/Decay Parameters reaches Data p%d.csv", i)


endpoints<-c()
asymptotes<- c()
LR<-c()
modeloutput<-c()


for (i in 1:32){
  data<-read.csv(filenames[i])
  endpoints<-cbind(endpoints,data[,4])
  asymptotes<- cbind(asymptotes,data[,2])
  LR<-cbind(LR,data[,1])
  modeloutput<-cbind(modeloutput,data[,3])
}

endpoints<-cbind(endpoints,data[,5])
asymptotes<- cbind(asymptotes,data[,5])
LR<-cbind(LR,data[,5])
modeloutput<-cbind(modeloutput,data[,5])

write.csv(endpoints, "ana/DecayParameters/Reach Decay Endpoints all P.csv", quote = FALSE, row.names = FALSE)
write.csv(asymptotes, "ana/DecayParameters/Reach Decay Asymptotes all P.csv", quote = FALSE, row.names = FALSE)
write.csv(LR, "ana/DecayParameters/Reach Decay LR all P.csv", quote = FALSE, row.names = FALSE)
write.csv(modeloutput, "ana/DecayParameters/Reach Decay Model Outputs all P.csv", quote = FALSE, row.names = FALSE)


filenames<-c()
for (i in 1:32)
  filenames[i]<-sprintf("ana/DecayParameters/Decay Parameters prop Data p%d.csv", i)


endpoints<-c()
asymptotes<- c()
LR<-c()
modeloutput<-c()


for (i in 1:32){
  data<-read.csv(filenames[i])
  endpoints<-cbind(endpoints,data[,4])
  asymptotes<- cbind(asymptotes,data[,2])
  LR<-cbind(LR,data[,1])
  modeloutput<-cbind(modeloutput,data[,3])
}

endpoints<-cbind(endpoints,data[,5])
asymptotes<- cbind(asymptotes,data[,5])
LR<-cbind(LR,data[,5])
modeloutput<-cbind(modeloutput,data[,5])

write.csv(endpoints, "ana/DecayParameters/Prop Decay Endpoints all P.csv", quote = FALSE, row.names = FALSE)
write.csv(asymptotes, "ana/DecayParameters/Prop Decay Asymptotes all P.csv", quote = FALSE, row.names = FALSE)
write.csv(LR, "ana/DecayParameters/Prop Decay LR all P.csv", quote = FALSE, row.names = FALSE)
write.csv(modeloutput, "ana/DecayParameters/Prop Decay Model Outputs all P.csv", quote = FALSE, row.names = FALSE)




}


trialCI <- function(data) {
  AllCIs <- data.frame()
  for (trial in 1:nrow(data)) {
    y <- unlist(data[trial, 2:length(data)])
    CItrial <- t.interval(unlist(y))
    if (prod(dim(AllCIs)) == 0) {
      AllCIs <- CItrial
    } else {
      AllCIs <- rbind(AllCIs, CItrial)
    }
  }
  return(AllCIs)
}
t.interval = function(data, variance = var(data, na.rm = TRUE), conf.level = 0.95) {
  z = qt((1 - conf.level) / 2,
         df = length(data) - 1,
         lower.tail = FALSE)
  
  xbar = mean(data, na.rm = TRUE)
  sdx = sqrt(variance / length(data))
  
  return(c(xbar - z * sdx, xbar + z * sdx))
  
}

plotproportionaldecayparamaterstogetherCIs<- function() {
  png("figs/Asymptotes & Learning Rates across time with CI.png", height = 500, width = 1000)
  layout(matrix(c(1,2), nrow=1, byrow=TRUE), heights=c(1,1), widths = c(1,1))
  
  
  data<-read.csv('ana/DecayParameters/Reach Decay Asymptotes all P.csv', header = TRUE)
  indx<-which(data$V33 != 0)
  for (i in indx)
    data[i,1:32]<- data[i,1:32]/abs(data[i,33])
  
  plot(rowMeans(data[indx,1:32]), type = 'l', xlab = "Time", ylab = "Compensation", main = "Asymptotes", col = "Blue", ylim = c(-2,4), axes = FALSE)
  
  
  dataCIs <- trialCI(data = data) 
  x <- c(c(1:24), rev(c(1:24)))
  y <- c(dataCIs[indx, 1], rev(dataCIs[indx, 2]))
  polygon(x, y, col = rgb(0.1, 0.3, 0.5, 0.2), border = NA)
  
  
  axis(2, at = c(-2,-1,0,.5,1,1.5,2,3,3.5,4), labels = c("-200%", "-100%","0%", "50%", "100%", "150%", "200%", "300%", "350%", "400%"), las = 2)
  
  data<-read.csv('ana/DecayParameters/Prop Decay Asymptotes all P.csv', header = TRUE)
  indx<-which(data$V33 != 0)
  for (i in indx)
    data[i,1:32]<- data[i,1:32]/abs(data[i,33])
  lines(rowMeans(data[indx,1:32]), col = "red")
  dataCIs <- trialCI(data = data) 
  x <- c(c(1:24), rev(c(1:24)))
  y <- c(dataCIs[indx, 1], rev(dataCIs[indx, 2]))
  polygon(x, y, col = rgb(1, 0.0, 0., 0.2), border = NA)
  
  
  
  legend(10, -1.2, legend = c("reaches", "localization"), bty = "n", lty = 1, col = c("blue", "red"))
  
  
  
  data<-read.csv('ana/DecayParameters/Reach Decay LR all P.csv', header = TRUE)
  indx<-which(data$V33 != 0)
  plot(rowMeans(data[indx,1:32]), type = 'l', xlab = "Time", ylab = "Amount learned on any trial", main = "Learning Rate", col = "Blue", ylim = c(-2,4), axes = FALSE)
  
  
  dataCIs <- trialCI(data = data) 
  x <- c(c(1:24), rev(c(1:24)))
  y <- c(dataCIs[indx, 1], rev(dataCIs[indx, 2]))
  polygon(x, y, col = rgb(0.1, 0.3, 0.5, 0.2), border = NA)
  axis(2, at = c(-2,-1,0,.5,1,1.5,2,3,3.5,4), labels = c("-200%", "-100%","0%", "50%", "100%", "150%", "200%", "300%", "350%", "400%"), las = 2)
  
  data<-read.csv('ana/DecayParameters/Prop Decay LR all P.csv', header = TRUE)
  indx<-which(data$V33 != 0)
  lines(rowMeans(data[indx,1:32]), col = "red")
  dataCIs <- trialCI(data = data) 
  x <- c(c(1:24), rev(c(1:24)))
  y <- c(dataCIs[indx, 1], rev(dataCIs[indx, 2]))
  polygon(x, y, col = rgb(1, 0.0, 0., 0.2), border = NA)
  
  
  legend(10, -1.2, legend = c("reaches", "localization"), bty = "n", lty = 1, col = c("blue", "red"))
  
  dev.off()
  
}




getmodeloutputs<- function(){
  df <- read.csv('data/asymptoticDecayParameterCIs.csv', stringsAsFactors = F)
  
  
  trialsets <- list('1'=c(50:61),      "2" = c(62:73),      "3" = c(74:85),   "4" = c(86:97), 
                    "5" = c(98:109),   "6" = c(110:133),    "7" = c(134:157), "8" = c(158:181),
                    "9" = c(194:205), "10" = c(206:217),   "11" = c(218:229), "12" = c(254:265),
                    "13" = c(266:289),   "14" = c(290:313),   "15" = c(314:325),   "16" = c(326:337),
                    "17" = c(350:361),   "18" = c(362:373),   "19" = c(374:385),   "20" = c(386:397),
                    "21" = c(410:421),   "22" = c(422:433),   "23" = c(434:445),   "24" = c(446:457))
  
  rot<- read.csv("data/Reaches_Baselined.csv", header = TRUE)$distortion
  rots<- c()
  for (i in 1:length(trialsets)){
    rots[as.numeric(i)]<-unique(rot[trialsets[[i]]])
  }
  
  
  
  transition<- colorRampPalette(c("plum", "darkorchid"))
  colors<- transition(24) 
  tasks<- c("localization", "reaches")
  phases<- as.character(1:24)
  
  
  
  for (signalname in tasks) {
    
    leadingzero <- FALSE
    if (signalname == 'localization') {
      leadingzero <- TRUE
      dfit<-data.frame(matrix(NA, nrow = 24, ncol = 13))
      colnames(dfit)<- c(1:ncol(dfit))
    } else{
      dfit<-data.frame(matrix(NA, nrow = 24, ncol = 12))
      colnames(dfit)<- c(1:ncol(dfit))
    }
    
    schedulelength <- 12
    if (leadingzero) {
      schedulelength <- schedulelength + 1
    }
    schedule <- rep(-1, schedulelength)
    
    for (trialset in phases) {
      par <-
        c('lambda' = df[which(df$signal == signalname &
                                df$phase == trialset), "lambda"], 'N0' = df[which(df$signal == signalname & df$phase == trialset), 'N0'])
      
      dfit[as.numeric(trialset),] <- asymptoticDecayModel(par,schedule)$output
      
      
      
    }
    dfit$rotation<- rots
    outputname<- sprintf("ana/Decay Outputs %s.csv", signalname)
    write.csv(dfit,outputname, quote = FALSE, row.names = FALSE)
    
    
    
  }
  
}

plotDecaymodels<- function() {
  
  data<- read.csv("ana/Decay Outputs localization.csv", header = TRUE)
  
  
  rotations<- c(-30,-15,15,30)
  pdf("figs/localization decay model outputs by rotation size.pdf", height = 8, width = 10)
  layout(matrix(1:4, nrow = 2, byrow = TRUE), heights = c(2,2))
  
  for (i in rotations){
    
    p30<-data[data$rotation == i,1:13]
    
    title<- sprintf("Decay Model Output \n localizations for %d°", i)
    plot(as.numeric(unlist(p30[1,])), type = "l", col = "Blue", ylim = c(0, 15),xlim = c(0,14), xlab = "Trials in Block", ylab = "Hand Direction [°]", main = title, axes = FALSE)
    text(x = 13.5, y = as.numeric(unlist(p30[1,13])), " 1st", col = "blue")
    lines(as.numeric(unlist(p30[2,])), col = "Green")
    text(x = 13.5, y = as.numeric(unlist(p30[2,13])), " 2nd", col = "green")
    lines(as.numeric(unlist(p30[3,])), col = "red")
    text(x = 13.5, y = as.numeric(unlist(p30[3,13])), " 3rd", col = "red")
    lines(as.numeric(unlist(p30[4,])), col = "Purple")
    text(x = 13.5, y = as.numeric(unlist(p30[4,13])), " 4th", col = "Purple")
    lines(as.numeric(unlist(p30[5,])), col = "Pink")
    text(x = 13.5, y = as.numeric(unlist(p30[5,13])), " 5th", col = "pink")
    axis(1, at = c(1,2,3,4,5,13), labels = c(0,1,2,3,4,"end of \n block"))
    axis(2, at = c(0,5,10,15), las =2)
    abline(v = 2, lty = 2, col = "light grey")
    abline(v = 3, lty = 2, col = "light grey")
    abline(v = 4, lty = 2, col = "light grey")
    abline(v = 5, lty = 2, col = "light grey")
  }
  
  
  dev.off()
  
  
  data<- read.csv("ana/Decay Outputs reaches.csv", header = TRUE)
  
  rotations<- c(-30,-15,15,30)
  
  pdf("figs/reaches decay model outputs by rotation size.pdf", height = 8, width = 10)
  layout(matrix(1:4, nrow = 2, byrow = TRUE), heights = c(2,2))
  
  
  for (i in rotations){
    
    p30<-data[data$rotation == i,1:12]
    
    title<- sprintf("Decay Model Output \n reaches for %d°", i)
    plot(as.numeric(unlist(p30[1,])), type = "l", col = "Blue", ylim = c(0, 40),xlim = c(0,13), xlab = "Trials in Block", ylab = "Hand Direction [°]", main = title, axes = FALSE)
    text(x = 12.5, y = as.numeric(unlist(p30[1,12])), " 1st", col = "blue")
    lines(as.numeric(unlist(p30[2,])), col = "Green")
    text(x = 12.5, y = as.numeric(unlist(p30[2,12])), " 2nd", col = "green")
    lines(as.numeric(unlist(p30[3,])), col = "red")
    text(x = 12.5, y = as.numeric(unlist(p30[3,12])), " 3rd", col = "red")
    lines(as.numeric(unlist(p30[4,])), col = "Purple")
    text(x = 12.5, y = as.numeric(unlist(p30[4,12])), " 4th", col = "Purple")
    lines(as.numeric(unlist(p30[5,])), col = "Pink")
    text(x = 12.5, y = as.numeric(unlist(p30[5,12])), " 5th", col = "pink")
    axis(1, at = c(1,2,3,4,5,6,12), labels = c(0,1,2,3,4,5,"end of \n block"))
    axis(2, at = c(0,10,20,30,40), las =2)
    abline(v = 2, lty = 2, col = "light grey")
    abline(v = 3, lty = 2, col = "light grey")
    abline(v = 4, lty = 2, col = "light grey")
    abline(v = 5, lty = 2, col = "light grey")
    abline(v = 6, lty = 2, col = "light grey")
    
  }
  dev.off()
}


##Plotting learning rates and asymptotes after bootstrapping across blocks
plotLR_Aperblock<- function(){
  library(svglite)
  variation_localization<- read.csv("data/variation_localization.csv", header = TRUE)
  variation_reaches<- read.csv("data/variation_reaches.csv", header = TRUE) 
  
  phase0<- c(3,6,10,20)
  remove<- c(phase0, (phase0+24))
  df<- read.csv("ana/asymptoticDecayParameterCIs.csv", header = TRUE)
  
  
  newdf<- df[-remove,]
  
  
  phases<- as.character(c(1,2,4,5,7,8,9,11,12,13:19,21:24))
  
  trialsets <- list('1'=c(50:61),      "2" = c(62:73),      "3" = c(74:85),   "4" = c(86:97), 
                    "5" = c(98:109),   "6" = c(110:133),    "7" = c(134:157), "8" = c(158:181),
                    "9" = c(194:205), "10" = c(206:217),   "11" = c(218:229), "12" = c(254:265),
                    "13" = c(266:289),   "14" = c(290:313),   "15" = c(314:325),   "16" = c(326:337),
                    "17" = c(350:361),   "18" = c(362:373),   "19" = c(374:385),   "20" = c(386:397),
                    "21" = c(410:421),   "22" = c(422:433),   "23" = c(434:445),   "24" = c(446:457))
  
  
  
  rot<- c()
  for (phase in phases) rot<-c(rot,as.numeric(unique(variation_reaches[as.numeric(unlist(trialsets[phase])),1])))
  
  rotation<- c(rot,rot)
  newdf<- cbind(newdf,rotation)
  
  
  
  
  phases<- as.character(c(2,4,5,7,8,9,11,12,13:19,21:24))
  epr<-c(0)
  for (phase in phases){
  stop<-min(as.numeric(unlist(trialsets[phase])))-1
  start<- stop-3
  epr<- c(epr,mean(as.numeric(unlist(variation_reaches[stop,2:33])), na.rm = TRUE))
  }
  

  epl<-c(0)
  for (phase in phases){
    stop<-min(as.numeric(unlist(trialsets[phase])))-1
    start<- stop-3
    epl<- c(epl,mean(as.numeric(unlist(variation_localization[stop,2:33])), na.rm = TRUE))
  }
  epr<- epr*-1
  
  
  
  
  #ep<- c(0,newdf$N0[1:19])
  scale<- newdf$rotation[1:20] - epl
  #ep<- c(0,newdf$N0[21:39])
  scale<- c(scale, (newdf$rotation[21:40] - epr))

  
  
  
  
  
  
  newN0<- (newdf$N0/abs(scale))*100
  newN_97<- (newdf$N0_975/abs(scale))*100
  newN_25<- (newdf$N0_025/abs(scale))*100
  newN_5<- (newdf$N0_5/abs(scale))*100
  #newdf$rotation

  svglite("figs/LR & Asymptotes Across Blocks Together_1127.svg", height = 10, width = 14)

  

  
  
  
  
  layout(matrix(c(1,1,2,2,1,1,2,2,3,3,4,4), nrow = 3, ncol = 4, byrow = TRUE))
  
  lefts<- sort(c((1:20)-.10,(1:20)-.10))
  rights<- sort(c((1:20)+.15,(1:20)+.15))
  
  plot(newN_5[1:20], xlim = c(1,20),ylim = c(0,200), col = "Red", main = "Asymptotes Across Blocks", ylab = "Percent Compensation", xlab = "Block (12 or 24 trials)", axes = FALSE, cex.lab = 1.7, cex.main = 1.5)
  abline(h = 100, lty = 3, col = "red")
  abline(h = 20, lty = 3, col = "blue")
  
  indx<- seq(from = 0, to = 20, by = 4)
  indx[1]<- 1
  axis(1, at = indx,cex.axis = 1.5)
  axis(2, at = c(0,25,50,75,100,125,150,175,200), las = 2,cex.axis = 1.5)
  
  startlocs<- seq(from = 1, to = 40, by = 2)
  for (i in 1:20){
  lower<- startlocs[i]
  x<- c(lefts[lower],rights[lower],rights[lower+1],lefts[lower+1])
  y <- c(newN_25[i],newN_25[i],newN_97[i],newN_97[i])
  polygon(x, y, col = rgb(1,0,0,.2), border = NA)
  }
  

  #time<- c(1:20,1:20)
  #modeld<- data.frame(newN_5, newdf$lambda_500, time)
  #model<-lm(newN_5[1:20]~time[1:20], data = modeld)
  #lines(1:20,predict(model), col = "red", lty = 2)
  
  time<- c(1:18,1:18)

  newnewN_5<- newN_5[-c(1,14,21,34)]
  lambda_500<- newdf$lambda_500[-c(1,14,21,34)]
  modeld<- data.frame(newnewN_5, lambda_500, time)
  model<-lm(newnewN_5[1:18]~time[1:18], data = modeld)
  lines(2:19,predict(model), col = "red", lty = 2)
  print(summary(model))
  
  

  #plot(newN_5[21:40], type = "l", ylim = c(0,200), col = "Blue", main = "Reach Asymptotes Across Blocks", ylab = "Percent Compensation", xlab = "Block (12 or 24 trials)", axes = FALSE)
  points(newN_5[21:40], col = "Blue")
  #axis(1, at = indx)
  #axis(2, at = c(0,25,50,75,100,125,150,175,200), las = 2)

  for (i in 1:20){
    lower<- startlocs[i]
    x<- c(lefts[lower],rights[lower],rights[lower+1],lefts[lower+1])
    y <- c(newN_25[i+20],newN_25[i+20],newN_97[i+20],newN_97[i+20])
    polygon(x, y, col = rgb(0,0,1,.2), border = NA)
  }
  
  model<-lm(newnewN_5[19:36]~time[19:36], data = modeld)
  lines(2:19,predict(model), col = "blue", lty = 2)
  print(summary(model))
  
  
  legend(3,200, legend= c("Localizations, r2 = .15", "Reaches, r2 = .19*", "Regression"), col = c("Red", "Blue", "black"), lty = c(1,1,2), lwd = 1, bty = "n", cex = 1.5)
  
  mtext('A', outer=FALSE, side=3, las=1, line=1, adj=0, padj=1,cex = 2)

  
  
  
  
  
  
  plot(df$lambda_500[1:20], ylim = c(0,1), col = "Red", main = "Learning Rates Across Blocks", ylab = "Amount changed per trial", xlab = "Block (12 or 24 trials)", axes = FALSE,  cex.lab = 1.7, cex.main = 1.5)
  
  abline(h = .2, lty = 3, col = "blue")
  for (i in 1:20){
    lower<- startlocs[i]
    x<- c(lefts[lower],rights[lower],rights[lower+1],lefts[lower+1])
    y <- c(df$lambda_025[i],df$lambda_025[i],df$lambda_975[i],df$lambda_975[i])
    polygon(x, y, col = rgb(1,0,0,.2), border = NA)
  }
  

  axis(1, at = indx, cex.axis = 1.5)
  axis(2, at = c(0,.25,.5,.75,1), las = 2, cex.axis = 1.5)
  #model<-lm(newdf.lambda_500[1:20]~time[1:20], data = modeld)
  #lines(1:20,predict(model), col = "red", lty = 2)
  
  model<-lm(lambda_500[1:18]~time[1:18], data = modeld)
  lines(2:19,predict(model), col = "red", lty = 2)



  print(summary(model))

  
  
  #plot(df$lambda_500[21:40], type = "l", ylim = c(0,1), col = "Blue", main = "Reach Learning Rates Across Blocks", ylab = "Amount learned per trial", xlab = "Block (12 or 24 trials)", axes = FALSE)
  points(df$lambda_500[21:40], col = "Blue")
  
  
  for (i in 1:20){
    lower<- startlocs[i]
    x<- c(lefts[lower],rights[lower],rights[lower+1],lefts[lower+1])
    y <- c(df$lambda_025[i+20],df$lambda_025[i+20],df$lambda_975[i+20],df$lambda_975[i+20])
    polygon(x, y, col = rgb(0,0,1,.2), border = NA)
  }

  model<-lm(lambda_500[19:36]~time[19:36], data = modeld)

  lines(2:19,predict(model), col = "blue", lty = 2)
  print(summary(model))
  
  
  legend(3,.1, legend= c("Localizations, r2 = .005", "Reaches, r2 = .24*", "Regression"), col = c("Red", "Blue", "Black"), lty = c(1,1,2), lwd = 1, bty = "n", cex = 1.5)
  

  #lines(2:19,predict(model), col = "blue", lty = 2)
  print(summary(model))
  
  
  
  mtext('B', outer=FALSE, side=3, las=1, line=1, adj=0, padj=1,cex = 2)

  

  dev.off()
}

##looking at variability across blocks This needs to be looking at the variability within subjects not across subjects

variabilityplot<-function() {
variation_localization<- read.csv("data/variation_localization.csv", header = TRUE)
variation_reaches<- read.csv("data/variation_reaches.csv", header = TRUE) 

trialsets <- list('1'=c(50:61),      "2" = c(62:73),   "4" = c(86:97), 
                  "5" = c(98:109),    "7" = c(134:157), "8" = c(158:181),
                  "9" = c(194:205),   "11" = c(218:229), "12" = c(254:265),
                  "13" = c(266:289),   "14" = c(290:313),   "15" = c(314:325),   "16" = c(326:337),
                  "17" = c(350:361),   "18" = c(362:373),   "19" = c(374:385),
                  "21" = c(410:421),   "22" = c(422:433),   "23" = c(434:445),   "24" = c(446:457))



sdCIsr<- data.frame()
sdCIsl<- data.frame()

participants<- 2:33

for (participant in participants){
for (trial in 1:20){
  
stop<-as.numeric(max(trialsets[[trial]]))
start<- stop - 3

sdCIsr[trial,participant-1]<-sd(variation_reaches[start:stop,participant], na.rm = TRUE)
sdCIsl[trial,participant-1]<-sd(variation_localization[start:stop,participant], na.rm = TRUE)

}
}

datar<- cbind(1:2, sdCIsr)
plot(rowMeans(datar[,2:33], na.rm = TRUE), type = "l", col = "Blue", ylim = c(0,12), axes = FALSE, xlab = "Block of Trials (12 or 24)", ylab = "Standard Deviation [°]", main = "Variability Over Blocks")
indx<- seq(from = 0, to = 20, by = 4)
indx[1]<- 1
axis(1, at = indx)
axis(2, at = c(0,2,4,6,8,10,12), las = 2)
dataCIs<- trialCI(datar)
y <- c(dataCIs[, 1], rev(dataCIs[, 2]))
x = c(c(1:20), rev(c(1:20)))
polygon(x,y,col = rgb(0,0,1,.2), border = NA )
legend(3,3, legend= c("Localizations, r2 = .01", "Reaches, r2 = .36*", "Regression"), col = c("Red", "Blue", "black"), lty = c(1,1,2), lwd = 1, bty = "n")
reaches<-rowMeans(datar[,2:33], na.rm = TRUE)
time<- 1:20
stuff<- data.frame(reaches, time)
model<-lm(reaches~time, data = stuff)
lines(time, predict(model), col = "blue", lty = 2)

data<- cbind(1:2, sdCIsl)
lines(rowMeans(data[,2:33], na.rm = TRUE), col = "red")
dataCIs<- trialCI(data)
y <- c(dataCIs[, 1], rev(dataCIs[, 2]))
x = c(c(1:20), rev(c(1:20)))
polygon(x,y,col = rgb(1,0,0,.2), border = NA )
localizations<-rowMeans(data[,2:33], na.rm = TRUE)
time<- 1:20
stuff<- data.frame(localizations, time)
model<-lm(localizations~time, data = stuff)
lines(time, predict(model), col = "red", lty = 2)
summary(model)



}

plotLR_Aallblocks<- function(){
  library(svglite)
  variation_localization<- read.csv("data/variation_localization.csv", header = TRUE)
  variation_reaches<- read.csv("data/variation_reaches.csv", header = TRUE) 
  
  phase0<- c(3,6,10,20)
  remove<- c(phase0, (phase0+24))
  df<- read.csv("ana/asymptoticDecayParameterCIs.csv", header = TRUE)
  
  
  newdf<- df[-remove,]
  
  
  phases<- as.character(c(1,2,4,5,7,8,9,11,12,13:19,21:24))
  
  trialsets <- list('1'=c(50:61),      "2" = c(62:73),      "3" = c(74:85),   "4" = c(86:97), 
                    "5" = c(98:109),   "6" = c(110:133),    "7" = c(134:157), "8" = c(158:181),
                    "9" = c(194:205), "10" = c(206:217),   "11" = c(218:229), "12" = c(254:265),
                    "13" = c(266:289),   "14" = c(290:313),   "15" = c(314:325),   "16" = c(326:337),
                    "17" = c(350:361),   "18" = c(362:373),   "19" = c(374:385),   "20" = c(386:397),
                    "21" = c(410:421),   "22" = c(422:433),   "23" = c(434:445),   "24" = c(446:457))
  
  
  
  rot<- c()
  for (phase in phases) rot<-c(rot,as.numeric(unique(variation_reaches[as.numeric(unlist(trialsets[phase])),1])))
  
  rotation<- c(rot,rot)
  newdf<- cbind(newdf,rotation)
  
  
  
  
  phases<- as.character(c(2,4,5,7,8,9,11,12,13:19,21:24))
  epr<-c(0)
  for (phase in phases){
    stop<-min(as.numeric(unlist(trialsets[phase])))-1
    start<- stop-3
    epr<- c(epr,mean(as.numeric(unlist(variation_reaches[stop,2:33])), na.rm = TRUE))
  }
  
  
  epl<-c(0)
  for (phase in phases){
    stop<-min(as.numeric(unlist(trialsets[phase])))-1
    start<- stop-3
    epl<- c(epl,mean(as.numeric(unlist(variation_localization[stop,2:33])), na.rm = TRUE))
  }
  epr<- epr*-1
  
  
  
  
  #ep<- c(0,newdf$N0[1:19])
  scale<- newdf$rotation[1:20] - epl
  #ep<- c(0,newdf$N0[21:39])
  scale<- c(scale, (newdf$rotation[21:40] - epr))
  
  
  
  
  
  
  
  newN0<- (newdf$N0/abs(scale))*100
  newN_97<- (newdf$N0_975/abs(scale))*100
  newN_25<- (newdf$N0_025/abs(scale))*100
  newN_5<- (newdf$N0_5/abs(scale))*100
  #newdf$rotation
  
  svglite("figs/LR & Asymptotes Across All Blocks Together_1222c.svg", height = 8, width = 14)
  
  layout(matrix(c(1,2), nrow = 1, ncol = 2, byrow = TRUE))

  
  data<-c(newN_5[1:2], NA, newN_5[3:4], NA, newN_5[5], NA, newN_5[6], NA, NA, newN_5[7], NA, newN_5[8], NA,NA, newN_5[9:10],NA,newN_5[11], NA,newN_5[12:13], NA,newN_5[14:16], NA, NA, newN_5[17:20], NA, NA) 
  
  
 places<-   which(!is.na(data))
  
 
 lefts<- sort(c((places)-.10,(places)-.10))
 rights<- sort(c((places)+.15,(places)+.15))
  
  plot(data, xlim = c(1,35),ylim = c(0,150), col = "Red", main = "Asymptotes Across Blocks", ylab = "Percent Compensation", xlab = "Block (12 or 24 trials)", axes = FALSE, cex.lab = 1.7, cex.main = 1.5)
  #abline(h = 100, lty = 3, col = "red")
  #abline(h = 20, lty = 3, col = "blue")
  
  indx<- seq(from = 0, to = 36, by = 4)
  indx[1]<- 1
  axis(1, at = indx,cex.axis = 1.5)
  axis(2, at = c(0,25,50,75,100,125,150), las = 2,cex.axis = 1.5)
  
  startlocs<- seq(from = 1, to = 40, by = 2)
  for (i in 1:20){
    lower<- startlocs[i]
    x<- c(lefts[lower],rights[lower],rights[lower+1],lefts[lower+1])
    y <- c(newN_25[i],newN_25[i],newN_97[i],newN_97[i])
    polygon(x, y, col = rgb(1,0,0,.2), border = NA)
  }
  
##ccreate data for model
  data1<-c(newN_5[21:22], NA, newN_5[23:24], NA, newN_5[25], NA, newN_5[26], NA, NA, newN_5[27], NA, newN_5[28], NA,NA, newN_5[29:30],NA,newN_5[31], NA,newN_5[32:33], NA,newN_5[34:36], NA, NA, newN_5[37:40],NA,NA) 
  data2<-c(df$lambda_500[1:2], NA, df$lambda_500[3:4], NA, df$lambda_500[5], NA, df$lambda_500[6], NA, NA, df$lambda_500[7], NA, df$lambda_500[8], NA,NA, df$lambda_500[9:10],NA,df$lambda_500[11], NA,df$lambda_500[12:13], NA,df$lambda_500[14:16], NA, NA, df$lambda_500[17:20], NA, NA) 
  data3<-c(df$lambda_500[21:22], NA, df$lambda_500[23:24], NA, df$lambda_500[25], NA, df$lambda_500[26], NA, NA, df$lambda_500[27], NA, df$lambda_500[28], NA,NA, df$lambda_500[29:30],NA,df$lambda_500[31], NA,df$lambda_500[32:33], NA,df$lambda_500[34:36], NA, NA, df$lambda_500[37:40], NA, NA) 
  
  
  time<- c(1:33,1:33)
  
  asymptotes<- c(data[-c(2,25)], data1[-c(2,25)])
  learningrates<- c(data2[-c(2,25)], data3[-c(2,25)]) 
  modeld<- data.frame(asymptotes, learningrates, time)
  model<-lm(asymptotes[1:33]~time[1:33], data = modeld)
  
  

  abline(summary(model)$coefficient[1], summary(model)$coefficient[2], col = "red", lty = 2)
  #lines(2:19,predict(model), col = "red", lty = 2)
  print(summary(model))
  
  
  data1<-c(newN_5[21:22], NA, newN_5[23:24], NA, newN_5[25], NA, newN_5[26], NA, NA, newN_5[27], NA, newN_5[28], NA,NA, newN_5[29:30],NA,newN_5[31], NA,newN_5[32:33], NA,newN_5[34:36], NA, NA, newN_5[37:40],NA,NA) 
  #plot(newN_5[21:40], type = "l", ylim = c(0,200), col = "Blue", main = "Reach Asymptotes Across Blocks", ylab = "Percent Compensation", xlab = "Block (12 or 24 trials)", axes = FALSE)
  points(data1, col = "Blue")
  #axis(1, at = indx)
  #axis(2, at = c(0,25,50,75,100,125,150,175,200), las = 2)
  
  for (i in 1:20){
    lower<- startlocs[i]
    x<- c(lefts[lower],rights[lower],rights[lower+1],lefts[lower+1])
    y <- c(newN_25[i+20],newN_25[i+20],newN_97[i+20],newN_97[i+20])
    polygon(x, y, col = rgb(0,0,1,.2), border = NA)
  }
  
  model<-lm(asymptotes[34:66]~time[34:66], data = modeld)
  abline(summary(model)$coefficient[1], summary(model)$coefficient[2], col = "blue", lty = 2)
  print(summary(model))
  
  
  legend(3,150, legend= c("Localizations, r2 = .04", "Reaches, r2 = .05", "Regression"), col = c("Red", "Blue", "black"), lty = c(1,1,2), lwd = 1, bty = "n", cex = 1.25)
  
  mtext('A', outer=FALSE, side=3, las=1, line=1, adj=0, padj=1,cex = 2)
  
  
  
  
  
  
  data2<-c(df$lambda_500[1:2], NA, df$lambda_500[3:4], NA, df$lambda_500[5], NA, df$lambda_500[6], NA, NA, df$lambda_500[7], NA, df$lambda_500[8], NA,NA, df$lambda_500[9:10],NA,df$lambda_500[11], NA,df$lambda_500[12:13], NA,df$lambda_500[14:16], NA, NA, df$lambda_500[17:20], NA, NA) 
  plot(data2, ylim = c(0,1), col = "Red", main = "Learning Rates Across Blocks", ylab = "Amount changed per trial", xlab = "Block (12 or 24 trials)", axes = FALSE,  cex.lab = 1.7, cex.main = 1.5)
  
  #abline(h = .2, lty = 3, col = "blue")
  for (i in 1:20){
    lower<- startlocs[i]
    x<- c(lefts[lower],rights[lower],rights[lower+1],lefts[lower+1])
    y <- c(df$lambda_025[i],df$lambda_025[i],df$lambda_975[i],df$lambda_975[i])
    polygon(x, y, col = rgb(1,0,0,.2), border = NA)
  }
  
  
  axis(1, at = indx, cex.axis = 1.5)
  axis(2, at = c(0,.25,.5,.75,1), las = 2, cex.axis = 1.5)
  #model<-lm(newdf.lambda_500[1:20]~time[1:20], data = modeld)
  #lines(1:20,predict(model), col = "red", lty = 2)
  
  model<-lm(learningrates[1:33]~time[1:33], data = modeld)
  abline(summary(model)$coefficient[1], summary(model)$coefficient[2], col = "red", lty = 2)
  #lines(2:19,predict(model), col = "red", lty = 2)
  
  
  
  print(summary(model))
  
  
  data3<-c(df$lambda_500[21:22], NA, df$lambda_500[23:24], NA, df$lambda_500[25], NA, df$lambda_500[26], NA, NA, df$lambda_500[27], NA, df$lambda_500[28], NA,NA, df$lambda_500[29:30],NA,df$lambda_500[31], NA,df$lambda_500[32:33], NA,df$lambda_500[34:36], NA, NA, df$lambda_500[37:40], NA, NA) 
  #plot(df$lambda_500[21:40], type = "l", ylim = c(0,1), col = "Blue", main = "Reach Learning Rates Across Blocks", ylab = "Amount learned per trial", xlab = "Block (12 or 24 trials)", axes = FALSE)
  points(data3, col = "Blue")
  
  
  for (i in 1:20){
    lower<- startlocs[i]
    x<- c(lefts[lower],rights[lower],rights[lower+1],lefts[lower+1])
    y <- c(df$lambda_025[i+20],df$lambda_025[i+20],df$lambda_975[i+20],df$lambda_975[i+20])
    polygon(x, y, col = rgb(0,0,1,.2), border = NA)
  }
  
  model<-lm(learningrates[34:66]~time[34:66], data = modeld)
  abline(summary(model)$coefficient[1], summary(model)$coefficient[2], col = "blue", lty = 2)
  
  #lines(2:19,predict(model), col = "blue", lty = 2)
  print(summary(model))
  
  
  legend(3,.75, legend= c("Localizations, r2 = .04", "Reaches, r2 = .03", "Regression"), col = c("Red", "Blue", "Black"), lty = c(1,1,2), lwd = 1, bty = "n", cex = 1.25)
  
  
  #lines(2:19,predict(model), col = "blue", lty = 2)
 
  
  
  
  mtext('B', outer=FALSE, side=3, las=1, line=1, adj=0, padj=1,cex = 2)
  
  dev.off()
  
  
  svglite("figs/rotations for LR and asymptotes_1222c.svg", height = 4, width = 14)
  
  layout(matrix(c(1,2), nrow = 1, ncol = 2, byrow = TRUE))
  
  
  source("R/shared.R")
  variation_localization<- read.csv("data/variation_localization.csv", header = TRUE)
  variation_reaches<- read.csv("data/variation_reaches.csv", header = TRUE) 
  
  vprop<- Getshiftsperrotation(variation_localization)
  vreac<- Getreachesperrotation(variation_reaches)
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
       cex.main = 1.5, 
       ylab = "Rotation [°]", ylim = c(-30, 30), xlim = c(1,480), xlab = "", xaxt = 'n')
  
  lines(x = z[1:25], y = sizes[1:25], type = 'l')
  lines(x = z[25:26], y = c(0,0), lty = 2, col = "Dark Grey")
  lines(x = z[26:33], y = sizes[26:33], type = 'l')
  lines(x = z[33:36], y = c(0,0,0,0), lty = 2, col = "Dark Grey")
  lines(x = z[36:51], y = sizes[36:51], type = 'l')
  lines(x = z[51:52], y = c(0,0), lty = 2, col = "Dark Grey")
  lines(x = z[52:61], y = sizes[52:61], type = 'l')
  lines(x = z[61:62], y = c(0,0), lty = 2, col = "Dark Grey")
  lines(x = z[62:71], y = sizes[62:71], type = 'l')
  lines(x = z[71:72], y = c(0,0), lty = 2, col = "Dark Grey")
  lines(x = z[73:74], y = sizes[73:74], type = 'l')
  
  # legend(
  #   -5,
  #   30,
  #   legend = c(
  #     'Reaches',
  #     'Localizations'),
  #   col = c('blue', 'red'),
  #   lty = c(1),
  #   
  #   
  #   lwd = c(2),
  #   bty = 'n', 
  #   cex = 1.2
  # )
  
  # legend(
  #   -5,
  #   30,
  #   legend = c(
  #     'Participant controls cursor',
  #     'Clamped cursor'),
  #   col = c('black', 'dark grey'),
  #   lty = c(1,2),
  #   lwd = c(2),
  #   bty = 'n', 
  #   cex = 1.2
  # )
  # 
  axis(2, at = c(-30, -15, 0, 15, 30), cex.axis = 1.5,
       las = 2)
  g<- c(seq(from = 50, to = 480, by = 48), 480)
  axis(1, at = c(1,g), cex.axis = 1.25)
  locations<- seq(from = 50, to=470, by=12)
  locations[]
  
  axis(1, at =locations, labels = c(1:36), cex.axis = 1, line = 2.5, las = 2 )
  
  mtext('C', outer=FALSE, side=3, las=1, line=1, adj=0, padj=1,cex = 2)
  
  
  plot(NULL, col = 'white', axes = F,cex.lab = 1.5,
       cex.main = 1.5, 
       ylab = "Rotation [°]", ylim = c(-30, 30), xlim = c(1,480), xlab = "", xaxt = 'n')
  
  lines(x = z[1:25], y = sizes[1:25], type = 'l')
  lines(x = z[25:26], y = c(0,0), lty = 2, col = "Dark Grey")
  lines(x = z[26:33], y = sizes[26:33], type = 'l')
  lines(x = z[33:36], y = c(0,0,0,0), lty = 2, col = "Dark Grey")
  lines(x = z[36:51], y = sizes[36:51], type = 'l')
  lines(x = z[51:52], y = c(0,0), lty = 2, col = "Dark Grey")
  lines(x = z[52:61], y = sizes[52:61], type = 'l')
  lines(x = z[61:62], y = c(0,0), lty = 2, col = "Dark Grey")
  lines(x = z[62:71], y = sizes[62:71], type = 'l')
  lines(x = z[71:72], y = c(0,0), lty = 2, col = "Dark Grey")
  lines(x = z[73:74], y = sizes[73:74], type = 'l')
  
  # legend(
  #   -5,
  #   30,
  #   legend = c(
  #     'Reaches',
  #     'Localizations'),
  #   col = c('blue', 'red'),
  #   lty = c(1),
  #   
  #   
  #   lwd = c(2),
  #   bty = 'n', 
  #   cex = 1.2
  # )
  
  # legend(
  #   -5,
  #   30,
  #   legend = c(
  #     'Participant controls cursor',
  #     'Clamped cursor'),
  #   col = c('black', 'dark grey'),
  #   lty = c(1,2),
  #   lwd = c(2),
  #   bty = 'n', 
  #   cex = 1.2
  # )
  # 
  axis(2, at = c(-30, -15, 0, 15, 30), cex.axis = 1.5,
       las = 2)
  g<- c(seq(from = 50, to = 480, by = 48), 480)
  axis(1, at = c(1,g), cex.axis = 1.25)
  locations<- seq(from = 50, to=470, by=12)
  locations[]
  
  axis(1, at =locations, labels = c(1:36), cex.axis = 1, line = 2.5, las = 2 )
  

  
  # 
  # 
  # 
  # y<-c()
  # for (i in 1:20){
  #   y<- c(y, rot[i], rot[i])  
  # }
  # x<- c(1,sort(c(2:20,2:20)))
  # plot(x = x, y = y[-40]*-1, type = 'l', axes = FALSE, ylab = "Rotation Size", xlab = "Block (12 or 24 trials", xlim = c(1,20), cex.lab = 1.7)
  # axis(1, at = indx,cex.axis = 1.5)
  # axis(2, at = c(-30,-15,0,15,30),cex.axis = 1.5)
  # mtext('C', outer=FALSE, side=3, las=1, line=1, adj=0, padj=1,cex = 2)
  # plot(x = x, y = y[-40]*-1, type = 'l', axes = FALSE, ylab = "Rotation Size", xlab = "Block (12 or 24 trials", xlim = c(1,20), cex.lab = 1.7)
  # axis(1, at = indx,cex.axis = 1.5)
  # axis(2, at = c(-30,-15,0,15,30),cex.axis = 1.5)
  # mtext('D', outer=FALSE, side=3, las=1, line=1, adj=0, padj=1,cex = 2)
  
  dev.off()
}

##looking at variability across blocks This needs to be looking at the variability within subjects not across subjects

variabilityplot<-function() {
  variation_localization<- read.csv("data/variation_localization.csv", header = TRUE)
  variation_reaches<- read.csv("data/variation_reaches.csv", header = TRUE) 
  
  trialsets <- list('1'=c(50:61),      "2" = c(62:73),   "4" = c(86:97), 
                    "5" = c(98:109),    "7" = c(134:157), "8" = c(158:181),
                    "9" = c(194:205),   "11" = c(218:229), "12" = c(254:265),
                    "13" = c(266:289),   "14" = c(290:313),   "15" = c(314:325),   "16" = c(326:337),
                    "17" = c(350:361),   "18" = c(362:373),   "19" = c(374:385),
                    "21" = c(410:421),   "22" = c(422:433),   "23" = c(434:445),   "24" = c(446:457))
  
  
  
  sdCIsr<- data.frame()
  sdCIsl<- data.frame()
  
  participants<- 2:33
  
  for (participant in participants){
    for (trial in 1:20){
      
      stop<-as.numeric(max(trialsets[[trial]]))
      start<- stop - 3
      
      sdCIsr[trial,participant-1]<-sd(variation_reaches[start:stop,participant], na.rm = TRUE)
      sdCIsl[trial,participant-1]<-sd(variation_localization[start:stop,participant], na.rm = TRUE)
      
    }
  }
  
  datar<- cbind(1:2, sdCIsr)
  plot(rowMeans(datar[,2:33], na.rm = TRUE), type = "l", col = "Blue", ylim = c(0,12), axes = FALSE, xlab = "Block of Trials (12 or 24)", ylab = "Standard Deviation [°]", main = "Variability Over Blocks")
  indx<- seq(from = 0, to = 20, by = 4)
  indx[1]<- 1
  axis(1, at = indx)
  axis(2, at = c(0,2,4,6,8,10,12), las = 2)
  dataCIs<- trialCI(datar)
  y <- c(dataCIs[, 1], rev(dataCIs[, 2]))
  x = c(c(1:20), rev(c(1:20)))
  polygon(x,y,col = rgb(0,0,1,.2), border = NA )
  legend(3,3, legend= c("Localizations, r2 = .01", "Reaches, r2 = .36*", "Regression"), col = c("Red", "Blue", "black"), lty = c(1,1,2), lwd = 1, bty = "n")
  reaches<-rowMeans(datar[,2:33], na.rm = TRUE)
  time<- 1:20
  stuff<- data.frame(reaches, time)
  model<-lm(reaches~time, data = stuff)
  lines(time, predict(model), col = "blue", lty = 2)
  
  data<- cbind(1:2, sdCIsl)
  lines(rowMeans(data[,2:33], na.rm = TRUE), col = "red")
  dataCIs<- trialCI(data)
  y <- c(dataCIs[, 1], rev(dataCIs[, 2]))
  x = c(c(1:20), rev(c(1:20)))
  polygon(x,y,col = rgb(1,0,0,.2), border = NA )
  localizations<-rowMeans(data[,2:33], na.rm = TRUE)
  time<- 1:20
  stuff<- data.frame(localizations, time)
  model<-lm(localizations~time, data = stuff)
  lines(time, predict(model), col = "red", lty = 2)
  summary(model)
  
  
  
}



# Comparing models for reach data
SimpleModelComparisonPlot<- function() {
  

  source('E:/Jenn/Documents/Varied_Prop_Adaptation/R/SignChangeModel.R')
  source('E:/Jenn/Documents/Varied_Prop_Adaptation/R/AttenuationModel.R')
  source('E:/Jenn/Documents/Varied_Prop_Adaptation/R/TwoRateModel.R')
  source('E:/Jenn/Documents/Varied_Prop_Adaptation/R/SizeChangeModel.R')
  source('E:/Jenn/Documents/Varied_Prop_Adaptation/R/SimpleTimeModel.R')

  source('R/SimpleTimeModel.R')
  source('R/SizeChangeModel.R')  
  source('R/SignChangeModel.R')
  source('R/AttenuationModel.R')
  source('R/TwoRateModel.R')

variation_reaches<- read.csv("data/variation_reaches.csv", header = TRUE) 
reaches<- rowMeans(variation_reaches[,2:33], na.rm = TRUE)
schedule<- variation_reaches$distortion
schedule[schedule == 360]<- NA
plotvariation()
par<-oneRateFit(schedule = schedule, reaches = reaches)
model<-oneRateModel(par,schedule)
oneRateMSE<-oneRateMSE(par,schedule,reaches)
lines(model*-1, lwd = 1.5, lty = 2, col = "green")

par1<-twoRateFit(schedule = schedule, reaches = reaches)
model<-twoRateModel(par1,schedule)
twoRateMSE<-twoRateMSE(par1,schedule,reaches)
lines(model$total*-1, lwd = 2, lty = 2, col = "brown")



pars<-SignChangeFit(schedule = schedule, reaches = reaches)
output<- SignChangeModel(pars, schedule)
SignChangeMSE<-SignChangeMSE(pars,schedule,reaches)
lines(output*-1, lwd = 2,lty = 5, col = "purple")


pars1<-threeRateFit(schedule = schedule, reaches = reaches)
modeloutput<- threeRateModel(pars1, schedule)
AttenutationMSE<-threeRateMSE(pars1,schedule,reaches)
lines(modeloutput*-1, lwd = 2,lty = 3, col = "Orange")


pars2<-SizeofChangeFit(schedule = schedule, reaches = reaches)
modeloutput1<- SizeofChangeModel(pars2, schedule)
SizeofChangeMSE<-SizeofChangeMSE(pars2,schedule,reaches)
lines(modeloutput1*-1, lwd = 2,lty = 4, col = "cyan")

pars3<-SimpleTimeFit(schedule = schedule, reaches = reaches)
modeloutput2<- SimpleTimeModel(pars3, schedule)
SimpleTimeMSE<-SimpleTimeMSE(pars3,schedule,reaches)
lines(modeloutput2*-1, lwd = 2,lty = 6, col = "black")


#text(x = 285, y = -16, labels = "brown is a two-rate model", col = "brown")
#text(x = 285, y = -24, labels = "Black is a simple time model", col = "black")
#text(x = 285, y = -18, labels = "cyan is Size of Change model", col = "cyan")
#text(x = 285, y = -30, labels = "Orange is attentuation model", col = "Orange")
#text(x = 285, y = -21, labels = "purple is Sign Change model", col = "purple")
#text(x = 285, y = -27, labels = "green is one-rate model", col = "green")

Modelnames<- c("OneRate", "TwoRate", "Time", "Number of Rotation", "Sign Changes", "Size of Change")
MSEs<- c(oneRateMSE, twoRateMSE,SimpleTimeMSE,AttenutationMSE, SignChangeMSE, SizeofChangeMSE)



legend(x = 208, y = -15, legend = c("One-Rate", "Two-Rate", "Time", "Rotation", "Size of Change", "Sign Change"), col = c("green", "brown", "grey", "orange", "cyan", "purple"), 
       lty = c(2,2,6,3,4,5), bty = 'n', ncol= 2)

return(data.frame(MSEs, Modelnames))

}

