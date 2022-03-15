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


plotblocks<- function (){
  
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
  
  
  svglite(file='figs/blocks_lasttrial.svg', width=15, height=21, system_fonts=list(sans = "Arial"))
  layout(matrix(c(1:40), nrow=8, byrow=TRUE), heights=c(1,1), widths = c(1,1))
  
  start<- min(which(blocks==1))
  stop<- max(which(blocks==1))
  plot(data[start:stop,1], type = "l", ylim = c(-20,30), xlab = "trials", ylab = "Hand Direction", bty = 'n', col = colors[1], lwd = 2, main = rotationsize[2])
  
  reachsig<-data[start:stop,1]
  pars<-asymptoticDecayFit(schedule = schedule, signal = reachsig)
  modeloutput<- asymptoticDecayModel(pars,schedule)
  lines(modeloutput$output, type = "l", col = "Blue")
  #legend(2,30, legend = c("data", "decay model"),bty = 'n', col = c(colors[1], "blue"))
  text(6,-14, sprintf('asymptote = %f', as.numeric(pars[2])))
  text(6,-17, sprintf('curve-endpoint = %f', modeloutput$output[12]))
  text(6,-20, sprintf('last trial = %f', endpoints[1]))
  print(pars)
  print(modeloutput$output[12])
  print(endpoints[2])
  
  
  for (i in 2:36){
    start<- min(which(blocks==i))
    stop<- max(which(blocks==i))
    plot((data[start:stop,1]-endpoints[i])*-1, type = "l", col = colors[i], ylim = c(-20,30), xlab = "trials in rotation", ylab = "Hand Direction",bty = 'n', lwd = 2,main = rotationsize[i+1]) 
    #legend(2,30, legend = c("data", "decay model"), col = c(colors[i], "blue"),bty = 'n')
    reachsig<-(data[start:stop,1]-endpoints[i])*-1
    pars<-asymptoticDecayFit(schedule = schedule, signal = reachsig)
    modeloutput<- asymptoticDecayModel(pars,schedule)
    lines(modeloutput$output, type = "l", col = "Blue")
    text(6,-14, sprintf('asymptote = %f', as.numeric(pars[2])))
    text(6,-17, sprintf('curve-endpoint = %f', modeloutput$output[12]))
    text(6,-20, sprintf('last trial = %f', endpoints[i+1]))
    print(pars)
    print(modeloutput$output[12])
    print(endpoints[i+1])
  }
  dev.off()
  
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


##  Plotting reaches and localization data separately for all changes in the rotation size (including sign) ##
prepdataforplotting<- function(data){

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
  
  rotdif$Abstdif<- abs(rotdif$tdif..37.)
  
  difs<-unique(rotdif$Abstdif)
  
  diftrials<- data.frame(matrix(NA, nrow = 8, ncol = 4))
  colnames(diftrials)<- c("15","30","45","60")
  
  diftrials[,1]<- rotdif$trials[rotdif$Abstdif == 15]
  
  
  diftrials[,2]<-c(rotdif$trials[rotdif$Abstdif == 30], NA,NA)
  
  
  diftrials[,3]<- c(rotdif$trials[rotdif$Abstdif == 45], NA,NA, NA,NA)
  
  zerodiftrials<<-rotdif$trials[rotdif$Abstdif == 0]
  
    diftrials[,4]<-c(rotdif$trials[rotdif$Abstdif == 60], NA,NA, NA,NA,NA,NA)
  
  return(diftrials)
}
plotdataperABSchange<-function(){
  
  
  variation_localization<- read.csv("data/Localizations_Baselined.csv", header = TRUE)
  variation_reaches<- read.csv("data/Reaches_Baselined.csv", header = TRUE) 
  diftrials<- prepdataforabsplotting(variation_reaches)
  
  
  task = "prop"
  title<- sprintf("%s per ABSOLUTE rotation change_endpoint removed", task)
  alldata<- variation_localization
  
  
  for (i in 1:nrow(alldata)){
    if (alldata$distortion[i] < 0)
      alldata[i,2:ncol(alldata)]<- alldata[i,2:ncol(alldata)]*-1
  } 
  
  linetype<- 5
  linetype2<- 3
  scale<- 1
  plotabschanges(task, diftrials,alldata,linetype, zerodiftrials,linetype2, title, scale)
  
  
  
  
  
  task = "reaches"
  
  alldata<- variation_reaches
  
  for (i in 1:nrow(alldata)){
    if (alldata$distortion[i] > 0) 
      alldata[i,2:ncol(alldata)]<- alldata[i,2:ncol(alldata)]*-1
    
    
  } 
  
  
  linetype <- 1
  linetype2<- 3
  title<- sprintf("%s per ABSOLUTE rotation change_endpoint removed", task)
  scale<- -1
  plotabschanges(task, diftrials,alldata,linetype, zerodiftrials, linetype2, title, scale)
  

  
}
plotabschanges<- function(task, diftrials,alldata, linetype, zerodiftrials, linetype2, title,scale){
  
  if (task == "prop"){
    model<- read.csv("ana/Decay Parameters localizations Data.csv", header = TRUE)
    modelendpoints<- model$Endpoint
  } else {
    model<- read.csv("ana/Decay Parameters reaches Data.csv", header = TRUE)
    modelendpoints<- model$Endpoint
  }

  
  #modelendpoints<- as.numeric(unlist(model[seq(from = 2, to = 143, by = 4),]))
  trialpoints<- c(seq(from = 61, to = 480, by = 12),480)
  
  # 15degree changes
  
  startloc<-as.numeric(unlist(strsplit(diftrials[1,1], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[1,1], " to ")))[2]
  #data<- alldata[startloc:stoploc,2:33]
  #data<- alldata[startloc:stoploc,2:33] - mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE)
  data<- alldata[startloc:stoploc,2:33] - modelendpoints[startloc-1 ==trialpoints]
  #print(mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))
  
  
  startloc<-as.numeric(unlist(strsplit(diftrials[2,1], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[2,1], " to ")))[2]
  #data<- cbind(data,(alldata[startloc:stoploc,2:33]))
  data<- cbind(data,(alldata[startloc:stoploc,2:33]- modelendpoints[startloc-1 ==trialpoints]))
  print(mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))
  
  
  startloc<-as.numeric(unlist(strsplit(diftrials[3,1], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[3,1], " to ")))[2]
  #data<- cbind(data,(alldata[startloc:stoploc,2:33]))
  #data<- cbind(data,(alldata[startloc:stoploc,2:33]- mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE)))
  data<- cbind(data,(alldata[startloc:stoploc,2:33]- modelendpoints[startloc-1 ==trialpoints]))
  print(mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))
  
  
  startloc<-as.numeric(unlist(strsplit(diftrials[4,1], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[4,1], " to ")))[2]
  #data<- cbind(data,(alldata[startloc:stoploc,2:33]))
  #data<- cbind(data,(alldata[startloc:stoploc,2:33]- mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE)))
  data<- cbind(data,(alldata[startloc:stoploc,2:33]- modelendpoints[startloc-1 ==trialpoints]))
  print(mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))
  
  
  startloc<-as.numeric(unlist(strsplit(diftrials[5,1], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[5,1], " to ")))[2]
  #data<- cbind(data,(alldata[startloc:stoploc,2:33]))
  #data<- cbind(data,(alldata[startloc:stoploc,2:33]- mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE)))
  data<- cbind(data,(alldata[startloc:stoploc,2:33]- modelendpoints[startloc-1 ==trialpoints]))
  print(mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))
  
  
  startloc<-as.numeric(unlist(strsplit(diftrials[6,1], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[6,1], " to ")))[2]
  #data<- cbind(data,(alldata[startloc:stoploc,2:33]))
  #data<- cbind(data,(alldata[startloc:stoploc,2:33]- mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE)))
  data<- cbind(data,(alldata[startloc:stoploc,2:33]- modelendpoints[startloc-1 ==trialpoints]))
  print(mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))
  
  
  startloc<-as.numeric(unlist(strsplit(diftrials[7,1], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[7,1], " to ")))[2]
  #data<- cbind(data,(alldata[startloc:stoploc,2:33]))
  #data<- cbind(data,(alldata[startloc:stoploc,2:33]- mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE)))
  data<- cbind(data,(alldata[startloc:stoploc,2:33]- modelendpoints[startloc-1 ==trialpoints]))
  print(mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))
  
  
  startloc<-as.numeric(unlist(strsplit(diftrials[8,1], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[8,1], " to ")))[2]
  #data<- cbind(data,(alldata[startloc:stoploc,2:33]))
  #data<- cbind(data,(alldata[startloc:stoploc,2:33]- mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE)))
  data<- cbind(data,(alldata[startloc:stoploc,2:33]- modelendpoints[startloc-1 ==trialpoints]))
  print(mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))
  plot((rowMeans(data, na.rm = TRUE)*scale), type = "l", col = "chartreuse",xlim = c(0,12.5),ylim = c(-15,25), xlab = "Trials in a Block", ylab = "Hand Deviation", main = title, lwd = 2, lty = linetype)
  text(x = 12.5, y = ((rowMeans(data, na.rm = TRUE))*scale)[12], "15",col = "chartreuse")
  
  
  #30 degree changes
  
  startloc<-as.numeric(unlist(strsplit(diftrials[1,2], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[1,2], " to ")))[2]
  #data<- alldata[startloc:stoploc,2:33]
  data<- alldata[startloc:stoploc,2:33]- mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE)
  print(mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))
  
  
  startloc<-as.numeric(unlist(strsplit(diftrials[2,2], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[2,2], " to ")))[2]
  #data<- cbind(data,(alldata[startloc:stoploc,2:33]))
  #data<- cbind(data,(alldata[startloc:stoploc,2:33]- mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE)))
  data<- cbind(data,(alldata[startloc:stoploc,2:33]- modelendpoints[startloc-1 ==trialpoints]))
  print(mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))
  
  
  startloc<-as.numeric(unlist(strsplit(diftrials[3,2], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[3,2], " to ")))[2]
  #data<- cbind(data,(alldata[startloc:stoploc,2:33]))
  #data<- cbind(data,(alldata[startloc:stoploc,2:33]- mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE)))
  data<- cbind(data,(alldata[startloc:stoploc,2:33]- modelendpoints[startloc-1 ==trialpoints]))
  print(mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))
  
  
  startloc<-as.numeric(unlist(strsplit(diftrials[4,2], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[4,2], " to ")))[2]
  #data<- cbind(data,(alldata[startloc:stoploc,2:33]))
  #data<- cbind(data,(alldata[startloc:stoploc,2:33]- mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE)))
  data<- cbind(data,(alldata[startloc:stoploc,2:33]- modelendpoints[startloc-1 ==trialpoints]))
  print(mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))
  
  
  startloc<-as.numeric(unlist(strsplit(diftrials[5,2], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[5,2], " to ")))[2]
  #data<- cbind(data,(alldata[startloc:stoploc,2:33]))
  #data<- cbind(data,(alldata[startloc:stoploc,2:33]- mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE)))
  data<- cbind(data,(alldata[startloc:stoploc,2:33]- modelendpoints[startloc-1 ==trialpoints]))
  print(mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))
  
  
  startloc<-as.numeric(unlist(strsplit(diftrials[6,2], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[6,2], " to ")))[2]
  #data<- cbind(data,(alldata[startloc:stoploc,2:33]))
  #data<- cbind(data,(alldata[startloc:stoploc,2:33]- mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE)))
  data<- cbind(data,(alldata[startloc:stoploc,2:33]- modelendpoints[startloc-1 ==trialpoints]))
  print(mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))
  
  
  lines((rowMeans(data, na.rm = TRUE)*scale), type = "l", col = "cyan", lwd = 2, lty = linetype)
  text(x = 12.5, y = ((rowMeans(data, na.rm = TRUE))*scale)[12], "30", col = "cyan")
  
  
  
  # 45 degree changes
  startloc<-as.numeric(unlist(strsplit(diftrials[1,3], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[1,3], " to ")))[2]
  #data<- alldata[startloc:stoploc,2:33]
  #data<- alldata[startloc:stoploc,2:33]- mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE)
  data<- alldata[startloc:stoploc,2:33]- modelendpoints[startloc-1 ==trialpoints]
  print(mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))
  
  
  startloc<-as.numeric(unlist(strsplit(diftrials[2,3], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[2,3], " to ")))[2]
  #data<- cbind(data,(alldata[startloc:stoploc,2:33]))
  #data<- cbind(data,(alldata[startloc:stoploc,2:33]- mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE)))
  data<- cbind(data,(alldata[startloc:stoploc,2:33]- modelendpoints[startloc-1 ==trialpoints]))
  print(mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))
  
  
  startloc<-as.numeric(unlist(strsplit(diftrials[3,3], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[3,3], " to ")))[2]
  #data<- cbind(data,(alldata[startloc:stoploc,2:33]))
  #data<- cbind(data,(alldata[startloc:stoploc,2:33]- mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE)))
  data<- cbind(data,(alldata[startloc:stoploc,2:33]- modelendpoints[startloc-1 ==trialpoints]))
  print(mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))
  
  startloc<-as.numeric(unlist(strsplit(diftrials[4,3], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[4,3], " to ")))[2]
  #data<- cbind(data,(alldata[startloc:stoploc,2:33]))
  #data<- cbind(data,(alldata[startloc:stoploc,2:33]- mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE)))
  data<- cbind(data,(alldata[startloc:stoploc,2:33]- modelendpoints[startloc-1 ==trialpoints]))
  print(mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))
  lines((rowMeans(data, na.rm = TRUE)*scale), type = "l", col = "darkorchid4", lty = linetype, lwd = 2)
  text(x = 12.5, y = ((rowMeans(data, na.rm = TRUE))*scale)[12], "45", col = "darkorchid4")
  
  
  
  #60 degree changes
  startloc<-as.numeric(unlist(strsplit(diftrials[1,4], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[1,4], " to ")))[2]
  #data<- cbind(data,(alldata[startloc:stoploc,2:33]))
  #data<- alldata[startloc:stoploc,2:33]- mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE)
  data<- alldata[startloc:stoploc,2:33]- modelendpoints[startloc-1 ==trialpoints]
  print(mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))
  
  
  startloc<-as.numeric(unlist(strsplit(diftrials[2,4], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(diftrials[2,4], " to ")))[2]
  #data<- cbind(data,(alldata[startloc:stoploc,2:33]))
  #data<- cbind(data,(alldata[startloc:stoploc,2:33]- mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE)))
  data<- cbind(data,(alldata[startloc:stoploc,2:33]- modelendpoints[startloc-1 ==trialpoints]))
  print(mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))
  lines((rowMeans(data, na.rm = TRUE)*scale), type = "l", col = "deeppink", lty = linetype, lwd = 2)
  text(x = 12.5, y = ((rowMeans(data, na.rm = TRUE))*scale)[12], "60", col = "deeppink")
  
  
  
  
  
  
  
  
  
  
  startloc<-as.numeric(unlist(strsplit(zerodiftrials[2], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(zerodiftrials[2], " to ")))[2]
  #data<- alldata[startloc:stoploc,2:33]
  data<- alldata[startloc:stoploc,2:33]- modelendpoints[startloc-1 ==trialpoints]
  print(mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))
  # lines((rowMeans(data, na.rm = TRUE)), type = "l", col = "cadetblue1", lty = linetype2, lwd = 2)
  # text(x = 12.5, y = ((rowMeans(data, na.rm = TRUE)))[12], "15", col = "cadetblue1")
  # 
  startloc<-as.numeric(unlist(strsplit(zerodiftrials[4], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(zerodiftrials[4], " to ")))[2]
  #data<- cbind(data,alldata[startloc:stoploc,2:33])
  data<- cbind(data,(alldata[startloc:stoploc,2:33]- modelendpoints[startloc-1 ==trialpoints]))
  print(mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))
  lines((rowMeans(data, na.rm = TRUE)), type = "l", col = "cadetblue1", lty = linetype2, lwd = 2)
  text(x = 12.5, y = ((rowMeans(data, na.rm = TRUE)))[12], "15", col = "cadetblue1")
  
  startloc<-as.numeric(unlist(strsplit(zerodiftrials[3], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(zerodiftrials[3], " to ")))[2]
  #data<- alldata[startloc:stoploc,2:33]
  data<- alldata[startloc:stoploc,2:33]- modelendpoints[startloc-1 ==trialpoints]
  print(mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))
  # lines((rowMeans(data, na.rm = TRUE)), type = "l", col = "chartreuse", lty = linetype2, lwd = 2)
  # text(x = 12.5, y = ((rowMeans(data, na.rm = TRUE)))[12], "-30", col = "chartreuse")
  
  
  startloc<-as.numeric(unlist(strsplit(zerodiftrials[5], " to ")))[1]
  stoploc<-as.numeric(unlist(strsplit(zerodiftrials[5], " to ")))[2]
  #data<- cbind(data,alldata[startloc:stoploc,2:33])
  data<- cbind(data,(alldata[startloc:stoploc,2:33]- modelendpoints[startloc-1 ==trialpoints]))
  print(mean(as.numeric(unlist(alldata[startloc-1,2:33])), na.rm = TRUE))
  lines((rowMeans(data, na.rm = TRUE)), type = "l", col = "chartreuse", lty = linetype2, lwd = 2)
  text(x = 12.5, y = ((rowMeans(data, na.rm = TRUE)))[12], "30", col = "chartreuse")

  text(x = 5, y = -10, "dotted lines are second 12 trials \n when rotation was repeated")
  
  
  
  
  
  
  
}








