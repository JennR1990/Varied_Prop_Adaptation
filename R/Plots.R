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

