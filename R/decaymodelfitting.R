decaymodelfitting<-function(data, task){
  library(svglite)
  source('E:/Jenn/Documents/Varied_Prop_Adaptation/R/shared.R')
  source('E:/Jenn/Documents/Varied_Prop_Adaptation/R/asymptoticDecayModel.R')
  data<- getreachesformodel(data)
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
  
  
  Allpars<- c()
  modeloutputs<- c()
  endpoints<-c()
  rotationsize<- c()
  for (i in 0:36){
    stop<- max(which(blocks==i))
    endpoints[i+1]<-as.numeric(unlist(data[stop,1]))
    rotationsize[i+1]<- as.numeric(unlist(data[stop,2]))
  }
  
  outputfile<-sprintf('figs/blocks_lasttrial_%s.svg', task)
  svglite(file=outputfile, width=15, height=21, system_fonts=list(sans = "Arial"))
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
  Allpars<- pars
  print(modeloutput$output[12])
  modeloutputs<- modeloutput$output[12]
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
    Allpars<- rbind(Allpars,pars)
    print(modeloutput$output[12])
    modeloutputs<- c(modeloutputs, modeloutput$output[12])
    print(endpoints[i+1])
  }
  dev.off()
  
  info<-data.frame(Allpars,modeloutputs,endpoints[2:37],rotationsize[2:37])
  colnames(info)<- c('learning Rate', "Asymptote", "ModelOutput", "Endpoint", "Rotation")
  outputfile<- sprintf("data/Decay Parameters %s Data.csv", task)
  write.csv(info, file =outputfile , quote = FALSE, row.names = FALSE)
  
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
