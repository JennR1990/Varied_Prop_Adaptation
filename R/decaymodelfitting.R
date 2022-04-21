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



