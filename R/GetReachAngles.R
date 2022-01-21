GetReachAngles<- function(experiment) {
  setwd("E:/Jenn/Documents/PhD Experiment/Time Model Good Data") 
  if (experiment == 1){
    participants<- c(1:32)
    distortion <-  c(rep(0,64),rep(30,160), rep(-30,16), rep(NA, 48))
  } else if (experiment == 2) {
    participants<- c(1:26,38,40:44)
    distortion <-  c(rep(0,64),rep(30,160), rep(-30,16), rep(NA, 48))
  } else if (experiment == 3) {
    participants<- c(1:32)
    P1table <- read.table('Time Model Variant 3 Selected Data/time_model3_1/1_1__time_model_3_reach_selected.txt')
    names(P1table)[1:5] <- c('task','block','trial','targetangle','rotation')
    temp <- aggregate(rotation ~ trial, data=P1table, FUN=mean)
    distortion <- as.numeric(c(temp$rotation))
    # distortion <-  c(rep(0,49),rep(-30,12), rep(-15,12), rep(0,12),rep(-15,12), rep(15,12), rep(0,12), rep(0,12), rep(15,12), rep(15,12), rep(-30,12), rep(-30,12), rep(0,12), rep(-30,12), rep(0,12), rep(30,12), rep(0,12), rep(0,12), rep(30,12), rep(-15,12), rep(-15,12), rep(30,12), rep(30,12), rep(15,12), rep(-15,12), rep(0,12), rep(-15,12), rep(-30,12), rep(30,12), rep(0,12), rep(0,12), rep(15,12), rep(30,12), rep(-30,12), rep(15,12), rep(0,12), rep(0,11))
  } else if (experiment == 4) {
    participants<- c(1:32)
    distortion <-  c(rep(0,32),rep(30,160), rep(-30,16), rep(NA, 48))
  } else if (experiment == 5) {
    participants<- c(1:18,20:27,29:34)
    distortion <-  c(rep(0,64),rep(30,160), rep(-30,16), rep(NA, 48))
  } else if (experiment == 6) {
    participants<- c(1:16)
    distortion <-  c(rep(0,32),rep(30,160), rep(-30,16), rep(NA, 48))
  }
  
  expangles<- data.frame(distortion)
  
  for (participant in participants){
    
    partiangles<-GetReachData(participant = participant, experiment = experiment)
    print(participant)
    expangles[,sprintf('p%d',participant)] <- partiangles$reachdeviations
    
  }
  outputfilename<- sprintf('../../Varied_Prop_Adaptation/data/time_model%d_Reach_Angles.csv', experiment)
  
  write.csv(expangles, file = outputfilename,  row.names = F, quote = F)
}



GetReachData<- function(participant, experiment) {
  
  filenames <- GetReachFilenames(participant, experiment)
  
  ppangles <- data.frame()
  
  for (filename in filenames) {
    
    reachNangles <- CalculateReachAngles(filename, experiment)
    
    if (prod(dim(ppangles)) == 0) {
      ppangles <- reachNangles
    } else {
      ppangles <- rbind(ppangles, reachNangles)
    }
    
  }
  return(ppangles)
  
}

GetReachFilenames<- function (ppn, expn) {
  
  if (expn == 1) {
    tasknumbers <- c(1:4)
    
    expfolder <- '../Time Model Good Data/Time Model Variant 1 Selected Data/'
    
    ppfolder <- sprintf('time_model1_%d/',ppn)
    
    
    filenames <- c()
    
    for (taskno in tasknumbers) {
      
      filenames <- c(filenames, sprintf('%s%s%d_%d__time_model_prop_selected.txt',expfolder,ppfolder,ppn,taskno))
    }
  } else if (expn == 2) {
    tasknumbers <- c(1:4)
    
    expfolder <- '../Time Model Good Data/Time Model Exposure Data/'
    
    ppfolder <- sprintf('time_model1_%d/',ppn)
    filenames <- c()
    
    for (taskno in tasknumbers) {
      
      filenames <- c(filenames, sprintf('%s%s%d_%d__time_model_Prop_selected.txt',expfolder,ppfolder,ppn,taskno))
    }
  } else if (expn == 3) {
    tasknumbers <- c(1)
    
    expfolder <- '../Time Model Good Data/Time Model Variant 3 Selected Data/'
    
    ppfolder <- sprintf('time_model3_%d/',ppn)
    filenames <- c()
    
    for (taskno in tasknumbers) {
      
      filenames <- c(filenames, sprintf('%s%s%d_%d__time_model_3_Prop_selected.txt',expfolder,ppfolder,ppn,taskno))
    }
  } else if (expn == 4) {
    tasknumbers <- c(2:5)
    
    expfolder <- '../Time Model Good Data/Time Model Variant 4 Selected Data/'
    
    ppfolder <- sprintf('time_model_nocursor_%d/',ppn)
    filenames <- c()
    
    for (taskno in tasknumbers) {
      
      filenames <- c(filenames, sprintf('%s%s%d_%d__time_model2_NoC_selected.txt',expfolder,ppfolder,ppn,taskno))
    }
  } else if (expn == 5) {
    tasknumbers <- c(1:4)
    
    expfolder <- '../Time Model Good Data/Time Model Terminal Selected Data/'
    
    ppfolder <- sprintf('time_model1_%d/',ppn)
    filenames <- c()
    
    for (taskno in tasknumbers) {
      
      filenames <- c(filenames, sprintf('%s%s%d_%d__time_model_prop_selected.txt',expfolder,ppfolder,ppn,taskno))
    }
  } else if (expn == 6) {
    tasknumbers <- c(2:5)
    
    expfolder <- '../Time Model Good Data/Time Model - No Cursor New Instructions Selected Data/'
    
    ppfolder <- sprintf('time_model_nocursor_%d/',ppn)
    filenames <- c()
    
    for (taskno in tasknumbers) {
      
      filenames <- c(filenames, sprintf('%s%s%d_%d__time_model2_NoC_selected.txt',expfolder,ppfolder,ppn,taskno))
    }
  }
  return(filenames) 
}






LoadReachFile <- function(filename) {
  df<-read.table(filename, header = TRUE)
  #colnames(df)<-c('task', 'block','tria', 'targetangle_deg', 'time','handx_cm', 'handy_cm', 'tapx_cm', 'tapy_cm', 'selected')
  colnames(df)<-c('participant','block','trial','targetangle_deg','rotation_deg', 'time_ms', 'cursorx_cm','cursory_cm',	'handx_cm',	'handy_cm',	'homex_cm', 'homey_cm',	'targetx_cm','targety_cm','step','trialselected', 'sampleselected', 'sampleinterpolated', "maxvelocity")
  #colnames(df)<-c('participant','block','trial','targetangle_deg','rotation_deg', 'time_ms', 'cursorx_cm','cursory_cm',	'handx_cm',	'handy_cm',	'homex_cm', 'homey_cm',	'targetx_cm','targety_cm','step')
  # print(str(df))
  # df$cursory_cm <- df$cursory_cm +8.5
  # df$handy_cm <- df$handy_cm +8.5
  # df$targety_cm <- df$targety_cm +8.5
  # df$homey_cm <- df$homey_cm +8.5
  df$targetangle_deg <- (-1 *(df$targetangle_deg - 90)) + 90
  return(df)
}


CalculateReachAngles<- function (filename, experiment) {
  
  df<-LoadReachFile(filename = filename)
  blocknos <- unique(df$block)
  
  
  if (experiment == 4) {
    reachdeviations<- rep(NA, times= 48)
    for (i in 1:length(blocknos))
      reachdeviations[i]<- unique(df$targetangle_deg[df$block == blocknos[i]])
    targetangles <- blocknos
  } else {
    reachdeviations <- df$targetangle_deg
    targetangles <- blocknos
  }  
  
  
  return(data.frame(reachdeviations,targetangles))
  
  
}















