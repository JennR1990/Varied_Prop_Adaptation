getallparticipants<- function(experiment) {
  
  if (experiment == 1){
    participants<- c(1:32)
    distortion <-  c(rep(0,64),rep(30,160), rep(-30,16), rep(NA, 48))
    #distortion <-  c( rep(NA, 48))
  } else if (experiment == 2) {
    participants<- c(1:32)
    distortion <-  c(rep(0,96),rep(30,160), rep(-30,16), rep(NA, 48)) 
  } else if (experiment == 3) {
    participants<- c(1:32)
    P1table <- read.table('Time Model Variant 3 Selected Data/time_model3_1/1_1__time_model_3_reach_selected.txt')
    names(P1table)[1:5] <- c('task','block','trial','targetangle','rotation')
    temp <- aggregate(rotation ~ trial, data=P1table, FUN=mean)
    distortion <- as.numeric(c(temp$rotation))
    # distortion <-  c(rep(0,49),rep(-30,12), rep(-15,12), rep(0,12),rep(-15,12), rep(15,12), rep(0,12), rep(0,12), rep(15,12), rep(15,12), rep(-30,12), rep(-30,12), rep(0,12), rep(-30,12), rep(0,12), rep(30,12), rep(0,12), rep(0,12), rep(30,12), rep(-15,12), rep(-15,12), rep(30,12), rep(30,12), rep(15,12), rep(-15,12), rep(0,12), rep(-15,12), rep(-30,12), rep(30,12), rep(0,12), rep(0,12), rep(15,12), rep(30,12), rep(-30,12), rep(15,12), rep(0,12), rep(0,11))
  } else if (experiment == 4) {
    participants<- c(1:32)
    distortion <-  c(rep(0,96),rep(30,160), rep(-30,16), rep(NA, 48))
  } else if (experiment == 5) {
    participants<- c(1:32)
    distortion <-  c(rep(0,64),rep(30,160), rep(-30,16), rep(NA, 48))
  } else if (experiment == 6) {
    participants<- c(1:16)
    distortion <-  c(rep(0,96),rep(30,160), rep(-30,16), rep(NA, 48))
  } else if (experiment == 7) {
    participants<- c(1:18, 20:27, 29:34)
    distortion <-  c(rep(0,64),rep(30,160), rep(-30,16), rep(NA, 48))
  }
  
  expangles<- data.frame(distortion)
  
  print(participants)
  for (participant in participants){
    
    partiangles<-getparticipantdata(participant = participant, experiment = experiment)
    print(participant)
    print(dim(partiangles))

    baselinedangles <- baselinebyaligned(df = partiangles, experiment = experiment, dist = distortion)
    #print(mean(baselinedangles$reachdeviations[32:64], na.rm = TRUE))
    print(experiment)
    if (experiment == 3) {
      expangles[,sprintf('p%d',participant)] <- baselinedangles$reaches[1:480]
    } else if (experiment %in% c(1,5,7)) {
      expangles[,sprintf('p%d',participant)] <- baselinedangles$reaches[1:288] 
    } else if (experiment %in% c(2,4,6)) {
      print(nrow(expangles))
      expangles[,sprintf('p%d',participant)] <- baselinedangles$reaches[1:320]
    }
  }
  outputfilename<- sprintf('time_model%d_Reaches.csv', experiment)
  
  write.csv(expangles, file = outputfilename,  row.names = F, quote = F)
}






baselinebyaligned<- function(df, experiment, dist) {
  #print(head(df))
  angles<-df$targetangles
  reaches<- df$reachdeviations
  if (length(reaches)<length(dist)){
    reaches<- c(reaches, rep(NA, times = (length(dist)-length(reaches))))
    angles<- c(angles, rep(NA, times = (length(dist)-length(angles))))
  }
  #take an average of the aligned data where distortion = 0
  #subtract that from reach angles
  if (experiment == 1| experiment == 5| experiment == 7) {
    bias<-mean(reaches[32:64], na.rm = TRUE)
    reaches[1:288]<- reaches[1:288] - bias
    #reaches[1:48]<- reaches[1:48] - 0
    # print(df$reachdeviations[1:288])
  } else if (experiment == 2 | experiment == 4| experiment == 6) {
    bias<-mean(df$reachdeviations[64:96], na.rm = TRUE)
    reaches[1:320]<- reaches[1:320] - bias
  } else if (experiment == 3) {
    bias<-mean(df$reachdeviations[23:49], na.rm = TRUE)
    reaches <- reaches- bias
  } else {
    print('hi')
  }

  data<- data.frame(reaches, angles)
  return(data)
}




getparticipantdata<- function(participant, experiment) {
  
  filenames <- getfilenames(participant, experiment)
  
  ppangles <- data.frame()
  
  for (filename in filenames) {
    
    reachNangles <- getanglesfortask(filename)
    
    if (prod(dim(ppangles)) == 0) {
      ppangles <- reachNangles
    } else {
      ppangles <- rbind(ppangles, reachNangles)
    }
    
  }
  return(ppangles)
  
}

getfilenames<- function (ppn, expn) {
  
  if (expn == 1) {
    tasknumbers <- c(1:4)
    
    expfolder <- '../Time Model Good Data/Time Model Variant 1 Selected Data/'
    
    ppfolder <- sprintf('time_model1_%d/',ppn)
    
    
    filenames <- c()
    
    for (taskno in tasknumbers) {
      
      filenames <- c(filenames, sprintf('%s%s%d_%d__time_model_reach_selected.txt',expfolder,ppfolder,ppn,taskno))
    }
  } else if (expn == 2) {
    tasknumbers <- c(1:5)
    
    expfolder <- '../Time Model Good Data/Time Model Variant 2 Selected Data/'
    
    ppfolder <- sprintf('time_model2_%d/',ppn)
    filenames <- c()
    
    for (taskno in tasknumbers) {
      
      filenames <- c(filenames, sprintf('%s%s%d_%d__time_model2_reach_selected.txt',expfolder,ppfolder,ppn,taskno))
    }
  } else if (expn == 3) {
    tasknumbers <- c(1)
    
    expfolder <- '../Time Model Good Data/Time Model Variant 3 Selected Data/'
    
    ppfolder <- sprintf('time_model3_%d/',ppn)
    filenames <- c()
    
    for (taskno in tasknumbers) {
      
      filenames <- c(filenames, sprintf('%s%s%d_%d__time_model_3_reach_selected.txt',expfolder,ppfolder,ppn,taskno))
    }
  } else if (expn == 4) {
    tasknumbers <- c(1:5)
    
    expfolder <- '../Time Model Good Data/Time Model Variant 4 Selected Data/'
    
    ppfolder <- sprintf('time_model_nocursor_%d/',ppn)
    filenames <- c()
    
    for (taskno in tasknumbers) {
      
      filenames <- c(filenames, sprintf('%s%s%d_%d__time_model2_reach_selected.txt',expfolder,ppfolder,ppn,taskno))
    }
  } else if (expn == 5) {
    tasknumbers <- c(1:4)
    
    expfolder <- '../Time Model Good Data/Time Model Variant 5 Raw & Selected Data/'
    
    ppfolder <- sprintf('time_model_activeloc_%d/',ppn)
    filenames <- c()
    
    for (taskno in tasknumbers) {
      
      filenames <- c(filenames, sprintf('%s%s%d_%d__time_model4_reach_selected.txt',expfolder,ppfolder,ppn,taskno))
    }
  } else if (expn == 6) {
    tasknumbers <- c(1:5)
    
    expfolder <- '../Time Model Good Data/Time Model - No Cursor New Instructions Selected Data/'
    
    ppfolder <- sprintf('time_model_nocursor_%d/',ppn)
    filenames <- c()
    
    for (taskno in tasknumbers) {
      
      filenames <- c(filenames, sprintf('%s%s%d_%d__time_model2_reach_selected.txt',expfolder,ppfolder,ppn,taskno))
    }
  }  else if (expn == 7) {
    tasknumbers <- c(1:4)
    
    expfolder <- '../Time Model Good Data/Time Model Terminal Selected Data/'
    
    ppfolder <- sprintf('time_model1_%d/',ppn)
    filenames <- c()
    
    for (taskno in tasknumbers) {
      
      filenames <- c(filenames, sprintf('%s%s%d_%d__time_model_reach_selected.txt',expfolder,ppfolder,ppn,taskno))
    }
  }
  return(filenames) 
}






loadreachfile <- function(filename) {
  df<-read.table(filename)
  if (ncol(df) == 21){
    colnames(df)<-c('participant','block','trial','targetangle_deg','rotation_deg', 'time_ms', 'cursorx_cm','cursory_cm',	'handx_cm',	'handy_cm',	'homex_cm', 'homey_cm',	'targetx_cm','targety_cm','step','clamped','trialselected', 'sampleselected', 'sampleinterpolated', "maxvelocity")
  } else{
    colnames(df)<-c('participant','block','trial','targetangle_deg','rotation_deg', 'time_ms', 'cursorx_cm','cursory_cm',	'handx_cm',	'handy_cm',	'homex_cm', 'homey_cm',	'targetx_cm','targety_cm','step', 'trialselected', 'sampleselected', 'sampleinterpolated', "maxvelocity")  
  }
  print(class(df$cursory_cm))
  df$cursory_cm <- df$cursory_cm +8.5
  df$handy_cm <- df$handy_cm +8.5
  df$targety_cm <- df$targety_cm +8.5
  df$homey_cm <- df$homey_cm +8.5
  df$targetangle_deg <- (-1 *(df$targetangle_deg - 90)) + 90
  return(df)
}


getanglesfortask<- function (filename) {
  df<-loadreachfile(filename = filename)
  blocknos <- unique(df$block)
  reachdeviations <- c()
  targetangles <- c()
  
  for (blockno in blocknos) {
    trialdf <- df[which(df$block == blockno),]
    angles <- getTrialReachAngleAt(trialdf)
    reachdeviations <- c(reachdeviations, angles[1])
    targetangles <- c(targetangles, angles[2])
    
  }
  return(data.frame(reachdeviations,targetangles))
  
  #the below code works to make this function automatically write the file for us, but it needs to make a bunch of files and then put all those together
  #so this isn't the best way yet. 
  #angles<-data.frame(reachdeviations,targetangles)
  #write.csv(angles, file = "aligned_output.csv")
  
}

rotateTrajectory <- function(X,Y,angle) {
  
  # create rotation matrix to rotate the X,Y coordinates by angle degrees
  th <- (angle/180) * pi
  R <- t(matrix(data=c(cos(th),sin(th),-sin(th),cos(th)),nrow=2,ncol=2))
  
  # put coordinates in a matrix as well
  coordinates <- matrix(data=c(X,Y),ncol=2)
  
  # rotate the coordinates
  Rcoordinates <- coordinates %*% R
  
  # return the rotated reach
  return(Rcoordinates)
  
}


getTrialReachAngleAt <- function(trialdf, location='maxvel') {
  
  
  # location (string) determines where the angle of the reach is
  #determines, it is one of:
  # maxvel: maximum velocity (default)
  # endpoint: end of the reach
  # cmX: the last sample before this distance from home, where X is
  # replaced by a numeral
  
  # return a matrix of two numbers:
  reachangle = matrix(data=NA,nrow=1,ncol=2)
  
  
  # we extract the target angle
  angle <- trialdf[1,'targetangle_deg']
  
  # so we can at least return that:
  reachangle[1,2] <- angle
  
  # if the trial was rejected, return empty matrix now
  if (trialdf[1,'trialselected'] == 0) {
    
    return(reachangle);
    
  }
  
  # extract the relevant reach information
  
  X <- trialdf[trialdf$sampleselected == 1,'handx_cm']
  Y <- trialdf[trialdf$sampleselected == 1,'handy_cm']
  MV <- trialdf[trialdf$sampleselected == 1,'maxvelocity']
  
  # rotate the trajectory
  # (this avoids problems in the output of atan2 for large angles)
  trajectory <- rotateTrajectory(X,Y,-1*angle)
  X <- trajectory[,1]
  Y <- trajectory[,2]
  
  # now try find the specified location in this reach:
  # if we can't find it, we need to know
  invalidlocation <- TRUE
  
  # maximum velocity, should be in the data
  if (location == 'maxvel') {
    rown <- which(MV == 1)
    if (length(rown) > 1) {
      rown <- rown[1]
    }
    if (length(rown) == 0) {
      # no maximum velocity defined!
      return(reachangle)
    }
    invalidlocation <- FALSE
  }
  # end point, just the last point in the selected stretch of the reach
  if (location == 'endpoint') {
    rown <- length(X)
    invalidlocation <- FALSE
  }
  # cutoff in centimers, the last sample before this cutoff distance is
  # reached
  # this assumes that people don't go back, or that there is only one
  #movement from home to target
  if (substring(location,1,2) == 'cm') {
    distance <- as.numeric(substring(location, 3))
    
    # get the distance from home:
    dist <- sqrt(X^2 + Y^2)
    
    # if there are no selected samples below 3 cm: return NAs
    if (length(which(dist < distance)) == 0) {
      return(reachangle)
    }
    
    # find the last sample, where dist < 3
    rown <- max(which(dist < distance))
    invalidlocation <- FALSE
  }
  
  # if we don't have a valid location, we can't calculate an angle to return
  if (invalidlocation) {
    return(reachangle)
  }
  
  # calculate the angle at that point for the rotated trajectory
  # this is the angular deviation we are looking for
  angulardeviation <- (atan2(Y[rown],X[rown]) / pi) * 180
  
  # put the result in the little matrix:
  reachangle[1,1] <- angulardeviation
  
  return(reachangle)
  
}