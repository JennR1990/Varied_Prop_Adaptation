getreachesformodel<- function(data) {
  meanreaches<-rowMeans(data[,2:ncol(data)], na.rm=TRUE)
  distortion<- data$distortion
  return(data.frame(meanreaches,distortion))
}

Loaddata<- function (group='passive', task='reaches') {
  # filename <- sprintf('data/%s_%s.csv',group,task)
  # df <- read.csv(filename, stringsAsFactors=F)
  return(read.csv(sprintf('data/%s_%s.csv',group,task), stringsAsFactors=F))
}
 
loadalldata<- function () {
  pause_reaches<<- removeReachOutliers(Loaddata(group='pause'))
  active_reaches<<- removeReachOutliers(Loaddata(group='active'))
  passive_reaches<<- removeReachOutliers(Loaddata())
  nocursor_reaches<<- removeReachOutliers(Loaddata(group='nocursor'))
  nocursorI_reaches<<- removeReachOutliers(Loaddata(group='nocursor', task = 'NI_reaches'))
  passive_localization<<- removeReachOutliers(Loaddata(task = 'localization'))
  active_localization<<- removeReachOutliers(Loaddata(group='active', task = 'localization'))
  nocursor_nocursors<<- removeReachOutliers(Loaddata(group='nocursor', task = 'nocursors'))
  nocursorI_nocursors<<- removeReachOutliers(Loaddata(group='nocursor', task = 'NI_nocursors'))
  nocursorI_reaches<<-nocursorI_reaches[,-9]
  nocursorI_nocursors<<-nocursorI_nocursors[,-9]
  newnocursor_reaches<<- cbind(nocursor_reaches, nocursorI_reaches[2:ncol(nocursorI_reaches)])
  newnocursor_nocursors<<- cbind(nocursor_nocursors, nocursorI_nocursors[2:ncol(nocursorI_nocursors)])
  terminal_reaches<<- removeReachOutliers(Loaddata(group='terminal'))
  terminal_localization<<- removeReachOutliers(Loaddata(group='terminal', task = 'localization'))
  pause_angles<<- Loaddata(group='Pause', task = "Angles")
  active_angles<<- Loaddata(group='Active', task = "Angles")
  passive_angles<<- Loaddata(group = "Pause", task = "Angles")
  nocursor_angles<<- Loaddata(group='No-Cursor', task = "Angles")
  
  Instructed<<- Loaddata(group = "Instructed_No-Cursors", task = "MovementTimes")
  uninstructed<<- Loaddata(group="Uninstructed_No-Cursors", task = "MovementTimes")
  
  passive_prop_angles<<- Loaddata(group='Passive_Tap', task = "Angles")
  active_prop_angles<<- Loaddata(group='Active_Tap', task = "Angles")
  
  no_cursorm<<-  removeReachOutliers(Loaddata(group = "no-cursor_maxvel", task = "uninstructed"))
  no_cursormI<<- removeReachOutliers(Loaddata(group="no-cursor_maxvel", task = "instructed"))
  no_cursormI<- no_cursormI[,-9]
  
}

fixnocursorcolnames<- function () {
  
  names<-colnames(newnocursor_reaches)
  newnames<- c('p33','p34','p35','p36','p37','p38','p39','p40','p41','p42','p43','p44','p45','p46','p47')
  names<- c(names[1:33], newnames)
  colnames(newnocursor_reaches)<<- names
  colnames(newnocursor_nocursors)<<- names
}

downloadOSFdata <- function(update=FALSE) {
  
  # this pulls data from the OSF repo:
  files <- c('active_localization.csv'  = 'https://osf.io/mc523/?action=download',
             'active_reaches.csv'       = 'https://osf.io/ejxy9/download',
             'nocursor_nocursors.csv'   = 'https://osf.io/5b8s9/download',
             'nocursor_reaches.csv'     = 'https://osf.io/vmnx7/download',
             'nocursor_NI_nocursors.csv'   = 'https://osf.io/y4k2x/download',
             'nocursor_NI_reaches.csv'     = 'https://osf.io/grnxh/download',
             'passive_localization.csv' = 'https://osf.io/27v54/download',
             'passive_reaches.csv'      = 'https://osf.io/mq5av/download',
             'pause_reaches.csv'        = 'https://osf.io/q59b3/download',
             'terminal_reaches.csv'     = 'https://osf.io/qdk9y/download',
             'terminal_localization.csv'= 'https://osf.io/a9sx5/download',
             'Active_Angles.csv'  = 'https://osf.io/ubdv8/?action=download',
             'Passive_Angles.csv'       = 'https://osf.io/3nsqm/download',
             'Pause_Angles.csv'   = 'https://osf.io/36cqd/download',
             'No-Cursor_Angles.csv'     = 'https://osf.io/jyz2n/download',
             'Instructed_No-Cursors_MovementTimes.csv'   = 'https://osf.io/8n3c6/download',
             'Uninstructed_No-Cursors_MovementTimes.csv'     = 'https://osf.io/k4pze/download',
             'Active_Tap_Angles.csv'  = 'https://osf.io/vkrs6/?action=download',
             'Passive_Tap_Angles.csv'       = 'https://osf.io/f67m5/download',
             'no-cursor_maxvel_instructed.csv'   = 'https://osf.io/62jbk/download',
             'no-cursor_maxvel_uninstructed.csv'     = 'https://osf.io/zmcpf/download')
  

  # check if data directory exists and create if necessary:
  # (data should come from OSF, so is not on github)
  if (!dir.exists('data')) {
    dir.create('data')
  }
  
  # check if each file exists and copy it if necessary: 
  for (filename in names(files)) {
    
    filepath <- sprintf('data/%s',filename)
    
    if (!file.exists(filepath) | update) {
      
      df <- read.csv(url(files[filename]), stringsAsFactors=F)
      write.csv(df, filepath, quote=FALSE, row.names=FALSE)
      
    }
    
  }
  
}


percentNAs <- function (df) {
  return((sum(is.na(df))/prod(dim(df)))*100)
}





twoRates <- function(groupdata, estN='ac_one', compOne=FALSE) {
  
  groupnames <- names(groupdata)
  
  if (is.numeric(estN)) {
    
    if (!all(groupnames %in% names(estN))) {
      stop('Not all groups in the data have an N specified.\n')
    } else {
      # all is good
      # we put this in a vector that aclag will create:
      observations <- estN
    }
    
    
    # all should be good now...
  } else if (is.character(estN)) {
    
    observations <- c()
    
    # determine number of independent observations for each group:
    for (group in groupnames) {
      
      # use reaches to determine number of independent observations:
      reaches <- groupdata[group]$reaches
      
      observations[group] <- seriesEffectiveSampleSize(reaches, method=estN)
      
    }
    
  } else {
    stop('Either specify "estN" as a numeric vector of N or a character naming an available method.\n')
  }
  
  # now do the actual modelling on all datasets?
  
  
  
  
}


seriesEffectiveSampleSize <- function(series, method='ac_one') {
 
  
  if (method == 'ac_one') {
    
    # create empty vector for number of independent observations per group:
    observations <- c()
    
    rho <- acf(series, lag.max=1, plot=FALSE)$acf[2]
    
    return( length(series) *  ((1-rho)/(1+rho)) )
    
  } else if (method == 'ac_lag.10') {
    
    critlag <- which(acf(series, lag.max=length(series)-1, plot=FALSE)$acf < 0.1)
    
    if (length(critlag) == 0) {
      
      # autocorrelation is high throughout: we have 1 observation?
      
      return(1)
      
    } else {
      
      # the sequence of reaches doesn't autocorrelate well throughout,
      # so it can be split into (more or less) independent observations
      # (but we have at least 1)
      
      return( max( ( length(series) / (critlag[1]-2) ), 1) )
      
    }
    
  } else if (method == 'ac_lag95%CI') {
    
    Neff_found <- FALSE
    
    critlag <- 1
    
    Neff <- length(series)
    
    while (!Neff_found) {
      
      lagpoints <- length(series) - critlag
      
      point_one <- series[c(1:lagpoints)]
      point_two <- series[c((critlag+1):length(series))]
      
      lag_cor <- cor(point_one, point_two)
      
      shuffle_cor <- rep(NA, 1000)
      
      for (bootstrap in c(1:1000)) {
        
        shuffle_cor[bootstrap] <- cor(point_one, sample(point_two))
        
      }
      
      upperlimit <- quantile(shuffle_cor, probs=0.95)
      
      if (lag_cor < upperlimit) {
        
        return( length(series) / max((critlag - 1), 1) )
        
      }
      
      critlag <- critlag + 1
      
      # lag can only go up to a certain value, determined by the length of the sequence
      if (critlag > (length(series) - 2)) {
        
        return( length(series) ) # or length(reaches) - 1?
        
      }
      
    }
    
  }
  
  stop('Unrecognized method for determining effective sample size.\nUse one of: ac_one, ac_lag.10 or ac_lag95%CI\n')
  
}


# OUTLIER REMOVAL ---------------------------------------------------------

removeSDoutliers <- function(values, sds=3) {
  
  avg <- mean(values, na.rm=TRUE)
  std <- sd(values, na.rm=TRUE) * sds
  
  values[values > avg + std] <- NA
  values[values < avg - std] <- NA
  
  return(values)
  
}

removeIQRoutliers <- function(values, range=3) {
  
  bp <- boxplot(values, range=3, plot=FALSE)
  
  values[values %in% bp$out] <- NA
  
  return(values)
  
}


removeReachOutliers <- function(data) {
  
  ntrials <- nrow(data)
  
  for (trialn in c(1:ntrials)) {
    
    data[trialn,2:ncol(data)] <- removeSDoutliers(as.numeric(unlist(data[trialn,2:ncol(data)])))
    
  }
  
  return(data)
  
}



