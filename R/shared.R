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
  
  variation_reaches<<- removeReachOutliers(Loaddata(group='Reaches', task = 'Baselined'))
  variation_localizationsss<<- removeReachOutliers(Loaddata(group='Localizations', task = 'Baselined'))

  
}



downloadOSFdata <- function(update=FALSE) {
  
  # this pulls data from the OSF repo:
  files <- c('Localizations_Baselined.csv'  = 'https://osf.io/zcyvq/?action=download',
             'Reaches_Baselined.csv'       = 'https://osf.io/gjpm3/download',)
  

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


t.interval = function(data,
                      variance = var(data, na.rm = TRUE),
                      conf.level = 0.95) {
  z = qt((1 - conf.level) / 2,
         df = length(data) - 1,
         lower.tail = FALSE)
  
  xbar = mean(data, na.rm = TRUE)
  sdx = sqrt(variance / length(data))
  
  return(c(xbar - z * sdx, xbar + z * sdx))
  
}



