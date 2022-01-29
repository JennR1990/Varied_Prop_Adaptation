
# single explonential model with asymptote -------------------

asymptoticDecayModel <- function(par, schedule) {
  
  # the process and error states are initialized at 0:
  Pt <- 0
  Et <- 0

  # the total output is stored here:
  output <- c()

  for (t in c(1:length(schedule))) {

    Pt <- Pt - (par['lambda'] * Et)

    # now we calculate what the previous error will be for the next trial:
    if (is.na(schedule[t])) {
      Et <- 0
    } else {
      Et <- Pt + (schedule[t] * par['N0'])
    }

    # at this point we save the process state in our vector:
    output <- c(output, Pt)

  }
  
  return(data.frame(output=output))
  
  #return(data.frame(output=par['N0'] - (par['N0'] * exp(par['lambda']*schedule))))
  
}

asymptoticDecayMSE <- function(par, schedule, signal, N0=FALSE) {
  
  if (N0) {
    par['N0'] = N0
  }
  
  adm <- asymptoticDecayModel(par, schedule)
  
  #print(c(length(adm$output), length(signal)))
  
  MSE <- mean((adm$output - signal)^2, na.rm=TRUE)
  
  return( MSE )
  
}

asymptoticDecayFit <- function(schedule, signal, gridpoints=11, gridfits=10, setAsymptote=FALSE, useOptimx=FALSE) {
  
  # set the search grid:
  parvals <- seq(1/gridpoints/2,1-(1/gridpoints/2),1/gridpoints)
  
  maxAsymptote <- 2*max(abs(signal), na.rm=TRUE)
  
  # define the search grid:
  if (setAsymptote) {
    searchgrid <- expand.grid('lambda' = parvals)
  } else {
    searchgrid <- expand.grid('lambda' = parvals, 
                              'N0'     = parvals * maxAsymptote)
  }
  
  # evaluate starting positions:
  MSE <- apply(searchgrid, FUN=asymptoticDecayMSE, MARGIN=c(1), schedule=schedule, signal=signal, N0=setAsymptote)
  
  if (setAsymptote) {
    df <- data.frame('lambda'=searchgrid[order(MSE)[1:gridfits],])
    lower <- c(0)
    upper <- c(1)
  } else {
    df <- data.frame(searchgrid[order(MSE)[1:gridfits],])
    lower <- c(0,0)
    upper <- c(1,maxAsymptote)
  }
  
  # testing if optimx is installed and making it available it so:
  #optimxInstalled <- require("optimx")
  
  if (useOptimx) {
    
    # run optimx on the best starting positions:
    allfits <- do.call("rbind",
                       apply( X=df,
                              MARGIN=c(1),
                              FUN=optimx::optimx,
                              fn=asymptoticDecayMSE,
                              method='L-BFGS-B',
                              lower=lower,
                              upper=upper,
                              schedule=schedule,
                              signal=signal,
                              N0=setAsymptote ) )
    
    # pick the best fit:
    win <- allfits[order(allfits$value)[1],]
    
    # return the best parameters:
    if (setAsymptote) {
      return(unlist(win[1]))
    } else {
      return(unlist(win[1:2]))
    }
    
  } else {
    
    cat('(consider installing optimx, falling back on optim now)\n')
    
    # use optim with Nelder-Mead after all:
    allfits <- do.call("rbind",
                       apply( data.frame(searchgrid[order(MSE)[1:gridfits],]),
                              MARGIN=c(1),
                              FUN=optim,
                              fn=asymptoticDecayMSE,
                              method='Nelder-Mead',
                              schedule=schedule,
                              signal=signal,
                              N0 = setAsymptote ) )
    
    # pick the best fit:
    win <- allfits[order(unlist(data.frame(allfits)[,'value']))[1],]
    
    # return the best parameters:
    return(win$par)
    
  }
  
}

# bootstrapping parameters -----

# bootstrapAsymptoticDecayModels <- function(bootstraps=1000) {
#   
#   groupsignals <- list('active'=c('localization','slowprocess'),
#                        'passive'=c('localization','slowprocess'),
#                        'nocursor'=c('nocursors','slowprocess'))
#   
#   # reversel == 16 trials
#   trialsets <- list('main'=c(1:32), 'reversal'=c(161:176))
#   
#   baselines <- list(
#     'nocursor' = list( 'nocursors'   =32, 'slowprocess'=96 ), 
#     'active'   = list( 'localization'=64, 'slowprocess'=64 ),
#     'passive'  = list( 'localization'=64, 'slowprocess'=64 )
#   )
#   
#   schedules <- list( 
#     'nocursor' = list( 'nocursors'   = -1, 'slowprocess'=  1 ), 
#     'active'   = list( 'localization'=  1, 'slowprocess'=  1 ),
#     'passive'  = list( 'localization'=  1, 'slowprocess'=  1 ) 
#   )
#   
#   participants <- sprintf('p%d',c(1:32))
#   
#   # loop through groups:
#   for (group in names(groupsignals)) {
#     
#     # do each signal for each group
#     for (signalname in groupsignals[[group]]) {
#       
#       # read in the full data set:
#       df <- read.csv(sprintf('data/%s_%s.csv',group,signalname))
#       
#       # determine length of baseline period and schedule-direction:
#       BL <- baselines[[group]][[signalname]]
#       schedulesign <- schedules[[group]][[signalname]]
#       
#       # loop through parts of the signal we want to fit:
#       for (trialset in c('main','reversal')) {
# 
#         # get the part of the data we want to fit:
#         indices <- trialsets[[trialset]] + BL
#         setdf <- df[indices,]
#         
#         # here we store all the bootstrapped parameters:
#         lambda <- c()
#         N0 <- c()
#         
#         # we need to baseline to end of main training for reversal modeling:
#         for (pp in participants) {
#           setdf[,pp] <- setdf[,pp] * schedulesign
#           if (trialset == 'reversal') { # main training is already baselined
#             if (signalname == 'slowprocess') {
#               setdf[,pp] <- setdf[,pp] - df[,pp][ min(indices) - 1 ]
#             } else {
#               a_i <- c(81:160) + BL
#               asymptote <- (mean(df[,pp][a_i], na.rm=TRUE) * schedulesign)
#               setdf[,pp] <- setdf[,pp] - asymptote
#             }
#             setdf[,pp] <- setdf[,pp] * -1
#           }
#         }
#         # baselining done
#         
#         # schedule is a vector of values -1 and length the same as the signal:
#         schedule <- rep(-1, dim(setdf)[1])
#         
#         # bootstrap parameters, by resampling participants:
#         for (bs in c(1:bootstraps)) {
#         
#           cat(sprintf('group: %s, signal: %s, set: %s, bootstrap: %d/%d\n', group, signalname, trialset, bs, bootstraps))
#           
#           signal <- apply(setdf[sample(participants, replace=TRUE)], MARGIN=1, FUN=mean, na.rm=TRUE)
#           
#           par <- asymptoticDecayFit(schedule=schedule, signal=signal)
#           
#           #plot(signal, type='l', main=par)
#           #print(par)
#           
#           lambda <- c(lambda, par['lambda'])
#           N0 <- c(N0, par['N0'])
# 
#         }
#         
#         write.csv(data.frame(lambda, N0), file=sprintf('data/%s_%s_%s.csv',group,signalname,trialset), quote=F, row.names=F)
#         
#       }
#       
#     }
#     
#   }
#   
# }

asymptoticDecaySettings <- function() {
  
  # this list determines which signals get done for each group
  groupsignals <- list(
     'passive'       = c('localization', 'slowprocess', 'reaches'),
    'terminal'        = c('localization', 'slowprocess', 'reaches'),
    'exposure'        = c('localization')
  )
  # this list determines which signals get done for each group

  

  
  # we used to run it on the reversal phase too, but it takes so much time...
  trialsets <- list('main'=c(1:160), 'reversal'=c(161:176))

  baselines <- list(
    'passive'       = list( 'localization'=64, 'slowprocess'=64, 'reaches'=64 ),
    'terminal'        = c('localization'=64,      'slowprocess'=64,    'reaches'=96),
    'exposure'        = c('localization'=64)
  )
  
  schedules <- list( 
    'passive'       = list( 'localization'=  1, 'slowprocess'=  1, 'reaches'= -1 ),
    'terminal'        = c('localization'=1,      'slowprocess'=1,    'reaches'=-1),
    'exposure'        = c('localization'=1)
  )
  
  optimxInstalled <- require("optimx")
  if (optimxInstalled) {
    useOptimx <- TRUE
  } else {
    useOptimx <- FALSE
  }

  settings <- list()
  settings[['groupsignals']] <- groupsignals
  settings[['trialsets']]    <- trialsets
  settings[['baselines']]    <- baselines
  settings[['schedules']]    <- schedules
  settings[['FUN']]          <- mean
  settings[['useOptimx']]    <- useOptimx

  return(settings)
  
}

bootstrapSemiAsymptoticDecayModels <- function(bootstraps=5) {
  
  settings <- asymptoticDecaySettings()
  
  groupsignals <- settings[['groupsignals']]
  trialsets    <- settings[['trialsets']]
  baselines    <- settings[['baselines']]
  schedules    <- settings[['schedules']]
  FUN          <- settings[['FUN']]
  useOptimx    <- settings[['useOptimx']]
  
  # loop through groups:
  for (group in names(groupsignals)) {
    
    participants <- sprintf('p%d',c(1:32))
    if (group == 'nocursor') {
      participants <- sprintf('p%d',c(1:48))
    }
    if (group == 'nocursor-in16') {
      participants <- sprintf('p%d',c(1:16))
    }
    if (group == 'nocursor-47') {
      participants <- sprintf('p%d',c(1:39,41:48))
    }
    if (group == 'nocursor-in15') {
      participants <- sprintf('p%d',c(1:7,9:16))
    }
    
    # do each signal for each group
    for (signalname in groupsignals[[group]]) {
      
      # if (signalname != 'reaches') {
      #   next()
      # }
      
      leadingzero <- FALSE
      if (signalname %in% c('localization', 'nocursors')) {
        leadingzero <- TRUE
      }
      
      #print(leadingzero)
      
      # read in the full data set:
      print(group)
      print(signalname)
      df <- read.csv(sprintf('data/%s_%s.csv',group,signalname))
      df <- df[,participants]
      
      # determine length of baseline period and schedule-direction:
      BL <- baselines[[group]][[signalname]]
      schedulesign <- schedules[[group]][[signalname]]
      
      # loop through parts of the signal we want to fit:
      for (trialset in names(trialsets)) {
        
        # get the part of the data we want to fit:
        indices <- trialsets[[trialset]] + BL
        #print(indices)
        setdf <- df[indices,]
        
        # here we store all the bootstrapped parameters:
        lambda <- c()
        N0 <- c()
        
        # we need to baseline to end of main training for reversal modeling:
        for (pp in participants) {
          setdf[,pp] <- setdf[,pp] * schedulesign
          if (trialset == 'reversal') { # main training is already baselined
            if (signalname == 'slowprocess') {
              setdf[,pp] <- setdf[,pp] - df[,pp][ min(indices) - 1 ]
            } else {
              a_i <- c(81:160) + BL
              asymptote <- (mean(df[,pp][a_i], na.rm=TRUE) * schedulesign)
              setdf[,pp] <- setdf[,pp] - asymptote
            }
            setdf[,pp] <- setdf[,pp] * -1
          }
        }
        # baselining done
        
        # schedule is a vector of values -1 and length the same as the signal:
        schedulelength <- dim(setdf)[1]
        if (leadingzero) {schedulelength <- schedulelength + 1}
        schedule <- rep(-1, schedulelength)
        #print(schedulelength)
        
        # if in reversal phase, we want to use the asymptote from the main rotation
        # which we get from the whole data
        if (trialset == 'reversal') {
          # get the part of the data we want to fit:
          Aindices <- trialsets[['main']] + BL
          #print(Aindices)
          Asetdf <- df[Aindices,]
          for (pp in participants) {
            Asetdf[,pp] <- Asetdf[,pp] * schedulesign
          }
          Aschedulelength <- dim(Asetdf)[1]
          if (leadingzero) {Aschedulelength <- Aschedulelength + 1}
          Aschedule <- rep(-1, Aschedulelength)
          Asignal <- apply(Asetdf, MARGIN=1, FUN=FUN, na.rm=TRUE)
          if (leadingzero) {Asignal <- c(0, Asignal)}
          par <- asymptoticDecayFit(schedule=Aschedule, signal=Asignal, useOptimx=useOptimx)
          
          #print(par)
          
          # twice as large! (will reduce the fitted lambda, but that makes sense...)
          setAsymptote <- par['N0'] * 2
          
        } else {
          setAsymptote <- FALSE
        }
        
        
        # bootstrap parameters, by resampling participants:
        for (bs in c(1:bootstraps)) {
          
          cat(sprintf('group: %s, signal: %s, set: %s, bootstrap: %d/%d\n', group, signalname, trialset, bs, bootstraps))
          
          signal <- apply(setdf[sample(participants, replace=TRUE)], MARGIN=1, FUN=FUN, na.rm=TRUE)
          if (leadingzero) {signal <- c(0, signal)}
          
          #print(c(length(signal), length(schedule)))
          
          par <- asymptoticDecayFit(schedule=schedule, signal=signal, setAsymptote=setAsymptote, useOptimx=useOptimx)
          
          #plot(signal, type='l', main=par)
          #print(par)
          
          lambda <- c(lambda, par['lambda'])
          if (trialset == 'main') {
            N0 <- c(N0, par['N0'])
          } else {
            N0 <- c(N0, setAsymptote)
          }
          
        }
        
        write.csv(data.frame(lambda, N0), file=sprintf('data/%s_%s_%s_semi.csv',group,signalname,trialset), quote=F, row.names=F)
        
      }
      
    }
    
  }
  
}

getAsymptoticDecayParameterCIs <- function(semi=TRUE) {
  
  settings <- asymptoticDecaySettings()
  
  groupsignals <- settings[['groupsignals']]
  trialsets    <- settings[['trialsets']]
  baselines    <- settings[['baselines']]
  schedules    <- settings[['schedules']]
  FUN          <- settings[['FUN']]
  useOptimx    <- settings[['useOptimx']]
  
  
  group <- c()
  signal <- c()
  phase <- c()
  
  lambda <- c()
  lambda_025 <- c()
  lambda_500 <- c()
  lambda_975 <- c()
  
  N0 <- c()
  N0_025 <- c()
  N0_500 <- c()
  N0_975 <- c()
  
  
  # loop through groups:
  for (groupname in names(groupsignals)) {
    
    participants <- sprintf('p%d',c(1:32))
    if (groupname == 'nocursor') {
      participants <- sprintf('p%d',c(1:48))
    }
    if (groupname == 'nocursor-in16') {
      participants <- sprintf('p%d',c(1:16))
    }
    if (groupname == 'nocursor-47') {
      participants <- sprintf('p%d',c(1:39,41:48))
    }
    if (groupname == 'nocursor-in15') {
      participants <- sprintf('p%d',c(1:7,9:16))
    }
    
    # do each signal for each group
    for (signalname in groupsignals[[groupname]]) {
      
      cat(sprintf('%s %s\n', groupname, signalname))
      
      leadingzero <- FALSE
      if (signalname %in% c('localization', 'nocursors')) {
        leadingzero <- TRUE
      }
      
      # read in the full data set:
      rawdf <- read.csv(sprintf('data/%s_%s.csv',groupname,signalname))
      rawdf <- rawdf[,participants]
      
      # determine length of baseline period and schedule-direction:
      BL <- baselines[[groupname]][[signalname]]
      schedulesign <- schedules[[groupname]][[signalname]]
      
      
      # loop through parts of the signal we want to fit:
      for (trialset in names(trialsets)) {
        
        # get the part of the data we want to fit:
        indices <- trialsets[[trialset]] + BL
        setdf <- rawdf[indices,]
        
        # we need to baseline to end of main training for reversal modeling:
        for (pp in participants) {
          setdf[,pp] <- setdf[,pp] * schedulesign
          if (trialset == 'reversal') { # main training is already baselined
            if (signalname == 'slowprocess') {
              setdf[,pp] <- setdf[,pp] - rawdf[,pp][ min(indices) - 1 ]
            } else {
              a_i <- c(81:160) + BL
              asymptote <- (mean(rawdf[,pp][a_i], na.rm=TRUE) * schedulesign)
              setdf[,pp] <- setdf[,pp] - asymptote
            }
            setdf[,pp] <- setdf[,pp] * -1
          }
        }
        # baselining done
        
        if (trialset == 'reversal' & semi) {
          # get the part of the data we want to fit:
          Aindices <- trialsets[['main']] + BL
          Asetdf <- rawdf[Aindices,]
          for (pp in participants) {
            Asetdf[,pp] <- Asetdf[,pp] * schedulesign
          }
          Aschedulelength <- dim(Asetdf)[1]
          if (leadingzero) {Aschedulelength <- Aschedulelength + 1}
          Aschedule <- rep(-1, Aschedulelength)
          #Aschedule <- rep(-1, dim(Asetdf)[1]+1)
          Asignal <- apply(Asetdf, MARGIN=1, FUN=FUN, na.rm=TRUE)
          if (leadingzero) {Asignal <- c(0, Asignal)}
          par <- asymptoticDecayFit(schedule=Aschedule, signal=Asignal, useOptimx=useOptimx)
          
          # twice as large! (will reduce the fitted lambda, but that makes sense...)
          setAsymptote <- par['N0'] * 2
          
        } else {
          setAsymptote <- FALSE
        }
        
        
        # schedule is a vector of values -1 and length the same as the signal:
        schedule <- rep(-1, dim(setdf)[1])
        if (leadingzero) {schedule <- c(0, schedule)}
        
        # this gets the overal parameters on the group median data:
        datasignal <- apply(setdf, MARGIN=1, FUN=FUN, na.rm=TRUE)
        if (leadingzero) {datasignal <- c(0, datasignal)}
        par <- asymptoticDecayFit(schedule=schedule, signal=datasignal, setAsymptote=setAsymptote, useOptimx=useOptimx)
        
        
        #print(length(apply(setdf, MARGIN=1, FUN=mean, na.rm=TRUE)))
        #plot(apply(setdf, MARGIN=1, FUN=mean, na.rm=TRUE), type='l', main=par)
        
        
        # read in the bootstrapped parameter values:
        #sstr <- '_semi' if (semi) else ''
        if (semi) {
          sstr <- '_semi'
        } else {
          sstr <- ''
        }
        df <- read.csv(sprintf('data/%s_%s_%s%s.csv', groupname, signalname, trialset, sstr), stringsAsFactors = F)
        
        group <- c(group, groupname)
        signal <- c(signal, signalname)
        phase <- c(phase, trialset)
        
        qs <- quantile(df$lambda, probs = c(0.025, 0.500, 0.975))
        
        lambda <- c(lambda, as.numeric(par['lambda']))
        lambda_025 <- c(lambda_025, qs[1])
        lambda_500 <- c(lambda_500, qs[2])
        lambda_975 <- c(lambda_975, qs[3])
        
        qs <- quantile(df$N0, probs = c(0.025, 0.500, 0.975))
        
        N0 <- c(N0, as.numeric(par['N0']))
        N0_025 <- c(N0_025, qs[1])
        N0_500 <- c(N0_500, qs[2])
        N0_975 <- c(N0_975, qs[3])
        
      }
      
    }
    
  }
  
  # write output
  write.csv(data.frame( group, signal, phase,
                        lambda, lambda_025, lambda_500, lambda_975,
                        N0, N0_025, N0_500, N0_975),
            file='data/asymptoticDecayParameterCIs.csv',
            quote = F, row.names = F)
     
}

getSaturationTrials <- function(criterion='CI') {
  
  df <- read.csv('data/asymptoticDecayParameterCIs.csv', stringsAsFactors = F)
  df <- df[which(df$phase == 'main'),]
  
  settings <- asymptoticDecaySettings()
  
  groupsignals <- settings[['groupsignals']]
  trialsets    <- settings[['trialsets']]
  baselines    <- settings[['baselines']]
  schedules    <- settings[['schedules']]
  FUN          <- settings[['FUN']]
  useOptimx    <- settings[['useOptimx']]
  
  group <- c()
  signal <- c()
  avg <- c()
  lwr <- c()
  upr <- c()
  
  # loop through groups:
  for (groupname in names(groupsignals)) {
    
    # DON'T NEED PARTICIPANTS HERE:    
    # participants <- sprintf('p%d',c(1:32))
    # if (groupname == 'nocursor') {
    #   participants <- sprintf('p%d',c(1:48))
    # }
    
    # do each signal for each group
    for (signalname in groupsignals[[groupname]]) {
      
      # read in the full data set:
      #rawdf <- read.csv(sprintf('data/%s_%s.csv',groupname,signalname))
      
      leadingzero <- FALSE
      if (signalname %in% c('localization', 'nocursors')) {
        leadingzero <- TRUE
      }
      
      
      # determine length of baseline period and schedule-direction:
      BL <- baselines[[groupname]][[signalname]]
      schedulesign <- schedules[[groupname]][[signalname]]
      
      
      # loop through parts of the signal we want to fit:
      #for (trialset in c('main','reversal')) {
      for (trialset in c('main')) {
        
        # get the part of the data we want to fit:
        indices <- trialsets[[trialset]] + BL
        #setdf <- rawdf[indices,]
        
        # schedule is a vector of values -1 and length the same as the signal:
        #schedulelength <- dim(setdf)[1]
        schedulelength <- length(indices)
        if (leadingzero) {schedulelength <- schedulelength + 1}
        schedule <- rep(-1, schedulelength)
        
        # this gets the overal parameters on the group median data:
        #par <- asymptoticDecayFit(schedule=schedule, signal=apply(setdf, MARGIN=1, FUN=mean, na.rm=TRUE), setAsymptote=setAsymptote)
        
        trialnos <- list()
        
        for (roc in c('lambda','lambda_975','lambda_025')) {
          
          par <- c('lambda'=df[which(df$group == groupname & df$signal == signalname),roc], 'N0'=df[which(df$group == groupname & df$signal == signalname),'N0'])
          
          fitdf <- asymptoticDecayModel(par=par, schedule=schedule)
          
          if (is.numeric( criterion )) {
            crit <- (par['N0'] * criterion)
          }
          if ( criterion == 'CI' ) {
            crit <- df$N0_025[which(df$group == groupname & df$signal == signalname)]
          }
          
          trialno <- which(fitdf$output > crit)[1]
          
          # subtract 1 from trial no, as first trial depends on feedback from previous phase
          trialno <- trialno
          
          trialnos[[roc]] <- trialno
          
        }
        #print(trialnos[['lambda']])
        
        group <- c(group, groupname)
        signal <- c(signal, signalname)
        avg <- c(avg, trialnos[['lambda']])
        lwr <- c(lwr, trialnos[['lambda_975']])
        upr <- c(upr, trialnos[['lambda_025']])
        
        cat(sprintf('%s, %s: trial %d (%d - %d)\n', groupname, signalname, trialnos[['lambda']], trialnos[['lambda_975']], trialnos[['lambda_025']]))

      }
      
    }
    
  }
  
  df <- data.frame(group, signal, avg, lwr, upr)
  
  write.csv(df, 'data/saturation_trials.csv', row.names = FALSE, quote = FALSE)
  
}

getStyles <- function() {
  
  styles <- list()
  
  ## Active
  
  styles[['active']] <- list(
    'solid'=rgb(1.0, 0.4, 0.0),         # orange
    'trans'=rgb(1.0, 0.4, 0.0, 0.1),    # transparent orange
    'label'='active localization'
  )
  
  ## Passive
  
  styles[['passive']] <- list(
    'solid'=rgb(0.7, 0.0, 0.7),          # purple
    'trans'=rgb(0.7, 0.0, 0.7, 0.2),     # transparent purple
    'label'='passive localization'
  )

  ## Pause
  
  styles[['pause']] <- list(
    'solid'=rgb(0.1, 0.3, 0.5),         # Blue
    'trans'=rgb(0.1, 0.3, 0.5, 0.1),     # transparent Blue
    'label'='pause'
  )

  ## No-Cursor
  
  styles[['nocursor-47']] <- list(
    'solid'=rgb(0.0, 0.7, 0.0),         # green
    'trans'=rgb(0.0, 0.7, 0.0, 0.2),     # transparent green
    'label'='no-cursor'
  )
  
  ## Reaches
  
  styles[['reaches']] <- list(
    'solid'=rgb(0,0,0),                 # black
    'trans'=rgb(0,0,0,0.2),             # gray
    'label'='reaches'
  )
  
  ## Terminal
  
  styles[['terminal']] <- list(
    'solid'=rgb(1, 0.0, 0.0),         # Red
    'trans'=rgb(1, 0.0, 0., 0.1),     # transparent Red
    'label'='no-cursor'
  )
  
  ## Exposure
  
  styles[['exposure']] <- list(
    'solid'=rgb(0.85, 0.65, 0.12),         # Red
    'trans'=rgb(0.85, 0.65, 0.12, 0.2),     # transparent Red
    'label'='no-cursor'
  )
  

  ## Slow Process
  
  styles[['slowprocess']] <- list(
    'solid'=rgb(0.63, 0.71, 0.81),      # blue-gray
    'trans'=rgb(0.63, 0.71, 0.81, 0.2),  # transparent blue-gray
    'label'='slow process'
    
  )
  
  
  return(styles)

}


plotSaturation <- function(xscale='normal', target='tiff') {
  
  
  fonts <- list(sans = "Arial", mono = "Arial")
  if (target == 'svg') {
    library('svglite')
    svglite::svglite(file='figs/AbstractFig.svg', width=8, height=6, bg='white', system_fonts=fonts)
    
  }
  if (target == 'pdf') {
    pdf(file='figs/AbstractFig.pdf', width=8, height=6, bg='white')
    
  }
  if (target == 'eps') {
    postscript(file='figs/AbstractFig.eps', bg='white', width=8, height=6, paper='special', horizontal=FALSE)
    
  }
  
  if (target == 'tiff') {
    tiff(filename='figs/AbstractFig.tiff', res=600, width=6, height=4.5, units='in', compression='lzw')
    
  } 
  
  
  df <- read.csv('data/asymptoticDecayParameterCIs.csv', stringsAsFactors = F)
  df <- df[which(df$phase == 'main'),]
  
  settings <- asymptoticDecaySettings()
  
  groupsignals <- list(
    'passive'       = c('reaches','localization', 'slowprocess'),
    'terminal'   = c('localization'),
    'exposure'         = c('localization')
  )
  
  trialsets    <- settings[['trialsets']]
  baselines    <- settings[['baselines']]
  schedules    <- settings[['schedules']]
  FUN          <- settings[['FUN']]
  useOptimx    <- settings[['useOptimx']]
  
  
  styles <- getStyles()
  
  
  if (xscale == 'logarithmic') {
    
    plot(-1000,-1000,
         xlab='trials completed in rotated phase',ylab='percentage of asymptote',
         main='modeled process speeds',
         xlim=c(1,81),ylim=c(0,1.1),
         bty='n',ax=F,log='x')
    TIME <- c(seq(1,11,0.05),seq(12,161))
    xcoords <- TIME + 1
    
  }
  
  if (xscale == 'normal') {
  
    plot(-1000,-1000,
         xlab='trials completed in rotated phase',ylab='percentage of saturation',
         main='modeled process speeds',
         xlim=c(0,20),ylim=c(0,1.1),
         bty='n',ax=F)
    TIME <- seq(0,160,.1)  
    xcoords <- TIME
    
  }
  
  groupcolors <- c(styles$passive$solid,
                   styles$terminal$solid,
                   styles$exposure$solid,
                   "black",
                   styles$slowprocess$solid)
  
  # loop through groups:
  for (groupname in names(groupsignals)) {
    
    # DON'T NEED PARTICIPANTS HERE:    
    # participants <- sprintf('p%d',c(1:32))
    # if (groupname == 'nocursor') {
    #   participants <- sprintf('p%d',c(1:48))
    # }
    
    # do each signal for each group
    for (signalname in groupsignals[[groupname]]) {
      
      leadingzero <- FALSE
      if (signalname %in% c('localization', 'nocursors')) {
        leadingzero <- TRUE
      }
      
      # determine length of baseline period and schedule-direction:
      BL <- baselines[[groupname]][[signalname]]
      schedulesign <- schedules[[groupname]][[signalname]]
      
      # loop through parts of the signal we want to fit:
      for (trialset in c('main')) {
        
        # get the part of the data we want to fit:
        indices <- trialsets[[trialset]] + BL
        #setdf <- rawdf[indices,]
        
        # schedule is a vector of values -1 and length the same as the signal:
        schedulelength <- length(indices)
        if (leadingzero) {schedulelength <- schedulelength + 1}
        schedule <- rep(-1, schedulelength)
        # this gets the overal parameters on the group median data:
        
        processes <- list()
        
        #print(c(groupname,signalname))
        
        for (roc in c('lambda','lambda_975','lambda_025')) {
          
          par <- c('lambda'=df[which(df$group == groupname & df$signal == signalname),roc], 'N0'=df[which(df$group == groupname & df$signal == signalname),'N0'])
          par['scale'] <- df$N0_025[which(df$group == groupname & df$signal == signalname)]
          #print(par)
          
          dfit <- asymptoticDecayModel(par,schedule)$output
          smspl <- smooth.spline(x=c(0:(length(schedule)-1)), y=dfit, spar=NULL)
          process <- predict(smspl,TIME)$y
          process <- (process) / (par['scale'])
          processes[[roc]] <- process

        }
        
        upr <- processes[['lambda_975']]
        lwr <- processes[['lambda_025']]
        up_idx <- which(upr >= 1)[1]
        lo_idx <- which(lwr >= 1)[1]
        
        #print(c(up_idx,lo_idx))
        
        X <- c(xcoords[1:up_idx],rev(xcoords[1:lo_idx]))
        Y <- c(upr[1:up_idx],rev(lwr[1:lo_idx]))
        
        # groupsignals <- list(
        #   'active'        = c('localization'),
        #   'passive'       = c('localization', 'slowprocess'),
        #   'nocursor-47'   = c('nocursors'    ),
        #   'pause'         = c('reaches'     )
        # )
        
        solid <- NA
        trans <- NA
        
        if (signalname %in% names(styles)) {
          solid <- styles[[signalname]]$solid
          trans <- styles[[signalname]]$trans
        } else {
          if (groupname %in% names(styles)) {
            solid <- styles[[groupname]]$solid
            trans <- styles[[groupname]]$trans
          }
        }
        
        #print(c(solid,trans))
        
        polygon(X,Y,col=trans,border=NA)
        
        avg <- processes[['lambda']]
        av_idx <- which(avg >= 1)[1]
        lines(xcoords[1:av_idx],avg[1:av_idx],col=solid)
        
      }
      
    }
    
  }
  
  
  if (xscale == 'logarithmic') {
    
    polygon(c(1,81,81,1),c(1,1,2,2),col='white',border=NA)
    
    lines(c(1,80),c(1,1),col='black',lty=1,lw=2)
    text(60,1.05,'asymptote lower bound')
    
    legend(22,.4,legend=c('passive localization', 'reach training', 'slow process'),col=groupcolors,lty=c(1,1,1,1,1),bty='n')
    
    axis(side=1, at=c(1,2,3,4,5,6,11,21,41,81), labels=sprintf('%d',c(0,1,2,3,4,5,10,20,40,80)))
    axis(side=2, at=seq(0,1,0.2), labels=sprintf('%d',round(seq(0,1,0.2)*100)))
    
  }
  
  if (xscale == 'normal') {
    
    polygon(c(0,20,20,0),c(1,1,2,2),col='white',border=NA)
    
    lines(c(0,20),c(1,1),col='black',lty=1,lw=2)
    text(20,1.05,'asymptote lower bound',adj=c(1,0.5))
    
    legend(11,1.03,legend=c('Continuous-Localization',  'Terminal-Localization', 'Exposure-Localization', 'Reaches','Slowprocess'),col=groupcolors,lty=c(1,1,1,1,1),bty='n', cex = .85)
    
    axis(side=1, at=c(0,5,10,15,20), labels=c('baseline',sprintf('%d',c(5,10,15,20))))
    axis(side=2, at=seq(0,1,0.2), labels=sprintf('%d',round(seq(0,1,0.2)*100)),las = 2)
    
  }
  
  if (target %in% c('svg','pdf','eps','tiff')) {
    dev.off()
  }
  
}
