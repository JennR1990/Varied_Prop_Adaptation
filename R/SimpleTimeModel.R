SimpleTimeModel <- function(par, schedule) {
  rotation<- schedule
  rotation[is.na(rotation)]<- 0
  
  # thse values should be zero at the start of the loop:
  Et <- 0 # previous error: none
  Pt <- 0 # state of slow process: aligned
  AP <- 0 # attenuation parameter: Starts at zero and increases with number of rotation changes
  
  # we'll store what happens on each trial in these vectors:
  process <- c()
  
  # now we loop through the perturbations in the schedule:
  for (t in c(1:length(schedule))) {
    
    
    if (t > 49){
      AP<- AP + 0.08333333
      
    } else {
      AP <- AP
    }
    
    # first we calculate what the model does
    # this happens before we get visual feedback about potential errors
    # on the first trial, since we set Et and Pt at zero, overall Pt will be zero
    # I want to add an additional term that basically scales the AP parameter so there is less learning overall as time goes by. 
    Pt <- (par['R'] * Pt) - (par['L'] * Et) - (par['A'] * AP)
    #Pt <- (.857 * Pt) - (.232 * Et) - (.1 * AP)
    
    # now we calculate what the previous error will be for the next trial:
    if (is.na(schedule[t])) {
      Et <- 0
    } else {
      Et <- Pt + schedule[t]
    }
    
    # at this point we save the states in our vectors:
    process <- c(process, Pt)
    
  }
  
  # after we loop through all trials, we return the model output:
  return(data.frame(process))
  
}


SimpleTimeMSE <- function(par, schedule, reaches) {
  
  bigError <- mean(schedule^2, na.rm=TRUE) * 10
  
  return( mean((SimpleTimeModel(par, schedule)$process - reaches)^2, na.rm=TRUE) )
  
}

SimpleTimeFit <- function(schedule, reaches, gridpoints=6, gridfits=6) {
  
  parvals <- seq(1/gridpoints/2,1-(1/gridpoints/2),1/gridpoints)
  
  searchgrid <- expand.grid('L'=parvals,
                            'R'=parvals,
                            'A'=parvals)
  # evaluate starting positions:
  MSE <- apply(searchgrid, FUN=SimpleTimeMSE, MARGIN=c(1), schedule=schedule, reaches=reaches)
  
  # optimxInstalled <- require("optimx")
  # 
  # if (optimxInstalled) {
  #   
  #   # run optimx on the best starting positions:
  #   allfits <- do.call("rbind",
  #                      apply( searchgrid[order(MSE)[1:gridfits],],
  #                             MARGIN=c(1),
  #                             FUN=optimx::optimx,
  #                             fn=threeRateMSE,
  #                             method='L-BFGS-B',
  #                             lower=c(0,0),
  #                             upper=c(.99,.99),
  #                             schedule=schedule,
  #                             reaches=reaches ) )
  #   
  #   # pick the best fit:
  #   win <- allfits[order(allfits$value)[1],]
  #   
  #   # return the best parameters:
  #   return(unlist(win[1:2]))
  #   
  # } else {
  
  cat('(consider installing optimx, falling back on optim now)\n')
  
  # use optim with Nelder-Mead after all:
  allfits <- do.call("rbind",
                     apply( searchgrid[order(MSE)[1:gridfits],],
                            MARGIN=c(1),
                            FUN=stats::optim,
                            fn=SimpleTimeMSE,
                            method='Nelder-Mead',
                            schedule=schedule,
                            reaches=reaches ) )
  
  # pick the best fit:
  win <- allfits[order(unlist(data.frame(allfits)[,'value']))[1],]
  
  # return the best parameters:
  return(win$par)
  
  #}
  
}

































