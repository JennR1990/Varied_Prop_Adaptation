BaselineTaps <- function() {
  movement <- read.csv('data/Localizations.csv', header = TRUE)
  angle <- read.csv("data/Tap_Angles.csv", header = TRUE)
  
  
  angles <-  c(55, 65, 75, 85, 95, 105, 115, 125)
  
  
  TargetAverages <- data.frame(matrix(NA, nrow = 32, ncol = 9))
  names(TargetAverages) <-
    c("55", "65", "75", "85", "95", "105", "115", "125", "Participant")
  TargetAverages$Participant <- c(1:32)
  
  for (p in 2:33) {
    for (i in 1:8) {
      TargetAverages[p - 1, i] <-
        mean(movement[1:49, p][angle[1:49, p] == angles[i]], na.rm = TRUE)
      
    }
    
    
  }
  
  
  
  for (c in 2:ncol(angle)) {
    p = 1
    for (r in 1:nrow(angle)) {
      k <- as.character(angle[r, c])
      targetcol <- as.numeric(which(colnames(TargetAverages) == k))
      movement[r, c] <- movement[r, c] - TargetAverages[p, targetcol]
    }
    p = p + 1
  }
  
  
  write.csv(movement,
            file = "data/Localizations_Baselined.csv",
            quote = F,
            row.names = FALSE)
 # return(movement)
  
  
}





BaselineReaches <- function() {
  movement <- read.csv('data/Reaches.csv', header = TRUE)
  angle <- read.csv("data/Reach_Angles.csv", header = TRUE)
  
  
  angles <-  c(60, 80, 100, 120)
  
  
  TargetAverages <- data.frame(matrix(NA, nrow = 32, ncol = 5))
  names(TargetAverages) <- c("60", "80", "100", "120", "Participant")
  TargetAverages$Participant <- c(1:32)
  
  for (p in 2:33) {
    for (i in 1:4) {
      TargetAverages[p - 1, i] <-
        mean(movement[1:49, p][angle[1:49, p] == angles[i]], na.rm = TRUE)
      
    }
    
    
  }
  
  
  for (c in 2:ncol(angle)) {
    p = 1
    for (r in 1:nrow(angle)) {
      k <- as.character(angle[r, c])
      targetcol <- as.numeric(which(colnames(TargetAverages) == k))
      movement[r, c] <- movement[r, c] - TargetAverages[p, targetcol]
    }
    p = p + 1
  }
  
  write.csv(movement,
            file = "data/Reaches_Baselined.csv",
            quote = F,
            row.names = FALSE)
  #return(movement)
  
  
}