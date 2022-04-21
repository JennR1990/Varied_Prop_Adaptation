
phase0<- c(3,6,10,20)
remove<- c(phase0, (phase0+24))
df<- read.csv("ana/asymptoticDecayParameterCIs.csv", header = TRUE)


newdf<- df[-remove,]


phases<- as.character(c(1,2,4,5,7,8,9,11,12,13:19,21:24))

trialsets <- list('1'=c(50:61),      "2" = c(62:73),      "3" = c(74:85),   "4" = c(86:97), 
                  "5" = c(98:109),   "6" = c(110:133),    "7" = c(134:157), "8" = c(158:181),
                  "9" = c(194:205), "10" = c(206:217),   "11" = c(218:229), "12" = c(254:265),
                  "13" = c(266:289),   "14" = c(290:313),   "15" = c(314:325),   "16" = c(326:337),
                  "17" = c(350:361),   "18" = c(362:373),   "19" = c(374:385),   "20" = c(386:397),
                  "21" = c(410:421),   "22" = c(422:433),   "23" = c(434:445),   "24" = c(446:457))



rot<- c()
for (phase in phases) rot<-c(rot,as.numeric(unique(variation_reaches[as.numeric(unlist(trialsets[phase])),1])))

rotation<- c(rot,rot)
newdf<- cbind(newdf,rotation)

newN0<- (newdf$N0/abs(newdf$rotation))*100
newN_97<- (newdf$N0_975/abs(newdf$rotation))*100
newN_25<- (newdf$N0_025/abs(newdf$rotation))*100

plot(newN0[1:20], type = "l", ylim = c(0,150), col = "Red", main = "Localization Asymptotes Across Blocks", ylab = "Percent Compensation", xlab = "Block (12 or 24 trials)")

locCI<-c(newN_25[1:20],rev(newN_97[1:20]))
x<- c(1:20,20:1)
polygon(x, locCI, col = rgb(1,0,0,.2), border = NA)


plot(newN0[21:40], type = "l", ylim = c(0,200), col = "Blue", main = "Reach Asymptotes Across Blocks", ylab = "Percent Compensation", xlab = "Block (12 or 24 trials)")
reachCI<-c(newN_25[21:40],rev(newN_97[21:40]))
polygon(x, reachCI, col = rgb(0,0,1,.2), border = NA)




plot(newN0[1:20], type = "l", ylim = c(0,150), col = "Red", main = "Localization Asymptotes Across Blocks", ylab = "Percent Compensation", xlab = "Block (12 or 24 trials)")

locCI<-c(newN_25[1:20],rev(newN_97[1:20]))
x<- c(1:20,20:1)
polygon(x, locCI, col = rgb(1,0,0,.2), border = NA)


plot(newN0[21:40], type = "l", ylim = c(0,200), col = "Blue", main = "Reach Asymptotes Across Blocks", ylab = "Percent Compensation", xlab = "Block (12 or 24 trials)")
reachCI<-c(newN_25[21:40],rev(newN_97[21:40]))
polygon(x, reachCI, col = rgb(0,0,1,.2), border = NA)
