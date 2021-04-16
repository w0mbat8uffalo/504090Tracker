FFNTracker <- function(player, x = seq(from=200, to=5, by=-5)){
  devtools::install_github("abresler/nbastatR")
  library(nbastatR)
  setwd("~/Downloads")
  AllPlayerTotals <- data.frame(bref_players_stats(seasons=2021, only_totals = T))
  PlayerTotals <- AllPlayerTotals[AllPlayerTotals$namePlayer == player,]
  
  ###Defines Table of Makes Needed for 50% target FG%
  FGAs <- c()
  MakesToTarget <- c()
  for (number in x){
    a <- PlayerTotals$fgmTotals
    b <- PlayerTotals$fgaTotals
    y <- (((b+number)/2)-a)/number
    y <- ceiling(y*number)
    if (number >= y & y >= 0){
      FGAs <- c(number, FGAs)
      MakesToTarget <- c(y, MakesToTarget)
    } else {
      FGAs <- c(number, FGAs)
      MakesToTarget <- c(NA, MakesToTarget)
    }
    FiftyTarget <- data.frame(FGAs, MakesToTarget)
  }
  
  ###Defines Table of Makes Needed for 40% target 3P%
  ThreeAttempts <- c()
  MakesToTarget <- c()
  for (number in x){
    a <- PlayerTotals$fg3mTotals
    b <- PlayerTotals$fg3aTotals
    y <- ((2*(b+number)/5)-a)/number
    y <- ceiling(y*number)
    if (number >= y & y >= 0){
      ThreeAttempts <- c(number, ThreeAttempts)
      MakesToTarget <- c(y, MakesToTarget)
    } else {
      ThreeAttempts <- c(number, ThreeAttempts)
      MakesToTarget <- c(NA, MakesToTarget)
    }
    FortyTarget <- data.frame(ThreeAttempts, MakesToTarget)
  }
  
  ###Defines Table of Makes Needed for 90% target FT%
  FTAs <- c()
  MakesToTarget <- c()
  for (number in x){
    a <- PlayerTotals$ftmTotals
    b <- PlayerTotals$ftaTotals
    y <- ((9*(b+number)/10)-a)/number
    y <- ceiling(y*number)
    if (number >= y & y >= 0){
      FTAs <- c(number, FTAs)
      MakesToTarget <- c(y, MakesToTarget)
    } else {
      FTAs <- c(number, FTAs)
      MakesToTarget <- c(NA, MakesToTarget)
    }
    NinetyTarget <- data.frame(FTAs, MakesToTarget)
  }
  AllTargets <- cbind(FiftyTarget, FortyTarget, NinetyTarget)
  AllTargets
}