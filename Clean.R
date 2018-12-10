# Data Science for Economists
# Analysis of NFL Wins
# By Alli Penner

library(moments)
library(tidyverse)

# read data
pbp2009 <- read.csv("~/Master's Thesis/nflscrapR-data/data/season_play_by_play/pbp_2009.csv")
pbp2010 <- read.csv("~/Master's Thesis/nflscrapR-data/data/season_play_by_play/pbp_2010.csv")
pbp2011 <- read.csv("~/Master's Thesis/nflscrapR-data/data/season_play_by_play/pbp_2011.csv")
pbp2012 <- read.csv("~/Master's Thesis/nflscrapR-data/data/season_play_by_play/pbp_2012.csv")
pbp2013 <- read.csv("~/Master's Thesis/nflscrapR-data/data/season_play_by_play/pbp_2013.csv")
pbp2014 <- read.csv("~/Master's Thesis/nflscrapR-data/data/season_play_by_play/pbp_2014.csv")
pbp2015 <- read.csv("~/Master's Thesis/nflscrapR-data/data/season_play_by_play/pbp_2015.csv")
pbp2016 <- read.csv("~/Master's Thesis/nflscrapR-data/data/season_play_by_play/pbp_2016.csv")
pbp2017 <- read.csv("~/Master's Thesis/nflscrapR-data/data/season_play_by_play/pbp_2017.csv")
pbpfull <- rbind(pbp2009, pbp2010, pbp2011, pbp2012, pbp2013, pbp2014, pbp2015, pbp2016, pbp2017)

yardweights <- read.csv("~/Master's Thesis/EqPoints.csv")
# mod yardweights to include safeties
yardweights <- rbind(yardweights, c(100, -2))


# Generate features
# overall yard moments (4)
# trad stats: total yards, total plays, comp percent, YPP, TOP (7)
# neo stats: 5 factors, whatever pff does (5)
# generate percent of yards to goal line gained instead

# aggregate on GameID to final data frame
# names of columns for final DF 
# All stats are twiined for home and away team
names <- c("GameID", "HomeTeam", "YPPHome", "PlayvarHome", "playskewHome", "playkurtHome", 
           "totalyardsHome", "PlaysHome", "Homecompercent",  "HomeTOP", "HomeExplode", 
           "HomeSuccess", "HomeFieldpos", "HomePointsPerTrip", "HomeTurnover", "HomePenalty", "AwayTeam", "YPPAway", 
           "PlayvarAway", "playskewAway", "playkurtAway", "totalyardsAway", "PlaysAway", 
           "Awaycompercent" ,"AwayTOP", "AwayExplode", 
           "AwaySuccess", "AwayFieldpos", "AwayPointsPerTrip", "AwayTurnover", "AwayPenalty", "win")

# vector of game Ids
gameids <- unique(pbpfull$GameID)

# generate empty frame to populate
Finaldf <- as.data.frame(matrix(NA, nrow = length(gameids), ncol = length(names)))
colnames(Finaldf) <- names

# populate final data frame
for (i in 1:length(gameids)){
  id <- gameids[i]
  
  # subset to one game
  game <- pbpfull[pbpfull$GameID==id,]
  
  # record game ID for row I
  Finaldf$GameID[i] <- id
  
  # teams
  Finaldf$HomeTeam[i] <- game$HomeTeam[1]
  Finaldf$AwayTeam[i] <- game$AwayTeam[1]
  
  # generate subsets by who's on offense
  home <- game[ game$DefensiveTeam == game$AwayTeam[1],]
  away <- game[ game$DefensiveTeam == game$HomeTeam[1],]
  home <- home[!is.na(home$Drive),]
  away <- away[!is.na(away$Drive),]
  
  # plays from scrimmage are run, pass, sack, QB Kneel, and spike
  # yards per play from scrimmage
  # simple mean 
  Finaldf$YPPHome[i] <- mean(home$Yards.Gained[home$PlayType %in% c("Pass", "Run", "Sack", "QB Kneel", "Spike")])
  Finaldf$YPPAway[i] <- mean(away$Yards.Gained[away$PlayType %in% c("Pass", "Run", "Sack", "QB Kneel", "Spike")])
  
  # Variance of yards gained from scrimmage
  # moments::var
  Finaldf$PlayvarHome[i] <- var(home$Yards.Gained[home$PlayType %in% c("Pass", "Run", "Sack", "QB Kneel", "Spike")])
  Finaldf$PlayvarAway[i] <- var(away$Yards.Gained[away$PlayType %in% c("Pass", "Run", "Sack", "QB Kneel", "Spike")])
  
  # skewness of yards gained from scrimmage
  #moments::skew
  Finaldf$playskewHome[i] <- skewness(home$Yards.Gained[home$PlayType %in% c("Pass", "Run", "Sack", "QB Kneel", "Spike")])
  Finaldf$playskewAway[i] <- skewness(away$Yards.Gained[away$PlayType %in% c("Pass", "Run", "Sack", "QB Kneel", "Spike")])
  
  # Kurtosis of yards gained from scrimmage
  Finaldf$playkurtHome[i] <- var(home$Yards.Gained[home$PlayType %in% c("Pass", "Run", "Sack", "QB Kneel", "Spike")])
  Finaldf$playkurtAway[i] <- var(away$Yards.Gained[away$PlayType %in% c("Pass", "Run", "Sack", "QB Kneel", "Spike")])
  
  # total yards from scrimmage
  # sum
  Finaldf$totalyardsHome[i] <- sum(home$Yards.Gained[home$PlayType %in% c("Pass", "Run", "Sack", "QB Kneel", "Spike")])
  Finaldf$totalyardsAway[i] <- sum(away$Yards.Gained[away$PlayType %in% c("Pass", "Run", "Sack", "QB Kneel", "Spike")])
  
  # Total Plays from scrimmage
  Finaldf$PlaysHome[i] <- length(home$Yards.Gained[home$PlayType %in% c("Pass", "Run", "Sack", "QB Kneel", "Spike")])
  Finaldf$PlaysAway[i] <- length(away$Yards.Gained[away$PlayType %in% c("Pass", "Run", "Sack", "QB Kneel", "Spike")])
  
  # completion percentage
  #number of complete passes/number of total passes
  Finaldf$Homecompercent[i] <- nrow(home[home$PassOutcome=="Complete",])/(nrow(home[home$PassOutcome=="Incomplete Pass",])+nrow(home[home$PassOutcome=="Complete",]))
  Finaldf$Awaycompercent[i] <- nrow(away[away$PassOutcome=="Complete",])/(nrow(away[away$PassOutcome=="Incomplete Pass",])+nrow(away[away$PassOutcome=="Complete",]))
  
  # Time of possession
  # vector with one value per drive
  timeperdrive <- rep.int(0, length(unique(home$Drive))) 
  for (j in unique(home$Drive)){
    # pull drive j 
    d <- home[home$Drive==j,]
    # remove NA
    d <- d[!is.na(d$TimeSecs),]
    # difference between first and last timestamp
    timeperdrive[j] <- max(d$TimeSecs)-min(d$TimeSecs) #time 
    # sum time used and record
    t_home <- sum(timeperdrive, na.rm = T)
  }
  
  j <- 1
  timeperdrive <- rep.int(0, length(unique(away$Drive)))
  for (j in unique(away$Drive)){
    d <- away[away$Drive==j,]
    d <- d[!is.na(d$TimeSecs),]
    timeperdrive[j] <- max(d$TimeSecs)-min(d$TimeSecs) #time 
    t_away <- sum(timeperdrive, na.rm = T)
  }
  
  Finaldf$HomeTOP[i] <- t_home 
  Finaldf$AwayTOP[i] <- t_away
  
  
  # explosiveness
  homescrim <- home[home$PlayType %in% c("Pass", "Run", "Sack", "QB Kneel", "Spike"),]
  awayscrim <- away[away$PlayType %in% c("Pass", "Run", "Sack", "QB Kneel", "Spike"),]
  
  # exclude blocked field goals
  homescrim <- homescrim[ which(is.na(homescrim$FieldGoalResult)),]
  awayscrim <- awayscrim[ which(is.na(awayscrim$FieldGoalResult)),]
  
  # exclude 2pt try
  homescrim <- homescrim[ which(is.na(homescrim$TwoPointConv)),]
  awayscrim <- awayscrim[ which(is.na(awayscrim$TwoPointConv)),]
  
  EP <- c()
  for (e in 1:nrow(homescrim)){
    if (homescrim$InterceptionThrown[e]==0 & 
        is.na(homescrim$RecFumbPlayer[e])){
      playstart <- homescrim$yrdline100[e]
      playend <- homescrim$yrdline100[e] - homescrim$Yards.Gained[e]
      startEP <- yardweights$EqPt.Value[ which(yardweights$YdfromEZ==playstart)]
      if (playend>100){
        endEP <- -2
      }
      else if (playend<0){
        endEP <- 6.96
      }
      else{
        endEP <- yardweights$EqPt.Value[ which(yardweights$YdfromEZ==playend)]
      }
      EP[e] <- endEP-startEP
    }
    else{
      EP[e] <- NA
    }
  }
  Finaldf$HomeExplode[i] <- mean(EP, na.rm = T) 
  
  EP <- c()
  for (e in 1:nrow(awayscrim)){
    if (awayscrim$InterceptionThrown[e]==0 & 
        is.na(awayscrim$RecFumbPlayer[e])){
      playstart <- awayscrim$yrdline100[e]
      playend <- awayscrim$yrdline100[e] - awayscrim$Yards.Gained[e]
      startEP <- yardweights$EqPt.Value[ which(yardweights$YdfromEZ==playstart)]
      if (playend>100){
        endEP <- -2
      }
      else if (playend<0){
        endEP <- 6.96
      }
      else{
        endEP <- yardweights$EqPt.Value[ which(yardweights$YdfromEZ==playend)]
      }      
      EP[e] <- endEP-startEP
    }
    else{
      EP[e] <- NA
    }
  }
  Finaldf$AwayExplode[i] <- mean(EP, na.rm = T) 
  
  
  # success
  # 50% of yards to go on 1st down, 70 on 2nd down, 100 on 3rd plus
  homescrim$pctgain <- homescrim$Yards.Gained/homescrim$ydstogo
  homescrim$success <- 0
  homescrim$success[is.na(homescrim$down)] <- NA
  homescrim$success[which(homescrim$down==1 & homescrim$pctgain>=.5)] <- 1
  homescrim$success[which(homescrim$down==2 & homescrim$pctgain>=.7)] <- 1
  homescrim$success[which(homescrim$down==3 & homescrim$pctgain>=1)] <- 1
  homescrim$success[which(homescrim$down==4 & homescrim$pctgain>=1)] <- 1
  homesucc <- mean(homescrim$success, na.rm=T)
  Finaldf$HomeSuccess[i] <- homesucc
  
  awayscrim$pctgain <- awayscrim$Yards.Gained/awayscrim$ydstogo
  awayscrim$success <- 0
  awayscrim$success[is.na(awayscrim$down)] <- NA
  awayscrim$success[which(awayscrim$down==1 & awayscrim$pctgain>=.5)] <- 1
  awayscrim$success[which(awayscrim$down==2 & awayscrim$pctgain>=.7)] <- 1
  awayscrim$success[which(awayscrim$down==3 & awayscrim$pctgain>=1)] <- 1
  awayscrim$success[which(awayscrim$down==4 & awayscrim$pctgain>=1)] <- 1
  awaysucc <- mean(awayscrim$success, na.rm=T)
  Finaldf$AwaySuccess[i] <- awaysucc
  
  # field pos
  fp <- rep.int(0, length(unique(home$Drive)))
  p <- 1
  for (k in unique(home$Drive)){
   d <- home[home$Drive==k,]
   if (d$PlayType[1]=="Kickoff"){
     fp[p] <- d$yrdline100[2]
   }
   else {
     fp[p] <- d$yrdline100[1]
   }
   p <- p+1
  }
  Finaldf$HomeFieldpos[i] <- mean(fp, na.rm = T)
  
  fp <- rep.int(0, length(unique(away$Drive)))
  p <- 1
  for (k in unique(away$Drive)){
    d <- away[away$Drive==k,]
    if (d$PlayType[1]=="Kickoff"){
      fp[p] <- d$yrdline100[2]
    }
    else {
      fp[p] <- d$yrdline100[1]
    }
    p <- p+1
  }
  Finaldf$AwayFieldpos[i] <- mean(fp, na.rm = T)
  
  # Points per trip
  ppt <- c(0)
  p <- 1
  for (k in unique(home$Drive)){
    d <- home[home$Drive==k,] 
    # call out kickoff
    d <- d[d$PlayType != "kickoff",]
    d$yrdline100[is.na(d$yrdline100)] <- 50
    if (any(d$yrdline100<=40)){
      fg <- d$FieldGoalResult
      fg[is.na(fg)] <- "No Good"
      
      ppt[p] <- sum(d$Touchdown)*7 + as.integer(any(fg=="Good"))*3
      p <- p+1
    }
  }
  Finaldf$HomePointsPerTrip[i] <- mean(ppt, na.rm = T)
  
  ppt <- c(0)
  p <- 1
  for (k in unique(away$Drive)){
    d <- away[away$Drive==k,] 
    # call out kickoff
    d <- d[d$PlayType != "kickoff",]
    d$yrdline100[is.na(d$yrdline100)] <- 50
    if (any(d$yrdline100<=40)){
      fg <- d$FieldGoalResult
      fg[is.na(fg)] <- "No Good"
      
      ppt[p] <- sum(d$Touchdown)*7 + as.integer(any(fg=="Good"))*3
      p <- p+1
    }
  }
  Finaldf$AwayPointsPerTrip[i] <- mean(ppt, na.rm = T)
  
  # turnovers
  Finaldf$HomeTurnover[i] <- sum(home$InterceptionThrown) + length(nrow(home[!is.na(home$RecFumbPlayer),]))
  Finaldf$AwayTurnover[i] <- sum(away$InterceptionThrown) + length(nrow(away[!is.na(away$RecFumbPlayer),]))
  
  # penalties
  penalties <- game[!is.na(game$PenalizedTeam),]
  Finaldf$HomePenalty[i] <- sum(penalties$Penalty.Yards[as.character(penalties$PenalizedTeam)==penalties$HomeTeam[1]])
  Finaldf$AwayPenalty[i] <- sum(penalties$Penalty.Yards[as.character(penalties$PenalizedTeam)==penalties$AwayTeam[1]])
    
  # winning
  Finaldf$win[i] <- 0

  if (as.character(game$HomeTeam[nrow(game)])==as.character(game$posteam[nrow(game)])){
    Finaldf$win[i] <- as.integer(game$ScoreDiff[nrow(game)]>0)
  }
  
  else {
    Finaldf$win[i] <- as.integer(game$ScoreDiff[nrow(game)]<0)
  }
  
  if (is.na(Finaldf$win[i])){
    if (as.character(game$HomeTeam[nrow(game)])==as.character(game$posteam[nrow(game)])){
      Finaldf$win[i] <- as.integer(game$ScoreDiff[nrow(game)-1]>0)
    }
    
    else {
      Finaldf$win[i] <- as.integer(game$ScoreDiff[nrow(game)-1]<0)
    }
  }
  
  
  # win_Prob is poawssessing team
  
  
  # see if its running
  print(i)
}

# write output to file
write.csv(Finaldf, "gamedata.csv", row.names = F)


