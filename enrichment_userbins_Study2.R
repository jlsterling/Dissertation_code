#set working directory
setwd("/Users/Jo/Dropbox/SMaPP/Legislator Project/Longitudinal/")
#Clear workspace by removing all objects
rm(list=ls(all=TRUE)); gc()

leg_twt <- read.csv("twitter_legislator_dataset.csv", header=TRUE, sep=",", quote = "\"")
myvars <- c("leg_num", "dwnom1", "inhib_di", "tentat_di", "anx_di", "anger_di", "certain_di", 
            "money_di", "social_di", "relig_di", "affil_di", "power_di", "risk_di", 
            "past_di", "future_di", "posemo_di", "negemo_di", "threat_di", "uncertain_di", 
            "resistchange_di", "inequality_di", "rulesreinf_di", "selfdisc_di", "nurturant_di", 
            "empathy_di", "achievement_di", "hedonism_di", "selfdirect_di", "stim_di", "benev_di", 
            "conform_di", "security_di", "tradition_di", "power_Neiman_di", 
            "universal_di", "harm_di", "fairness_di", "ingroup_di", "authority_di", 
            "purity_di", "powHV_di", "affilHV_di", "i_di", "we_di", "swear_di", "feel_di", 
            "achieve_di", "death_di", "sixltr_di", "verbs_di", "nouns_di")
newdata <- leg_twt[myvars]
rm(leg_twt)
####WORKED UP UNTIL HERE#####
#emptying recycle bin
gc()

#newdata[!complete.cases(newdata),]
newdata <- na.omit(newdata)
#newdata[!complete.cases(newdata),]


#sort by ideology
newdata$ideo <- newdata$dwnom1
newdata <- newdata[order(newdata$ideo), ]
newdata$casenum <- seq(from = 1, to = nrow(newdata))
plot(newdata$casenum, newdata$ideo)

######CREATING THE FUNCTIONS TO BE USED THROUGHOUT THE ANALYSES######
#split the dataset (data) into equally sized bins (based on the bin size "bin")
rawBins <- function(bin, data) {
  binSize <<- round(nrow(data) / bin)
  n <- nrow(data)
  r  <- rep(1:ceiling(n/binSize),each=binSize)[1:n]
  mtWindows <<- split(data,r)
  cat("Number of observations per bin = ", binSize)
}

#creates an individual case identifier that marks which bin each observation should be in
GenUserBins <- function(CountInBins, data){
  newBinNum <<- numeric(nrow(data))
  binCount <- 0
  listNum <- 0
  for (i in CountInBins){
    binCount <- binCount + 1
    temp_List <- as.list(rep(binCount, i))
    for (x in temp_List){
      listNum <- listNum + 1
      newBinNum[[listNum]] <<- x
    }
  }
}


#first splits dataset into equally sized bins, then checks to see whether users fall on the dividing line of these
# bins. If they do, it pushes the user into the bin in which most of her tweets fall. Then creates an individual 
# identifier of bin membership and splits the dataset by that identifier
##only thing you need to call in the end
##reads in each of the above two functions 
userbBins <- function(user_col = "user", data, bin) {
  rawBins(bin, data)
  userWindows <- split(data,data[, user_col])
  counter <- 0
  binNum <- 0
  countInBin <- numeric(bin)
  for (i in userWindows) {
    counter <- counter + nrow(i)
    if (counter <= binSize) {
      next
    }
    else {
      binNum <- binNum + 1
      extraInBin <- counter - binSize
      extraOutBin <- nrow(i) - extraInBin
      if (extraInBin >= extraOutBin){
        countInBin[[binNum]] <- counter
        counter <- 0
      }
      else {
        counter <- counter - nrow(i)
        countInBin[[binNum]] <- counter
        counter <- 0
        counter <- counter + nrow(i)
      }
    }  
  }
  GenUserBins(countInBin, data)
  h  <- newBinNum
  userWindows <<- split(newdata,h)
}


total_inhib <- sum(newdata$inhib_di)
total_tentat <- sum(newdata$tentat_di)
total_anger <- sum(newdata$anger_di)
total_certain <- sum(newdata$certain_di)
total_anx <- sum(newdata$anx_di)
total_money <- sum(newdata$money_di)
total_social <- sum(newdata$social_di)
total_relig<- sum(newdata$relig_di)
total_affil <- sum(newdata$affil_di)
total_power <- sum(newdata$power_di)
total_risk <- sum(newdata$risk_di)
total_past <- sum(newdata$past_di)
total_future <- sum(newdata$future_di)
total_posemo <- sum(newdata$posemo_di)
total_negemo <- sum(newdata$negemo_di)
total_threat <- sum(newdata$threat_di)
total_uncertain <- sum(newdata$uncertain_di)
total_resistch <- sum(newdata$resistchange_di)
total_inequal <- sum(newdata$inequality_di)
total_rulesrei <- sum(newdata$rulesreinf_di)
total_selfdisc <- sum(newdata$selfdisc_di)
total_nurtur <- sum(newdata$nurturant_di)
total_empath <- sum(newdata$empathy_di)
total_achievement <- sum(newdata$achievement_di)
total_hedonism <- sum(newdata$hedonism_di)
total_selfdirect <- sum(newdata$selfdirect_di)
total_stim <- sum(newdata$stim_di)
total_benev <- sum(newdata$benev_di)
total_conform <- sum(newdata$conform_di)
total_security <- sum(newdata$security_di)
total_tradition <- sum(newdata$tradition_di)
total_power_Neiman <- sum(newdata$power_Neiman_di)
total_universal <- sum(newdata$universal_di)
total_harm <- sum(newdata$harm_di)
total_fairness <- sum(newdata$fairness_di)
total_ingroup <- sum(newdata$ingroup_di)
total_authority <- sum(newdata$authority_di)
total_purity <- sum(newdata$purity_di)
total_powHV <- sum(newdata$powHV_di)
total_affilHV <- sum(newdata$affilHV_di)
total_i <- sum(newdata$i_di)
total_we <- sum(newdata$we_di)
total_swear <- sum(newdata$swear_di)
total_feel <- sum(newdata$feel_di)
total_achieve <- sum(newdata$achieve_di)
total_death <- sum(newdata$death_di)
total_sixltr <- sum(newdata$sixltr_di)


total_twt <- nrow(newdata)

total_noinhib <- total_twt - total_inhib
total_notentat <- total_twt - total_tentat
total_noanger <- total_twt - total_anger
total_nocertain <- total_twt - total_certain
total_noanx <- total_twt - total_anx
total_nomoney <- total_twt - total_money
total_nosocial <- total_twt - total_social
total_norelig<- total_twt - total_relig
total_noaffil <- total_twt - total_affil
total_nopower <- total_twt - total_power
total_norisk <- total_twt - total_risk
total_nopast <- total_twt - total_past
total_nofuture <- total_twt - total_future
total_noposemo <- total_twt - total_posemo
total_nonegemo <- total_twt - total_negemo
total_nothreat <- total_twt - total_threat
total_nouncertain <- total_twt - total_uncertain
total_noresistch <- total_twt - total_resistch
total_noinequal <- total_twt - total_inequal
total_norulesrei <- total_twt - total_rulesrei
total_noselfdisc <- total_twt - total_selfdisc
total_nonurtur <- total_twt - total_nurtur
total_noempath <- total_twt - total_empath
total_noachievement <- total_twt - total_achievement
total_nohedonism <- total_twt - total_hedonism
total_noselfdirect <- total_twt - total_selfdirect
total_nostim <- total_twt - total_stim
total_nobenev <- total_twt - total_benev
total_noconform <- total_twt - total_conform
total_nosecurity <- total_twt - total_security
total_notradition <- total_twt - total_tradition
total_nopower_Neiman <- total_twt - total_power_Neiman
total_nouniversal <- total_twt - total_universal
total_noharm <- total_twt - total_harm
total_nofairness <- total_twt - total_fairness
total_noingroup <- total_twt - total_ingroup
total_noauthority <- total_twt - total_authority
total_nopurity <- total_twt - total_purity
total_nopowHV <- total_twt - total_powHV
total_noaffilHV <- total_twt - total_affilHV
total_noi <- total_twt - total_i
total_nowe <- total_twt - total_we
total_noswear <- total_twt - total_swear
total_nofeel <- total_twt - total_feel
total_noachieve <- total_twt - total_achieve
total_nodeath <- total_twt - total_death
total_nosixltr <- total_twt - total_sixltr


#create windows
userbBins(user_col = "leg_num", newdata, 20)
#userbBins(user_col = "leg_num", newdata, 527)

#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

print("on inhibition")
count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_inhib <- sum(i$inhib_di)
      ttotal_twt <- nrow(i)
      ttotal_noinhib <- ttotal_twt - ttotal_inhib
      ctotal_inhib <- total_inhib - ttotal_inhib
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_noinhib <- total_noinhib - ttotal_noinhib
      cont.2.2 <- matrix(c(ttotal_inhib, ttotal_noinhib, ctotal_inhib, ctotal_noinhib), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_inhib_fish <- as.data.frame(p.vals)
df_inhib_fish$ci_low <- ci_low
df_inhib_fish$ci_high <- ci_high
df_inhib_fish$odds_ratio <- odds_ratio

save(df_inhib_fish,file="fischers_inhib_output.Rda")

print("on tentative")
#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_tentat <- sum(i$tentat_di)
      ttotal_twt <- nrow(i)
      ttotal_notentat <- ttotal_twt - ttotal_tentat
      ctotal_tentat <- total_tentat - ttotal_tentat
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_notentat <- total_notentat - ttotal_notentat
      cont.2.2 <- matrix(c(ttotal_tentat, ttotal_notentat, ctotal_tentat, ctotal_notentat), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_tentat_fish <- as.data.frame(p.vals)
df_tentat_fish$ci_low <- ci_low
df_tentat_fish$ci_high <- ci_high
df_tentat_fish$odds_ratio <- odds_ratio

save(df_tentat_fish,file="fischers_tentat_output.Rda")

print("on anger")
#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_anger <- sum(i$anger_di)
      ttotal_twt <- nrow(i)
      ttotal_noanger <- ttotal_twt - ttotal_anger
      ctotal_anger <- total_anger - ttotal_anger
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_noanger <- total_noanger - ttotal_noanger
      cont.2.2 <- matrix(c(ttotal_anger, ttotal_noanger, ctotal_anger, ctotal_noanger), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_anger_fish <- as.data.frame(p.vals)
df_anger_fish$ci_low <- ci_low
df_anger_fish$ci_high <- ci_high
df_anger_fish$odds_ratio <- odds_ratio

save(df_anger_fish,file="fischers_anger_output.Rda")

print("on certainty")

#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_certain <- sum(i$certain_di)
      ttotal_twt <- nrow(i)
      ttotal_nocertain <- ttotal_twt - ttotal_certain
      ctotal_certain <- total_certain - ttotal_certain
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_nocertain <- total_nocertain - ttotal_nocertain
      cont.2.2 <- matrix(c(ttotal_certain, ttotal_nocertain, ctotal_certain, ctotal_nocertain), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_certain_fish <- as.data.frame(p.vals)
df_certain_fish$ci_low <- ci_low
df_certain_fish$ci_high <- ci_high
df_certain_fish$odds_ratio <- odds_ratio

save(df_certain_fish,file="fischers_certain_output.Rda")

print("on anxiety")

#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
  count <- count + 1
  ttotal_anx <- sum(i$anx_di)
  ttotal_twt <- nrow(i)
  ttotal_noanx <- ttotal_twt - ttotal_anx
  ctotal_anx <- total_anx - ttotal_anx
  ctotal_twt <- total_twt - ttotal_twt
  ctotal_noanx <- total_noanx - ttotal_noanx
  cont.2.2 <- matrix(c(ttotal_anx, ttotal_noanx, ctotal_anx, ctotal_noanx), 2)
  f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
  p.vals[[count]] <- f.tmp$p.value
  ci_low[[count]] <- f.tmp$conf.int[1]
  ci_high[[count]] <- f.tmp$conf.int[2]
  odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_anx_fish <- as.data.frame(p.vals)
df_anx_fish$ci_low <- ci_low
df_anx_fish$ci_high <- ci_high
df_anx_fish$odds_ratio <- odds_ratio

save(df_anx_fish,file="fischers_anx_output.Rda")


print("on money")
#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_money <- sum(i$money_di)
      ttotal_twt <- nrow(i)
      ttotal_nomoney <- ttotal_twt - ttotal_money
      ctotal_money <- total_money - ttotal_money
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_nomoney <- total_nomoney - ttotal_nomoney
      cont.2.2 <- matrix(c(ttotal_money, ttotal_nomoney, ctotal_money, ctotal_nomoney), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_money_fish <- as.data.frame(p.vals)
df_money_fish$ci_low <- ci_low
df_money_fish$ci_high <- ci_high
df_money_fish$odds_ratio <- odds_ratio

save(df_money_fish,file="fischers_money_output.Rda")

print("on social")
#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_social <- sum(i$social_di)
      ttotal_twt <- nrow(i)
      ttotal_nosocial <- ttotal_twt - ttotal_social
      ctotal_social <- total_social - ttotal_social
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_nosocial <- total_nosocial - ttotal_nosocial
      cont.2.2 <- matrix(c(ttotal_social, ttotal_nosocial, ctotal_social, ctotal_nosocial), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_social_fish <- as.data.frame(p.vals)
df_social_fish$ci_low <- ci_low
df_social_fish$ci_high <- ci_high
df_social_fish$odds_ratio <- odds_ratio

save(df_social_fish,file="fischers_social_output.Rda")

print("on religion")

#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_relig <- sum(i$relig_di)
      ttotal_twt <- nrow(i)
      ttotal_norelig <- ttotal_twt - ttotal_relig
      ctotal_relig <- total_relig - ttotal_relig
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_norelig <- total_norelig - ttotal_norelig
      cont.2.2 <- matrix(c(ttotal_relig, ttotal_norelig, ctotal_relig, ctotal_norelig), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_relig_fish <- as.data.frame(p.vals)
df_relig_fish$ci_low <- ci_low
df_relig_fish$ci_high <- ci_high
df_relig_fish$odds_ratio <- odds_ratio

save(df_relig_fish,file="fischers_relig_output.Rda")

print("on affiliation")

#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_affil <- sum(i$affil_di)
      ttotal_twt <- nrow(i)
      ttotal_noaffil <- ttotal_twt - ttotal_affil
      ctotal_affil <- total_affil - ttotal_affil
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_noaffil <- total_noaffil - ttotal_noaffil
      cont.2.2 <- matrix(c(ttotal_affil, ttotal_noaffil, ctotal_affil, ctotal_noaffil), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_affil_fish <- as.data.frame(p.vals)
df_affil_fish$ci_low <- ci_low
df_affil_fish$ci_high <- ci_high
df_affil_fish$odds_ratio <- odds_ratio

save(df_affil_fish,file="fischers_affil_output.Rda")

print("on power")
#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_power <- sum(i$power_di)
      ttotal_twt <- nrow(i)
      ttotal_nopower <- ttotal_twt - ttotal_power
      ctotal_power <- total_power - ttotal_power
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_nopower <- total_nopower - ttotal_nopower
      cont.2.2 <- matrix(c(ttotal_power, ttotal_nopower, ctotal_power, ctotal_nopower), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_power_fish <- as.data.frame(p.vals)
df_power_fish$ci_low <- ci_low
df_power_fish$ci_high <- ci_high
df_power_fish$odds_ratio <- odds_ratio

save(df_power_fish,file="fischers_power_output.Rda")

print("on risk")
#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_risk <- sum(i$risk_di)
      ttotal_twt <- nrow(i)
      ttotal_norisk <- ttotal_twt - ttotal_risk
      ctotal_risk <- total_risk - ttotal_risk
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_norisk <- total_norisk - ttotal_norisk
      cont.2.2 <- matrix(c(ttotal_risk, ttotal_norisk, ctotal_risk, ctotal_norisk), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_risk_fish <- as.data.frame(p.vals)
df_risk_fish$ci_low <- ci_low
df_risk_fish$ci_high <- ci_high
df_risk_fish$odds_ratio <- odds_ratio

save(df_risk_fish,file="fischers_risk_output.Rda")

print("on past")
#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_past <- sum(i$past_di)
      ttotal_twt <- nrow(i)
      ttotal_nopast <- ttotal_twt - ttotal_past
      ctotal_past <- total_past - ttotal_past
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_nopast <- total_nopast - ttotal_nopast
      cont.2.2 <- matrix(c(ttotal_past, ttotal_nopast, ctotal_past, ctotal_nopast), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_past_fish <- as.data.frame(p.vals)
df_past_fish$ci_low <- ci_low
df_past_fish$ci_high <- ci_high
df_past_fish$odds_ratio <- odds_ratio

save(df_past_fish,file="fischers_past_output.Rda")

print("on future")
#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_future <- sum(i$future_di)
      ttotal_twt <- nrow(i)
      ttotal_nofuture <- ttotal_twt - ttotal_future
      ctotal_future <- total_future - ttotal_future
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_nofuture <- total_nofuture - ttotal_nofuture
      cont.2.2 <- matrix(c(ttotal_future, ttotal_nofuture, ctotal_future, ctotal_nofuture), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_future_fish <- as.data.frame(p.vals)
df_future_fish$ci_low <- ci_low
df_future_fish$ci_high <- ci_high
df_future_fish$odds_ratio <- odds_ratio

save(df_future_fish,file="fischers_future_output.Rda")


print("on positive emotion")
#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_posemo <- sum(i$posemo_di)
      ttotal_twt <- nrow(i)
      ttotal_noposemo <- ttotal_twt - ttotal_posemo
      ctotal_posemo <- total_posemo - ttotal_posemo
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_noposemo <- total_noposemo - ttotal_noposemo
      cont.2.2 <- matrix(c(ttotal_posemo, ttotal_noposemo, ctotal_posemo, ctotal_noposemo), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_posemo_fish <- as.data.frame(p.vals)
df_posemo_fish$ci_low <- ci_low
df_posemo_fish$ci_high <- ci_high
df_posemo_fish$odds_ratio <- odds_ratio

save(df_posemo_fish,file="fischers_posemo_output.Rda")

print("on negative emotion")

#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_negemo <- sum(i$negemo_di)
      ttotal_twt <- nrow(i)
      ttotal_nonegemo <- ttotal_twt - ttotal_negemo
      ctotal_negemo <- total_negemo - ttotal_negemo
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_nonegemo <- total_nonegemo - ttotal_nonegemo
      cont.2.2 <- matrix(c(ttotal_negemo, ttotal_nonegemo, ctotal_negemo, ctotal_nonegemo), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_negemo_fish <- as.data.frame(p.vals)
df_negemo_fish$ci_low <- ci_low
df_negemo_fish$ci_high <- ci_high
df_negemo_fish$odds_ratio <- odds_ratio

save(df_negemo_fish,file="fischers_negemo_output.Rda")

print("on threat")

#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_threat <- sum(i$threat_di)
      ttotal_twt <- nrow(i)
      ttotal_nothreat <- ttotal_twt - ttotal_threat
      ctotal_threat <- total_threat - ttotal_threat
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_nothreat <- total_nothreat - ttotal_nothreat
      cont.2.2 <- matrix(c(ttotal_threat, ttotal_nothreat, ctotal_threat, ctotal_nothreat), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_threat_fish <- as.data.frame(p.vals)
df_threat_fish$ci_low <- ci_low
df_threat_fish$ci_high <- ci_high
df_threat_fish$odds_ratio <- odds_ratio

save(df_threat_fish,file="fischers_threat_output.Rda")

print("on uncertain")

#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_uncertain <- sum(i$uncertain_di)
      ttotal_twt <- nrow(i)
      ttotal_nouncertain <- ttotal_twt - ttotal_uncertain
      ctotal_uncertain <- total_uncertain - ttotal_uncertain
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_nouncertain <- total_nouncertain - ttotal_nouncertain
      cont.2.2 <- matrix(c(ttotal_uncertain, ttotal_nouncertain, ctotal_uncertain, ctotal_nouncertain), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_uncertain_fish <- as.data.frame(p.vals)
df_uncertain_fish$ci_low <- ci_low
df_uncertain_fish$ci_high <- ci_high
df_uncertain_fish$odds_ratio <- odds_ratio

save(df_uncertain_fish,file="fischers_uncertain_output.Rda")

print("on resistance to change")
#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_resistch <- sum(i$resistchange_di)
      ttotal_twt <- nrow(i)
      ttotal_noresistch <- ttotal_twt - ttotal_resistch
      ctotal_resistch <- total_resistch - ttotal_resistch
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_noresistch <- total_noresistch - ttotal_noresistch
      cont.2.2 <- matrix(c(ttotal_resistch, ttotal_noresistch, ctotal_resistch, ctotal_noresistch), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_resistch_fish <- as.data.frame(p.vals)
df_resistch_fish$ci_low <- ci_low
df_resistch_fish$ci_high <- ci_high
df_resistch_fish$odds_ratio <- odds_ratio

save(df_resistch_fish,file="fischers_resistch_output.Rda")

print("on inequality")
#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_inequal <- sum(i$inequality_di)
      ttotal_twt <- nrow(i)
      ttotal_noinequal <- ttotal_twt - ttotal_inequal
      ctotal_inequal <- total_inequal - ttotal_inequal
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_noinequal <- total_noinequal - ttotal_noinequal
      cont.2.2 <- matrix(c(ttotal_inequal, ttotal_noinequal, ctotal_inequal, ctotal_noinequal), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_inequal_fish <- as.data.frame(p.vals)
df_inequal_fish$ci_low <- ci_low
df_inequal_fish$ci_high <- ci_high
df_inequal_fish$odds_ratio <- odds_ratio

save(df_inequal_fish,file="fischers_inequal_output.Rda")

print("on rules and reinforcement")
#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_rulesrei <- sum(i$rulesreinf_di)
      ttotal_twt <- nrow(i)
      ttotal_norulesrei <- ttotal_twt - ttotal_rulesrei
      ctotal_rulesrei <- total_rulesrei - ttotal_rulesrei
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_norulesrei <- total_norulesrei - ttotal_norulesrei
      cont.2.2 <- matrix(c(ttotal_rulesrei, ttotal_norulesrei, ctotal_rulesrei, ctotal_norulesrei), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_rulesrei_fish <- as.data.frame(p.vals)
df_rulesrei_fish$ci_low <- ci_low
df_rulesrei_fish$ci_high <- ci_high
df_rulesrei_fish$odds_ratio <- odds_ratio

save(df_rulesrei_fish,file="fischers_rulesrei_output.Rda")

print("on self-discipline")

#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_selfdisc <- sum(i$selfdisc_di)
      ttotal_twt <- nrow(i)
      ttotal_noselfdisc <- ttotal_twt - ttotal_selfdisc
      ctotal_selfdisc <- total_selfdisc - ttotal_selfdisc
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_noselfdisc <- total_noselfdisc - ttotal_noselfdisc
      cont.2.2 <- matrix(c(ttotal_selfdisc, ttotal_noselfdisc, ctotal_selfdisc, ctotal_noselfdisc), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_selfdisc_fish <- as.data.frame(p.vals)
df_selfdisc_fish$ci_low <- ci_low
df_selfdisc_fish$ci_high <- ci_high
df_selfdisc_fish$odds_ratio <- odds_ratio

save(df_selfdisc_fish,file="fischers_selfdisc_output.Rda")

print("on nurturant/caregiving")
#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_nurtur <- sum(i$nurturant_di)
      ttotal_twt <- nrow(i)
      ttotal_nonurtur <- ttotal_twt - ttotal_nurtur
      ctotal_nurtur <- total_nurtur - ttotal_nurtur
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_nonurtur <- total_nonurtur - ttotal_nonurtur
      cont.2.2 <- matrix(c(ttotal_nurtur, ttotal_nonurtur, ctotal_nurtur, ctotal_nonurtur), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_nurtur_fish <- as.data.frame(p.vals)
df_nurtur_fish$ci_low <- ci_low
df_nurtur_fish$ci_high <- ci_high
df_nurtur_fish$odds_ratio <- odds_ratio

save(df_nurtur_fish,file="fischers_nurtur_output.Rda")

print("on empathy")

#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_empath <- sum(i$empathy_di)
      ttotal_twt <- nrow(i)
      ttotal_noempath <- ttotal_twt - ttotal_empath
      ctotal_empath <- total_empath - ttotal_empath
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_noempath <- total_noempath - ttotal_noempath
      cont.2.2 <- matrix(c(ttotal_empath, ttotal_noempath, ctotal_empath, ctotal_noempath), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_empath_fish <- as.data.frame(p.vals)
df_empath_fish$ci_low <- ci_low
df_empath_fish$ci_high <- ci_high
df_empath_fish$odds_ratio <- odds_ratio

save(df_empath_fish,file="fischers_empath_output.Rda")

print("on achievement")
#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_achievement <- sum(i$achievement_di)
      ttotal_twt <- nrow(i)
      ttotal_noachievement <- ttotal_twt - ttotal_achievement
      ctotal_achievement <- total_achievement - ttotal_achievement
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_noachievement <- total_noachievement - ttotal_noachievement
      cont.2.2 <- matrix(c(ttotal_achievement, ttotal_noachievement, ctotal_achievement, ctotal_noachievement), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_achievement_fish <- as.data.frame(p.vals)
df_achievement_fish$ci_low <- ci_low
df_achievement_fish$ci_high <- ci_high
df_achievement_fish$odds_ratio <- odds_ratio

save(df_achievement_fish,file="fischers_achievement_output.Rda")

print("on hedonism")

#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_hedonism <- sum(i$hedonism_di)
      ttotal_twt <- nrow(i)
      ttotal_nohedonism <- ttotal_twt - ttotal_hedonism
      ctotal_hedonism <- total_hedonism - ttotal_hedonism
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_nohedonism <- total_nohedonism - ttotal_nohedonism
      cont.2.2 <- matrix(c(ttotal_hedonism, ttotal_nohedonism, ctotal_hedonism, ctotal_nohedonism), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_hedonism_fish <- as.data.frame(p.vals)
df_hedonism_fish$ci_low <- ci_low
df_hedonism_fish$ci_high <- ci_high
df_hedonism_fish$odds_ratio <- odds_ratio

save(df_hedonism_fish,file="fischers_hedonism_output.Rda")

print("on self-direction")
#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_selfdirect <- sum(i$selfdirect_di)
      ttotal_twt <- nrow(i)
      ttotal_noselfdirect <- ttotal_twt - ttotal_selfdirect
      ctotal_selfdirect <- total_selfdirect - ttotal_selfdirect
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_noselfdirect <- total_noselfdirect - ttotal_noselfdirect
      cont.2.2 <- matrix(c(ttotal_selfdirect, ttotal_noselfdirect, ctotal_selfdirect, ctotal_noselfdirect), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_selfdirect_fish <- as.data.frame(p.vals)
df_selfdirect_fish$ci_low <- ci_low
df_selfdirect_fish$ci_high <- ci_high
df_selfdirect_fish$odds_ratio <- odds_ratio

save(df_selfdirect_fish,file="fischers_selfdirect_output.Rda")

print("on stimulation")

#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_stim <- sum(i$stim_di)
      ttotal_twt <- nrow(i)
      ttotal_nostim <- ttotal_twt - ttotal_stim
      ctotal_stim <- total_stim - ttotal_stim
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_nostim <- total_nostim - ttotal_nostim
      cont.2.2 <- matrix(c(ttotal_stim, ttotal_nostim, ctotal_stim, ctotal_nostim), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_stim_fish <- as.data.frame(p.vals)
df_stim_fish$ci_low <- ci_low
df_stim_fish$ci_high <- ci_high
df_stim_fish$odds_ratio <- odds_ratio

save(df_stim_fish,file="fischers_stim_output.Rda")

print("on benevolence")
#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_benev <- sum(i$benev_di)
      ttotal_twt <- nrow(i)
      ttotal_nobenev <- ttotal_twt - ttotal_benev
      ctotal_benev <- total_benev - ttotal_benev
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_nobenev <- total_nobenev - ttotal_nobenev
      cont.2.2 <- matrix(c(ttotal_benev, ttotal_nobenev, ctotal_benev, ctotal_nobenev), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_benev_fish <- as.data.frame(p.vals)
df_benev_fish$ci_low <- ci_low
df_benev_fish$ci_high <- ci_high
df_benev_fish$odds_ratio <- odds_ratio

save(df_benev_fish,file="fischers_benev_output.Rda")

print("on conformity")
#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_conform <- sum(i$conform_di)
      ttotal_twt <- nrow(i)
      ttotal_noconform <- ttotal_twt - ttotal_conform
      ctotal_conform <- total_conform - ttotal_conform
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_noconform <- total_noconform - ttotal_noconform
      cont.2.2 <- matrix(c(ttotal_conform, ttotal_noconform, ctotal_conform, ctotal_noconform), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_conform_fish <- as.data.frame(p.vals)
df_conform_fish$ci_low <- ci_low
df_conform_fish$ci_high <- ci_high
df_conform_fish$odds_ratio <- odds_ratio

save(df_conform_fish,file="fischers_conform_output.Rda")

print("on security")
#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_security <- sum(i$security_di)
      ttotal_twt <- nrow(i)
      ttotal_nosecurity <- ttotal_twt - ttotal_security
      ctotal_security <- total_security - ttotal_security
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_nosecurity <- total_nosecurity - ttotal_nosecurity
      cont.2.2 <- matrix(c(ttotal_security, ttotal_nosecurity, ctotal_security, ctotal_nosecurity), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_security_fish <- as.data.frame(p.vals)
df_security_fish$ci_low <- ci_low
df_security_fish$ci_high <- ci_high
df_security_fish$odds_ratio <- odds_ratio

save(df_security_fish,file="fischers_security_output.Rda")

print("on tradition")
#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_tradition <- sum(i$tradition_di)
      ttotal_twt <- nrow(i)
      ttotal_notradition <- ttotal_twt - ttotal_tradition
      ctotal_tradition <- total_tradition - ttotal_tradition
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_notradition <- total_notradition - ttotal_notradition
      cont.2.2 <- matrix(c(ttotal_tradition, ttotal_notradition, ctotal_tradition, ctotal_notradition), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_tradition_fish <- as.data.frame(p.vals)
df_tradition_fish$ci_low <- ci_low
df_tradition_fish$ci_high <- ci_high
df_tradition_fish$odds_ratio <- odds_ratio

save(df_tradition_fish,file="fischers_tradition_output.Rda")

print("on power (Neiman)")

#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_power_Neiman <- sum(i$power_Neiman_di)
      ttotal_twt <- nrow(i)
      ttotal_nopower_Neiman <- ttotal_twt - ttotal_power_Neiman
      ctotal_power_Neiman <- total_power_Neiman - ttotal_power_Neiman
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_nopower_Neiman <- total_nopower_Neiman - ttotal_nopower_Neiman
      cont.2.2 <- matrix(c(ttotal_power_Neiman, ttotal_nopower_Neiman, ctotal_power_Neiman, ctotal_nopower_Neiman), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_power_Neiman_fish <- as.data.frame(p.vals)
df_power_Neiman_fish$ci_low <- ci_low
df_power_Neiman_fish$ci_high <- ci_high
df_power_Neiman_fish$odds_ratio <- odds_ratio

save(df_power_Neiman_fish,file="fischers_power_Neiman_output.Rda")

print("on universal")

#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_universal <- sum(i$universal_di)
      ttotal_twt <- nrow(i)
      ttotal_nouniversal <- ttotal_twt - ttotal_universal
      ctotal_universal <- total_universal - ttotal_universal
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_nouniversal <- total_nouniversal - ttotal_nouniversal
      cont.2.2 <- matrix(c(ttotal_universal, ttotal_nouniversal, ctotal_universal, ctotal_nouniversal), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_universal_fish <- as.data.frame(p.vals)
df_universal_fish$ci_low <- ci_low
df_universal_fish$ci_high <- ci_high
df_universal_fish$odds_ratio <- odds_ratio

save(df_universal_fish,file="fischers_universal_output.Rda")

print("on harm")

#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_harm <- sum(i$harm_di)
      ttotal_twt <- nrow(i)
      ttotal_noharm <- ttotal_twt - ttotal_harm
      ctotal_harm <- total_harm - ttotal_harm
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_noharm <- total_noharm - ttotal_noharm
      cont.2.2 <- matrix(c(ttotal_harm, ttotal_noharm, ctotal_harm, ctotal_noharm), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_harm_fish <- as.data.frame(p.vals)
df_harm_fish$ci_low <- ci_low
df_harm_fish$ci_high <- ci_high
df_harm_fish$odds_ratio <- odds_ratio

save(df_harm_fish,file="fischers_harm_output.Rda")

print("on fairness")

#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_fairness <- sum(i$fairness_di)
      ttotal_twt <- nrow(i)
      ttotal_nofairness <- ttotal_twt - ttotal_fairness
      ctotal_fairness <- total_fairness - ttotal_fairness
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_nofairness <- total_nofairness - ttotal_nofairness
      cont.2.2 <- matrix(c(ttotal_fairness, ttotal_nofairness, ctotal_fairness, ctotal_nofairness), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_fairness_fish <- as.data.frame(p.vals)
df_fairness_fish$ci_low <- ci_low
df_fairness_fish$ci_high <- ci_high
df_fairness_fish$odds_ratio <- odds_ratio

save(df_fairness_fish,file="fischers_fairness_output.Rda")

print("on ingroup")

#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_ingroup <- sum(i$ingroup_di)
      ttotal_twt <- nrow(i)
      ttotal_noingroup <- ttotal_twt - ttotal_ingroup
      ctotal_ingroup <- total_ingroup - ttotal_ingroup
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_noingroup <- total_noingroup - ttotal_noingroup
      cont.2.2 <- matrix(c(ttotal_ingroup, ttotal_noingroup, ctotal_ingroup, ctotal_noingroup), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_ingroup_fish <- as.data.frame(p.vals)
df_ingroup_fish$ci_low <- ci_low
df_ingroup_fish$ci_high <- ci_high
df_ingroup_fish$odds_ratio <- odds_ratio

save(df_ingroup_fish,file="fischers_ingroup_output.Rda")

print("on authority")

#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_authority <- sum(i$authority_di)
      ttotal_twt <- nrow(i)
      ttotal_noauthority <- ttotal_twt - ttotal_authority
      ctotal_authority <- total_authority - ttotal_authority
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_noauthority <- total_noauthority - ttotal_noauthority
      cont.2.2 <- matrix(c(ttotal_authority, ttotal_noauthority, ctotal_authority, ctotal_noauthority), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_authority_fish <- as.data.frame(p.vals)
df_authority_fish$ci_low <- ci_low
df_authority_fish$ci_high <- ci_high
df_authority_fish$odds_ratio <- odds_ratio

save(df_authority_fish,file="fischers_authority_output.Rda")

print("on purity")

#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_purity <- sum(i$purity_di)
      ttotal_twt <- nrow(i)
      ttotal_nopurity <- ttotal_twt - ttotal_purity
      ctotal_purity <- total_purity - ttotal_purity
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_nopurity <- total_nopurity - ttotal_nopurity
      cont.2.2 <- matrix(c(ttotal_purity, ttotal_nopurity, ctotal_purity, ctotal_nopurity), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_purity_fish <- as.data.frame(p.vals)
df_purity_fish$ci_low <- ci_low
df_purity_fish$ci_high <- ci_high
df_purity_fish$odds_ratio <- odds_ratio

save(df_purity_fish,file="fischers_purity_output.Rda")

print("on power (Harvard)")

#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_powHV <- sum(i$powHV_di)
      ttotal_twt <- nrow(i)
      ttotal_nopowHV <- ttotal_twt - ttotal_powHV
      ctotal_powHV <- total_powHV - ttotal_powHV
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_nopowHV <- total_nopowHV - ttotal_nopowHV
      cont.2.2 <- matrix(c(ttotal_powHV, ttotal_nopowHV, ctotal_powHV, ctotal_nopowHV), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_powHV_fish <- as.data.frame(p.vals)
df_powHV_fish$ci_low <- ci_low
df_powHV_fish$ci_high <- ci_high
df_powHV_fish$odds_ratio <- odds_ratio

save(df_powHV_fish,file="fischers_powHV_output.Rda")

print("on affiliation (Harvard)")

#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_affilHV <- sum(i$affilHV_di)
      ttotal_twt <- nrow(i)
      ttotal_noaffilHV <- ttotal_twt - ttotal_affilHV
      ctotal_affilHV <- total_affilHV - ttotal_affilHV
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_noaffilHV <- total_noaffilHV - ttotal_noaffilHV
      cont.2.2 <- matrix(c(ttotal_affilHV, ttotal_noaffilHV, ctotal_affilHV, ctotal_noaffilHV), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_affilHV_fish <- as.data.frame(p.vals)
df_affilHV_fish$ci_low <- ci_low
df_affilHV_fish$ci_high <- ci_high
df_affilHV_fish$odds_ratio <- odds_ratio

save(df_affilHV_fish,file="fischers_affilHV_output.Rda")

print("on i")

#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_i <- sum(i$i_di)
      ttotal_twt <- nrow(i)
      ttotal_noi <- ttotal_twt - ttotal_i
      ctotal_i <- total_i - ttotal_i
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_noi <- total_noi - ttotal_noi
      cont.2.2 <- matrix(c(ttotal_i, ttotal_noi, ctotal_i, ctotal_noi), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_i_fish <- as.data.frame(p.vals)
df_i_fish$ci_low <- ci_low
df_i_fish$ci_high <- ci_high
df_i_fish$odds_ratio <- odds_ratio

save(df_i_fish,file="fischers_i_output.Rda")

print("on we")

#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_we <- sum(i$we_di)
      ttotal_twt <- nrow(i)
      ttotal_nowe <- ttotal_twt - ttotal_we
      ctotal_we <- total_we - ttotal_we
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_nowe <- total_nowe - ttotal_nowe
      cont.2.2 <- matrix(c(ttotal_we, ttotal_nowe, ctotal_we, ctotal_nowe), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_we_fish <- as.data.frame(p.vals)
df_we_fish$ci_low <- ci_low
df_we_fish$ci_high <- ci_high
df_we_fish$odds_ratio <- odds_ratio

save(df_we_fish,file="fischers_we_output.Rda")

print("on swear")

#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_swear <- sum(i$swear_di)
      ttotal_twt <- nrow(i)
      ttotal_noswear <- ttotal_twt - ttotal_swear
      ctotal_swear <- total_swear - ttotal_swear
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_noswear <- total_noswear - ttotal_noswear
      cont.2.2 <- matrix(c(ttotal_swear, ttotal_noswear, ctotal_swear, ctotal_noswear), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_swear_fish <- as.data.frame(p.vals)
df_swear_fish$ci_low <- ci_low
df_swear_fish$ci_high <- ci_high
df_swear_fish$odds_ratio <- odds_ratio

save(df_swear_fish,file="fischers_swear_output.Rda")

print("on feeling")

#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_feel <- sum(i$feel_di)
      ttotal_twt <- nrow(i)
      ttotal_nofeel <- ttotal_twt - ttotal_feel
      ctotal_feel <- total_feel - ttotal_feel
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_nofeel <- total_nofeel - ttotal_nofeel
      cont.2.2 <- matrix(c(ttotal_feel, ttotal_nofeel, ctotal_feel, ctotal_nofeel), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_feel_fish <- as.data.frame(p.vals)
df_feel_fish$ci_low <- ci_low
df_feel_fish$ci_high <- ci_high
df_feel_fish$odds_ratio <- odds_ratio

save(df_feel_fish,file="fischers_feel_output.Rda")

print("on achieve")

#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_achieve <- sum(i$achieve_di)
      ttotal_twt <- nrow(i)
      ttotal_noachieve <- ttotal_twt - ttotal_achieve
      ctotal_achieve <- total_achieve - ttotal_achieve
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_noachieve <- total_noachieve - ttotal_noachieve
      cont.2.2 <- matrix(c(ttotal_achieve, ttotal_noachieve, ctotal_achieve, ctotal_noachieve), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_achieve_fish <- as.data.frame(p.vals)
df_achieve_fish$ci_low <- ci_low
df_achieve_fish$ci_high <- ci_high
df_achieve_fish$odds_ratio <- odds_ratio

save(df_achieve_fish,file="fischers_achieve_output.Rda")

print("on death")

#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_death <- sum(i$death_di)
      ttotal_twt <- nrow(i)
      ttotal_nodeath <- ttotal_twt - ttotal_death
      ctotal_death <- total_death - ttotal_death
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_nodeath <- total_nodeath - ttotal_nodeath
      cont.2.2 <- matrix(c(ttotal_death, ttotal_nodeath, ctotal_death, ctotal_nodeath), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_death_fish <- as.data.frame(p.vals)
df_death_fish$ci_low <- ci_low
df_death_fish$ci_high <- ci_high
df_death_fish$odds_ratio <- odds_ratio

save(df_death_fish,file="fischers_death_output.Rda")

print("on six letter")

#march down windows in m increments
p.vals <- numeric(length(userWindows))
ci_low <- numeric(length(userWindows))
ci_high <- numeric(length(userWindows))
odds_ratio <- numeric(length(userWindows))

count <- 0
for(i in userWindows){
      count <- count + 1
      ttotal_sixltr <- sum(i$sixltr_di)
      ttotal_twt <- nrow(i)
      ttotal_nosixltr <- ttotal_twt - ttotal_sixltr
      ctotal_sixltr <- total_sixltr - ttotal_sixltr
      ctotal_twt <- total_twt - ttotal_twt
      ctotal_nosixltr <- total_nosixltr - ttotal_nosixltr
      cont.2.2 <- matrix(c(ttotal_sixltr, ttotal_nosixltr, ctotal_sixltr, ctotal_nosixltr), 2)
      f.tmp <- fisher.test(cont.2.2, alter = "two.sided")
      p.vals[[count]] <- f.tmp$p.value
      ci_low[[count]] <- f.tmp$conf.int[1]
      ci_high[[count]] <- f.tmp$conf.int[2]
      odds_ratio[[count]] <- f.tmp$estimate
}

require(devtools)
df_sixltr_fish <- as.data.frame(p.vals)
df_sixltr_fish$ci_low <- ci_low
df_sixltr_fish$ci_high <- ci_high
df_sixltr_fish$odds_ratio <- odds_ratio

save(df_sixltr_fish,file="fischers_sixltr_output.Rda")





