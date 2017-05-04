#set working directory
setwd("/Users/Jo/Dropbox/SMaPP/Legislator Project/Longitudinal/")
#Clear workspace by removing all objects
rm(list=ls(all=TRUE)); gc()

leg_twt <- read.csv("twitter_legislator_dataset.csv", header=TRUE, sep=",", quote = "\"")
myvars <- c("leg_num", "wc", "dwnom1", "inhib_di", "tentat_di", "anx_di", "anger_di", "certain_di", 
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
#emptying recycle bin
gc()
newdata <- na.omit(newdata)
#sort by ideology
newdata$ideo <- newdata$dwnom1
newdata <- newdata[order(newdata$ideo), ]

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

#create windows
userbBins(user_col = "leg_num", newdata, 20)


#march down windows in m increments
mean_ideo <- numeric(length(userWindows))
median_ideo <- numeric(length(userWindows))
min_ideo <- numeric(length(userWindows))
max_ideo <- numeric(length(userWindows))
mean_wc <- numeric(length(userWindows))
median_wc <- numeric(length(userWindows))
min_wc <- numeric(length(userWindows))
max_wc <- numeric(length(userWindows))


count <- 0
for(i in userWindows){
  count <- count + 1
  ideo <- i$ideo
  wc <- i$wc
  mean_ideo[[count]] <- mean(ideo, na.rm = TRUE)
  median_ideo[[count]] <- median(ideo, na.rm = TRUE)
  min_ideo[[count]] <- min(ideo)
  max_ideo[[count]] <- max(ideo)
  mean_wc[[count]] <- mean(wc, na.rm = TRUE)
  median_wc[[count]] <- median(wc, na.rm = TRUE)
  min_wc[[count]] <- min(wc)
  max_wc[[count]] <- max(wc)
}


require(devtools)
df_ideobins <- as.data.frame(mean_ideo)
df_ideobins$median_ideo <- median_ideo
df_ideobins$min_ideo <- min_ideo
df_ideobins$max_ideo <- max_ideo
df_ideobins$mean_wc <- mean_wc
df_ideobins$median_wc <- median_wc
df_ideobins$min_wc <- min_wc
df_ideobins$max_wc <- max_wc

save(df_ideobins,file="ideo_for_bins_leg.Rda")


