#########################################################################

#FILE NAME: graphing_withfunctions.R
#CREATED ON: 3/29/2017
#CREATED BY: Joanna Sterling  
#PURPOSE: To graph the results of nonparametric fischers tests against the results
##        of parametric regression analyses. Here both linear and quadratic.
#DATA IN: R files of: ideology estimates of fischer bins, word count estimates of fischer bins,
##        results of fishers analyses, estimates from median model of regression analyses
#PROCESSING:Reads in ideology and word count estimates per bins. Plots the fischers scores for each
##        bin. Reads in the median regression equations (both linear and quadratic). Plots each line based
##        on the ideology and word count estimates per bin
#DATA OUT: .PDFs of each language type you care about with point estimates for the proportion of tweets that
##        contain this kind of language compared to the proportion of tweets that contain this language on average.
##        One line which is a smoothed moving average of those points. One line that estimates the relationship
##        based on the linear regression estimates. One line that estimates the relationship based on teh 
##        quadratic relationship estimates

#########################################################################

####PREPARING YOUR WORKSPACE####

#Clear workspace by removing all objects
rm(list=ls(all=TRUE))
gc()

#Load required packages

####LOADING AND PREPPING DATA####

#set working directory
setwd("/Users/Jo/Dropbox/SMaPP/Legislator Project/Longitudinal/enrich_combine")
rm(list=ls(all=TRUE)); gc()
#load in bin estimates for ideology and word count
load("ideo_for_bins_leg.Rda")
#load in the total word counts for the entire dataset
load("word_tweet_totals.Rda")

####CREATING REGRESSION BASED PREDICTED VALUES####

#split dataset into bins
chunk <- 1
n <- nrow(df_ideobins)
r  <- rep(1:ceiling(n/chunk),each=chunk)[1:n]
mtWindows <- split(df_ideobins,r)
#find average wordcount of all bins
wc <- mean(df_ideobins$mean_wc)

regModels <- read.csv("/Users/jo/Dropbox/SMaPP/Legislator Project/Longitudinal/reg_models.csv")
for (i in 1:nrow(regModels)) {
  rowLang <- regModels[i, ]
  langCat <- as.character(rowLang$lang_cat)
  consCoefL <- as.numeric(rowLang$consL)
  ideoCoefL <- as.numeric(rowLang$ideoL)
  wcCoefL <- as.numeric(rowLang$logwcL)
  consCoefQ <- as.numeric(rowLang$consQ)
  ideoCoefQ <- as.numeric(rowLang$ideoQ)
  ideoSqCoefQ <- as.numeric(rowLang$ideo_sqQ)
  wcCoefQ <- as.numeric(rowLang$logwcQ)
  linPredVals = numeric((nrow(df_ideobins)))
  quadPredVals = numeric((nrow(df_ideobins)))
  count <- 0
  for (j in mtWindows) {
    count <- count + 1
    ideo <- j$mean_ideo
    linPredVals[[count]] <- 1 / (1 + exp(-(consCoefL + (ideoCoefL * ideo) + (wcCoefL * log(wc)))))
    quadPredVals[[count]] <- 1 / (1 + exp(-(consCoefQ + (ideoCoefQ * ideo) + (ideoSqCoefQ * ideo * ideo) + (wcCoefQ * log(wc)))))
  }
  #create the likelihood ratios that describe whether the predicted values for these bins differ significantly from the 
  # predicted values for the bins on average
  linListName <- paste("likeRatioLin", langCat, sep="_")
  assign(linListName, linPredVals / mean(linPredVals))
  quadListName <- paste("likeRatioQuad", langCat, sep="_")
  assign(quadListName, quadPredVals / mean(quadPredVals))
}


####CREATE PLOTTER FUNCTION####
#Each fischers plot will contain:
  #dots associated with likelihood ratios for language use within that bin compared to the entire dataset
  #moving average of the dots (blue line)
  #linear approximation of the pattern from multi-level logistic regresions (red line)
  #quadratic approximation of the pattern from the multi-level logistic regresions (green line)
#pulls likelihood ratios from the linear regression models from syntax above
plotter <- function(dataset, wordCount, percent, variable, shortName) {
  sortedIdeoBins <- c(df_ideobins$mean_ideo[2:20], df_ideobins$mean_ideo[1])
  sortedLikeRLin <- c(eval(parse(text = paste("likeRatioLin", shortName, sep="_")))[2:20], eval(parse(text = paste("likeRatioLin", shortName, sep="_")))[1])
  sortedLikeRQuad <- c(eval(parse(text = paste("likeRatioQuad", shortName, sep="_")))[2:20], eval(parse(text = paste("likeRatioQuad", shortName, sep="_")))[1])
  pdf(paste(shortName, "_ideo.pdf", sep=""))
  plot(df_ideobins$mean_ideo, dataset$odds_ratio, main=paste("Odds Ratio of", variable, "\n Enrichment Liberal-Conservative Bins"), 
       xlab="Mean Ideology", ylab=paste(variable, "Enrichment"), pch=19, col=dataset$color, ylim=c(0,2))
  lines(lowess(df_ideobins$mean_ideo, dataset$odds_ratio), col="blue") # lowess line (x,y)
  lines(sortedIdeoBins, sortedLikeRLin, col="red") # lowess line (x,y)
  lines(sortedIdeoBins, sortedLikeRQuad, col="green") # lowess line (x,y)
  abline(h = 1, col = "gray", lty = 3, lwd = 3)
  text(0.7, 0.2, cex=1.5, paste(percent, "% of \n tweets"))
  dev.off()
}


####CYCLE THROUGH EACH LANGUAGE CATEGORY AND PRINT FISCHERS PLOT####
  #starts with csv file including datafile names, dataframe names, variable names, and formal varaible names
  #then calls plotter function
lang_cats <- read.csv("variable_list.csv")
for (i in 1:nrow(lang_cats)) {
  #create the variables needed for the analyses below (specific for each language category)
  row <- lang_cats[i, ]
  file <- as.character(row$filename)
  #load in the datafile for the specific language category
  load(file)
  dataset <- eval(parse(text = as.character(row$dataset)))
  variable <- as.character(row$variable)
  shortName <- as.character(row$short_name)
  wordCount <- as.character(row$total_count)
  prop <- twt_tots_df[[wordCount]]/twt_tots_df$total_twt
  percent <- 100 * round(prop, digits = 4)
  #color p-values based on significance
  dataset$color[dataset$p.vals>=.01]="grey"
  dataset$color[dataset$p.vals<.01]="black"
  #run plotter function
  plotter(dataset, wordCount, percent, variable, shortName)
}

