###CREATING THE DICTIONARIES FOR NEIMAN DICTIONARIES & LIWC###
setwd("~/Dropbox/SMaPP/Legislator Project/Longitudinal")

#first load all necessary modules and create lemmatization function
##FIRST NEED TO INSTALL QUANTEDA##
library(quanteda)

##import files and look at summary statas
##Input all of legislator tweets##
twtws <- read.csv('legislator_tweets.csv', header=TRUE, stringsAsFactors=FALSE)
leg_twts <- corpus(twtws, text_field = "text")
reads <- textstat_readability(leg_twts, "Flesch.Kincaid")
summary(reads)

##Input all of legislator facebook posts##
fbps <- read.csv('legislator_fbposts.csv', header=TRUE, stringsAsFactors=FALSE)
leg_fbps <- corpus(fbps, text_field = "text")
reads2 <- textstat_readability(leg_fbps, "Flesch.Kincaid")
summary(reads2)

##Input all of legislator twitter posts##
crsp <- read.csv('legislator_congspeech.csv', header=TRUE, stringsAsFactors=FALSE)
leg_crsp <- corpus(crsp, text_field = "text")
reads3 <- textstat_readability(leg_crsp, "Flesch.Kincaid")
summary(reads3)
