#Grasslands feeding the world
#This script explores and summarises references from the WOS literature review
#Author: Claire Runge

setwd("D:/Box Sync/Projects/Other Projects/Ongoing/Grasslands_feedingtheworld/Analysis/Systematic review")

library(bib2df)
library(ggplot2)
library(dplyr)
library(stringr) #str_trim

###################
#LOAD DATA
###################

filelist <- list.files("180103/", pattern = 'WOS_2_core.*\\.bib', full.names=TRUE)

bibDF <-do.call(rbind, lapply(filelist, function(x) bib2df(x)))

#need to wait for the issue to be fixed in github before I can do anything else
https://cran.rstudio.com/web/packages/bib2df/vignettes/bib2df.html





###################
#Data summary
###################

test <- bib2df(filelist[1])
test1 <- bib2df(filelist[1])[,1:47]
test2 <- test1[!is.na(test1$CATEGORY),]
head(test2$KEYWORDS)
keywordlist <- lapply(test2$KEYWORDS, function(x){ 
                                splitx <- strsplit(x, ";") #each keyword item in list
                                splity <- lapply(splitx, function(y) tolower(str_trim(y))) #trim whitespace and convert to lowercase
                                return(unlist(splity))
                                })

#count keywords

keywordcount <- keywordlist %>% unlist() %>% as.factor() %>% table()

head(keywordcount)
write.csv(keywordcount, "keywordcount.csv")

