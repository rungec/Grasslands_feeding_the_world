#Grasslands feeding the world
#This script explores and summarises references from the WOS literature review
#Author: Claire Runge

setwd("D:/Box Sync/Projects/Other Projects/Ongoing/Grasslands_feedingtheworld/Analysis/Systematic review")

library(bib2df)
library(ggplot2)

###################
#LOAD DATA
###################

filelist <- list.files("180103/", pattern = 'WOS_2_core.*\\.bib', full.names=TRUE)

bibDF <-do.call(rbind, lapply(filelist, function(x) bib2df(x)))

test <- bib2df(filelist[1])
test1 <- bib2df(filelist[1])[,1:47]
head(test1$KEYWORDS)

#need to wait for the issue to be fixed in github before I can do anything else
https://cran.rstudio.com/web/packages/bib2df/vignettes/bib2df.html

###################
#PLOTS
###################