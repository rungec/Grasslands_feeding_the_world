#Grasslands feeding the world
#This script plots data from the WOS literature review
#Author: Claire Runge

setwd("D:/Box Sync/Projects/Other Projects/Ongoing/Grasslands_feedingtheworld/Analysis/Systematic review")

library(ggplot2)

###################
#LOAD DATA
###################

lucDF <- read.table("171201/WOS_report_48_LUCetc_by_year.txt", sep="\t", header=TRUE, nrows=29, col.names=c("Year", "Records", "Percent"))
lucDF_grass <- read.table("171201/WOS_report_49_LUCetc_grassshrubsavannah_by_year.txt", sep="\t", header=TRUE, nrows=23, col.names=c("Year", "Records", "Percent"))
lucDF_forest <- read.table("171201/WOS_report_50_LUCetc_forest_by_year.txt", sep="\t", header=TRUE, nrows=26, col.names=c("Year", "Records", "Percent"))

###################
#PLOTS
###################

#Plot number of records by year and biome
p <- ggplot(lucDF, aes(Year, Records)) +
  geom_line(col='grey70', size=1)+
  geom_line(data=lucDF_grass, aes(Year, Records), col='sienna1', size=1) +
  geom_line(data=lucDF_forest, aes(Year, Records), col='turquoise3', size=1) +
  geom_text(x=2010, y=250, label="all records", col="grey70", hjust=0, vjust=0) +
  geom_text(x=2012, y=150, label="forest", col='turquoise3', hjust=0, vjust=0) +
  geom_text(x=2010, y=50, label="grassland", col='sienna1', hjust=0, vjust=0) +
  geom_hline(yintercept=0, col='black') +
  theme_minimal()
ggsave("Grasslands_litreview_wos48_byyear_andbiome.pdf", p, width=8, height=6)
ggsave("Grasslands_litreview_wos48_byyear_andbiome.png", p, width=8, height=6)






