#Coding Club Tutorial: Coding Ettiquette 
#Ana Katarina; 07/10/2021
#University of Edinburgh
#Data available from http://www.livingplanetindex.org/

#Organising scripts into sections ----
#No strcit rules for numbers/names of section 
#However script should include:
# 1)Introduction: author statement; names; contact details; date
# 2)Libraries: packages used in the script
# 3)Defining functions section 
# 4)Setting the working directory
# 5)Importing data - what data are you using and where is it stored?
# 6)Logical flow - following the sections of a report for example

#Packages----
library(tidyr)  # Formatting data for analysis
library(dplyr)  # Manipulating data
library(ggplot2)  # Visualising results
library(readr)  # Manipulating data

#Functions----
#A custom ggplot2 function stored as an object theme.LPI
theme.LPI <- function(){
  theme_bw()+
    theme(axis.text.x=element_text(size=12, angle=45, vjust=1, hjust=1),
          axis.text.y=element_text(size=12),
          axis.title.x=element_text(size=14, face="plain"),             
          axis.title.y=element_text(size=14, face="plain"),             
          panel.grid.major.x=element_blank(),                                          
          panel.grid.minor.x=element_blank(),
          panel.grid.minor.y=element_blank(),
          panel.grid.major.y=element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size=20, vjust=1, hjust=0.5),
          legend.text = element_text(size=12, face="italic"),          
          legend.title = element_blank(),                              
          legend.position=c(0.9, 0.9))
}
#Set working directory (Windows)----
setwd("C:/Users/Ana/Documents/Edinburgh University/Courses 21-22/Data-Science/Coding-Club-GITHUB-zips/CC-etiquette-master")
#Importing data----
LPI <- read.csv("LPIdata_CC.csv")

#Formatting data----
LPI2 <- gather(LPI, "year", "abundance", 9:53)  # Transforming the data from wide to long format, some blank cells may disappear
# gather function requires tidyr package

LPI2$year <- parse_number(LPI2$year)  # Do you see awkward Xs before all the years? This gets rid of them.
names(LPI2)  # Check what the different variables are called
names(LPI2) <- tolower(names(LPI2))  # Make all variable names lower case

#use str()to check if the variables have stayed how we want them 
str(LPI2)
#Abundance is a character (chr) variable when it should be a numeric variable 
LPI2$abundance <- as.numeric(LPI2$abundance) #changes it to numeric variable

#Calculate summary stats for each biome in the LPI database ----
levels(LPI2$biome) #Generates a list of all biomes in LPI2
