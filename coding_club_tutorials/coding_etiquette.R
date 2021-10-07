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
stewd("")


