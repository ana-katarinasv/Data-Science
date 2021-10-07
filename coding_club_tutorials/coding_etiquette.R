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
# 5)Importing data: what data are you using and where is it stored?
# 6)Logical flow: following the sections of a report for example
# 7)Outputs of analysis (figures you want saved)
#     - save a .pdf and .png file of every graph
#     - good practice -- save images as subdirectory of working directory


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

LPI_biome_summ <- LPI2 %>% #use of pipe operator
  group_by(biome) %>%  #group by biome
  summarise(populations = n())  # create columns, number of populations

#Visualising the number of populations in each biome with ggplot2 package----
(barplot <- ggplot(LPI_biome_summ, aes(biome, color = biome, y = populations))
 + geom_bar(stat="identity")+
   theme.LPI()+ #Use of personal theme function
   ylab("Number of populations")+
   xlab("Biome")+
   theme(legend.position = "none"))#Removal of legend for simplicity
#putting entire ggplot code in brackets creates the graph and then shows it in the plot viewer
#without brackets you've created the object without visualising it 

#Outputs of analysis----
png(file="C:/Users/Ana/Documents/Edinburgh University/Courses 21-22/Data-Science/R-WD/Data-Science/coding_club_tutorials/img/biome_pop.png", width = 1000, height = 2000)  # Note that png() uses pixel values for width and height
ggplot(LPI_biome_summ, aes(biome, color = biome, y = populations)) + geom_bar(stat = "identity") +
  theme.LPI() +
  ylab("Number of populations") +
  xlab("Biome") +
  theme(legend.position = "none")
dev.off()  # This tells R you are done with the plotting and it can save the file

pdf(file="C:/Users/Ana/Documents/Edinburgh University/Courses 21-22/Data-Science/R-WD/Data-Science/coding_club_tutorials/img/biome_pop.pdf",  width = 13.33, height = 26.66)  # pdf() uses inches
ggplot(LPI_biome_summ, aes(biome, color = biome, y = populations)) + geom_bar(stat = "identity") +
  theme.LPI() +
  ylab("Number of populations") +
  xlab("Biome") +
  theme(legend.position = "none")
dev.off()

#Following coding syntax etiquette ----

#object names should be concise and meaningful
#object,function, variable names should be lowercase
#avoid spaces in file names
#variable names should be nouns 
#function names should be verbs 
#use underscore to separate words in file names/object/variables
#use dots to spearate function words

# Object names
avg_clicks  # Good.
avg.clicks  # Acceptable.
avg_Clicks  # Not okay.

# Function names
calculate.avg.clicks  # This is what we are aiming for.
CalculateAvgClicks  # Not that bad, but mixing capital and lowercase letters can lead to typos
calculate_avg_clicks 
calculateAvgClicks  # Bad. The convention is that functions are defined using dots, not underscores.

