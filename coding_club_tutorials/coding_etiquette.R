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

# Following coding syntax etiquette ----

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



# Spacing ----
# place spaces around all inflix operators (=,+,-,<-,etc.)
# always put space after comma
# : and :: don't need spaces

x <- 1:10  # Good
base::get  # Good
dplyr::select  # When you use `package_name::function_name` in your code like the example here, this means you are calling the function `select()` from the package `dplyr`
# this way of using functions works without having loaded the package beforehand using `library(dplyr)`, but it's not very commonly used, since it's longer.

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
# Here we are creating an imaginary object with a geographical projection commonly used for the UK

# no spaces before left parenthesis except in a function call
# Good
if (debug) do(x)
plot(x, y)
# Bad
if(debug)do(x)
plot (x, y)

# extra spacing is ok if it improves alignment of =/<-
LPI_biome_summ <- LPI_long %>%
  group_by(biome) %>%  # Group by biome
  summarise(populations = n(),   # Create columns, number of populations
            mean_study_length_years = mean(lengthyear),  # mean study length
            max_lat = max(decimal_latitude),  # max latitude
            min_lat = min(decimal_latitude),  # max longitude
            dominant_sampling_method = names(which.max(table(sampling_method))),  # modal sampling method
            dominant_units = names(which.max(table(units))))  # modal unit typ

# no spaces around code in parentheses/brackets (unless it's a comma)
# Good
if (debug) do(x)
diamonds[5, ]
# Bad
if ( debug ) do(x)  # No spaces around debug
x[1,]   # Needs a space after the comma
x[1 ,]  # Space goes after comma not before

# inline commenting -- place tw spaces after code followed by # and single space then text
# Calculating summary statistics for each biome in the Living Planet Index database
# No need to copy and run this code now, this just illustrates comments
LPI_biome_summ <- LPI2 %>%
  group_by(biome) %>%  # Group by biome
  summarise(populations = n(),   # Create columns, number of populations
            mean_study_length_years = mean(lengthyear),  # mean study length
            max_lat = max(decimal_latitude),  # max latitude
            min_lat = min(decimal_latitude),  # max longitude
            dominant_sampling_method = names(which.max(table(sampling_method))),  # modal sampling method
            dominant_units = names(which.max(table(units))))  # modal unit type


# Curly braces ----

# opening curly brace should never go on its own line
# should always be followed by a new line
# closing curly brace should always go on its own line, unless it’s followed by else
# always indent the code inside curly braces

# Good

if (y < 0 && debug) {
  message("Y is negative")
}

if (y == 0) {
  log(x)
} else {
  y ^ x
}

# Bad

if (y < 0 && debug)
{message("Y is negative")}

if (y == 0) {
  log(x)
}
else {
  y ^ x
}

# Line length ----

# convention = 80 characters per line 
# Tools/Global Options/Code/Display/Show Margin/80 characters
# when using pipes from dplyr package, keep pipe operator %>% at end of line

LPI_long <- LPI_long %>%
  group_by(., genus_species_id) %>%  # group rows so that each group is one population
  mutate(., maxyear = max(year), minyear = min(year)) %>%  # Create columns for the first and most recent years that data was collected
  mutate(., lengthyear = maxyear-minyear) %>%  # Create a column for the length of time data available
  mutate(., scalepop = (pop-min(pop))/(max(pop)-min(pop))) %>%  # Scale population trend data
  filter(., is.finite(scalepop)) %>%
  filter(., lengthyear > 5) %>%  # Only keep rows with more than 5 years of data
  ungroup(.)  # Remove any groupings you've greated in the pipe, not entirely necessary but it's better to be safe

# when using ggplot2, keep + at end of line 
(vulture_scatter <- ggplot(vultureITCR, aes (x = year, y = abundance, colour = Country.list)) +
    geom_point(size = 2) +                                                
    geom_smooth(method = lm, aes(fill = Country.list)) +                    
    theme_my_own() +
    scale_fill_manual(values = c("#EE7600", "#00868B")) +               
    scale_colour_manual(values = c("#EE7600", "#00868B"),               
                        labels = c("Croatia", "Italy")) +                 
    ylab("Griffon vulture abundance\n") +                             
    xlab("\nYear"))


