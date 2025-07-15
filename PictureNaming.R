# load necessary libraries
library(tidyverse)
library(plyr)
library(lmerTest)

# we need to tell R where to find everything by setting a working directory!
setwd("./logfiles")

# # have a first look at the data
# VP01 <- read_table("List1-he-log.txt") # read_table command for loading text files
# 
# View(VP01)
# glimpse(VP01)
# summary(VP01)
# 
# unique(VP01$Voicekey)
# 
# # have a first look at the data
# VP02 <- read_table("List1-HX-1-log.txt") # read_table command for loading text files
# 
# unique(VP02$Voicekey)
# nrow(VP02[VP02$Voicekey == -1,])/nrow(VP02)
# 
# VP03 <- read_table("List2-Jul-02-log.txt") 
# nrow(VP03)
# View(VP03)
# VP03 <- filter(VP03, Age == 22)
# nrow(VP03)
# View(VP03)
# nrow(VP03[VP03$Voicekey == -1,])/nrow(VP03)
# 
# VP03 <- mutate(VP03,
#                Error = 0) # mutate adds a new column, in this case: column called Error, all values 0
# View(VP03)
# VP03$Error[VP03$item == "violin_white.png"] <- 1 # too cumbersome, better do this outside R 

# Plan for July 2nd
# 1. Get all logfiles with error column in one folder (still waiting for Hanxin and Juliane)
# 2. Give these files a unique name (VP01 - VP10)
# 3. Bind all files to a big file in R
# We need to read in all the files, only VP04, VP05, VP06, VP07 loaded without problems
VP01 <- read_table("cleanedVP01.txt") # read_table command for loading text files
VP02 <- read_table("cleanedVP02.txt") # read_table command for loading text files
VP03 <- read_table("cleanedVP03.txt") # read_table command for loading text files
VP04 <- read_table("VP04.txt") # read_table command for loading text files

VP05 <- read_table("VP05.txt") # read_table command for loading text files
VP06 <- read_table("VP06.txt") # read_table command for loading text files
VP07 <- read_table("VP07.txt") # read_table command for loading text files
VP08 <- read_table("VP08.txt") # read_table command for loading text files

VP09 <- read_table("VP09.txt") # read_table command for loading text files
VP10 <- read_table("VP10.txt") # read_table command for loading text files
# VP11 <- read_table("VP11.txt") # read_table command for loading text files
# VP12 <- read_table("VP12.txt") # read_table command for loading text files
# VP13 <- read_table("VP13.txt") # read_table command for loading text files

head(VP10)

VP14 <- read_table("VP14.txt") # read_table command for loading text files
VP15 <- read_table("VP15.txt") # read_table command for loading text files
VP16 <- read_table("VP16.txt") # read_table command for loading text files
VP17 <- read_table("VP17.txt") # read_table command for loading text files

alldata <- rbind(VP01, VP02, VP03, VP04, VP05, VP06, VP07, VP08, VP14, VP15, VP16, VP17) # VP05 has strange column name
nrow(alldata)

# 4. Filter out all errors and voice key misses 
  cleandata <- filter(alldata,
                      Error == 0)
  View(cleandata)
  cleandata <- filter(cleandata,
                      Voicekey != -1) # != means 'does not equal'

# 5. Filter for control and critical
cleandata$category <- as.factor(cleandata$category)
levels(cleandata$category) # control, critical, filler

expdata <- filter(cleandata,
                  category != 'filler')
view(expdata)

#expdata <- filter(cleandata,
#                  category == 'control' || category == 'critical') # alternative way to get the same result as above

# 6. Compare descriptive RTs in a table (RT = reaction time)
expdata$category <- as.factor(as.character(expdata$category))
levels(expdata$category) # control, critical

tapply(expdata$Voicekey, expdata$category, mean)
tapply(expdata$Voicekey, expdata$category, sd)

# 7. Compare descriptive RTs in a bar graphs
figdata <- ddply(expdata, "category", summarise, RT = mean(Voicekey))
ggplot(figdata, aes(x=category, y=RT)) + geom_bar(stat="identity")

# compar RTS to different colors
figdata_color <- ddply(expdata, "color", summarise, RT = mean(Voicekey))
ggplot(figdata_color, aes(x=color, y=RT)) + geom_bar(stat="identity")

# 8. Do a statistical analysis of our data
# calculate a linear mixed effects model
# voicekey = dependent variable; category = independent variable
m <- lmer(Voicekey ~ category + (1|SubjID) + (1|item), data = expdata) 

summary(m)









