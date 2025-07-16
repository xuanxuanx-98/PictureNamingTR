# load necessary libraries
library(tidyverse)
library(plyr)
library(lmerTest)

# we need to tell R where to find everything by setting a working directory!
setwd("./Logfiles")

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
VP09 <- read_table("cleanedVP09.txt") # read_table command for loading text files # problem
VP10 <- read_table("cleanedVP10.txt") # read_table command for loading text files
VP10 <- VP10[,2:12]
VP11 <- read_table("cleanedVP11.txt") # read_table command for loading text files # problem
VP12 <- read_table("cleanedVP12.txt") # read_table command for loading text files # problem
VP13 <- read_table("cleanedVP13.txt") # read_table command for loading text files # problem
VP14 <- read_table("VP14.txt") # read_table command for loading text files
VP15 <- read_table("VP15.txt") # read_table command for loading text files
VP16 <- read_table("VP16.txt") # read_table command for loading text files
VP17 <- read_table("VP17.txt") # read_table command for loading text files

alldata <- rbind(VP01, VP02, VP03, VP04, VP05, VP06, VP07, VP08, VP09, VP10, VP11, VP12, VP13, VP14, VP15, VP16, VP17) 
# alldata <- rbind(VP09, VP10, VP11, VP12, VP13) 
View(alldata)

# 4. Filter out all errors and voice key misses 
cleandata <- filter(alldata,
                    Error == 0)
View(cleandata)
cleandata <- filter(cleandata,
                    Voicekey != -1) # != means 'does not equal'

# 5. Filter for control and critical
cleandata$category <- as.factor(cleandata$category)
levels(cleandata$category) # filler, critical, category

expdata <- filter(cleandata,
                  category != 'filler')
View(expdata)

#expdata <- filter(cleandata,
#                  category == 'control' || category == 'critical') # alternative way to get the same result as above

# 6. Compare descriptive RTs (means) in a table (RT = reaction time)
expdata$category <- as.factor(as.character(expdata$category))
levels(expdata$category)
tapply(expdata$Voicekey, expdata$category, mean) # tapply = table apply, makes a table

# 7. Compare descriptive RTs in a bar graphs
figdata <- ddply(expdata, "category", summarise, RT = mean(Voicekey))
ggplot(figdata, aes(x=category, y=RT)) + geom_bar(stat="identity")

# Compar RTS according to different colors
figdata_color <- ddply(expdata, "color", summarise, RT = mean(Voicekey))
ggplot(figdata_color, aes(x=color, y=RT)) + geom_bar(stat="identity")

# 8. do a statistical analysis of our data
# calculate a linear mixed effects model
m <- lmer(Voicekey ~ category + (1|SubjID) + (1|item), data = expdata) # lmer Befehl um Modell zu rechnen, abhängige Variable vor der Tilde, unabhängige Variable nach der Tilde, (|) Zufallsvariablen, data = der Datensatz, auf dem das Modell gerechnet werden soll
summary(m) # Modell anschauen, p-value: möglichst unter 5%, besagt "wie wahrscheinlich, dass meine Prediction falsch ist"








