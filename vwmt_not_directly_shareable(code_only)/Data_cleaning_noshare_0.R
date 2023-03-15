# get ready----
set.seed(1234321)
# install.packages('pacman')
library(pacman)
pacman::p_load(pacman,dplyr,tidyr,plyr,tidyverse)

rm(list=ls()) 

workspace = getwd()
preFolder = paste(workspace, "pre", sep = "/")
postFolder = paste(workspace, "post", sep = "/")
trainingG1Folder = paste(workspace, "trainingG1", sep = "/")
trainingG2Folder = paste(workspace, "trainingG2", sep = "/")
cleanedPrePostFolder = paste(workspace,'cleanedPrePost',sep = "/")
cleanedTrainingFolder = paste(workspace,'cleanedTraining',sep = "/")
outputFolder = paste(workspace, "output", sep = "/")
# load and clean ----

#clear up
removePre <- list.files(preFolder,include.dirs = F,full.names = T, recursive = T)
file.remove(removePre)
removePost <- list.files(postFolder,include.dirs = F,full.names = T, recursive = T)
file.remove(removePost)
removeTraining <- list.files(trainingG1Folder,include.dirs = F,full.names = T, recursive = T)
file.remove(removeTraining)
removeTraining <- list.files(trainingG2Folder,include.dirs = F,full.names = T, recursive = T)
file.remove(removeTraining)

# unzip
inputFolder = paste(workspace, "rawData","post_s6", sep = "/")
postFiles <- list.files(path=inputFolder, pattern = "*.zip", full.names = TRUE)
llply(.data = postFiles, .fun = unzip, exdir = postFolder)

inputFolder = paste(workspace, "rawData","pre_s1", sep = "/")
preFiles <- list.files(path=inputFolder, pattern = "*.zip", full.names = TRUE)
llply(.data = preFiles, .fun = unzip, exdir = preFolder)

inputFolder = paste(workspace, "rawData","orient_training", sep = "/")
trainingG1Files <- list.files(path=inputFolder, pattern = "*.zip", full.names = TRUE)
llply(.data = trainingG1Files, .fun = unzip, exdir = trainingG1Folder)

inputFolder = paste(workspace, "rawData","visual_search_training", sep = "/")
trainingG2Files <- list.files(path=inputFolder, pattern = "*.zip", full.names = TRUE)
llply(.data = trainingG2Files, .fun = unzip, exdir = trainingG2Folder)

#read files # require reduceReplica.R
#warnings: incomplete final line found by readTableHeader on 'XXX'. Do not worry! This means these csv files do not end with an End Of Line (EOL) character AKA unfinished session with less than 5 lines
source('reduceReplica.R')

toreadFiles <- list.files(path = preFolder, pattern="*.csv", full.names = TRUE)
readFiles <- reduceReplica(toreadFiles)
pre <- ldply(.data = readFiles, .fun = read.csv,header = TRUE, sep=',', stringsAsFactors = FALSE)

toreadFiles <- list.files(path = postFolder, pattern="*.csv", full.names = TRUE)
readFiles <- reduceReplica(toreadFiles)
post <- ldply(.data = readFiles, .fun = read.csv,header = TRUE, sep=',', stringsAsFactors = FALSE)

toreadFiles <- list.files(path = trainingG1Folder, pattern="*.csv", full.names = TRUE)
readFiles <- reduceReplica(toreadFiles)
ortT <- ldply(.data = readFiles, .fun = read.csv,header = TRUE, sep=',', stringsAsFactors = FALSE)

toreadFiles <- list.files(path = trainingG2Folder, pattern="*.csv", full.names = TRUE)
readFiles <- reduceReplica(toreadFiles)
vsT <- ldply(.data = readFiles, .fun = read.csv,header = TRUE, sep=',', stringsAsFactors = FALSE)

source('goodName.R')
pre <- goodName(pre)
post <- goodName(post)
ortT <- goodName(ortT)
vsT <- goodName(vsT)

# to create a sharable raw-sh data, I replaced the extID by some vwmt-id from these dataframes
replace <- read.csv('groupInfo98withprolificID.csv', header = TRUE, sep = ',')
shareableRaw = paste(workspace, "shareableRaw", sep = "/")

demographics <- read.csv('Participants demographics(all).csv')
demographics_pre_exclusion <- merge(demographics, replace,by.x="participant_id", by.y="Row.names")
demographics_pre_exclusion <- demographics_pre_exclusion %>% subset(select = -c(participant_id))
names(demographics_pre_exclusion)[names(demographics_pre_exclusion) == "id"] <- "extId"
write.csv(demographics_pre_exclusion, file=paste(shareableRaw,'demographics_pre_exclusion.csv',sep = "/"))

PRE <- merge(pre, replace,by.y="Row.names", by.x="extId")
PRE <- PRE %>% subset(select = -c(extId))
names(PRE)[names(PRE) == "id"] <- "extId"
write.csv(PRE, file=paste(shareableRaw,'PRE.csv',sep = "/"))

POST <- merge(post, replace,by.y="Row.names", by.x="extId")
POST <- POST %>% subset(select = -c(extId))
names(POST)[names(POST) == "id"] <- "extId"
write.csv(POST, file=paste(shareableRaw,'POST.csv',sep = "/"))

ORTT <- merge(ortT, replace,by.y="Row.names", by.x="extId")
ORTT <- ORTT %>% subset(select = -c(extId))
names(ORTT)[names(ORTT) == "id"] <- "extId"
write.csv(ORTT, file=paste(shareableRaw,'ORTT.csv',sep = "/"))

VST <- merge(vsT, replace,by.y="Row.names", by.x="extId")
VST <- VST %>% subset(select = -c(extId))
names(VST)[names(VST) == "id"] <- "extId"
write.csv(VST, file=paste(shareableRaw,'VST.csv',sep = "/"))
