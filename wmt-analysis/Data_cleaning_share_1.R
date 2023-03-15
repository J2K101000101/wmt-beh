# Clean the data to get ready for FitIntoModel_2.m with MATLAB, then run Analysis_3.R with R


# get ready----
set.seed(1234321)
# install.packages('pacman')
library(pacman)
pacman::p_load(pacman,dplyr,tidyr,plyr,tidyverse)

rm(list=ls()) 

strictToken<- read.csv('strictToken.csv')
keep <- strictToken$V1
workspace = getwd()
cleanedPrePostFolder = paste(workspace,'cleanedPrePost',sep = "/")
cleanedTrainingFolder = paste(workspace,'cleanedTraining',sep = "/")
outputFolder = paste(workspace, "output", sep = "/")
shareableRaw = paste(workspace, "shareableRaw", sep = "/")

#clear up
removePrePost <- list.files(cleanedPrePostFolder,include.dirs = F,full.names = T, recursive = T)
file.remove(removePrePost)
removeTraining <- list.files(cleanedTrainingFolder,include.dirs = F,full.names = T, recursive = T)
file.remove(removeTraining)
removeOutput <- list.files(outputFolder,include.dirs = F,full.names = T, recursive = T)
file.remove(removeOutput)

post <- read.csv (paste(shareableRaw, 'POST.csv',sep='/'), header = TRUE, sep=',', stringsAsFactors = FALSE) 
post <- post[post$sessionToken %in% keep, ]
pre<- read.csv (paste(shareableRaw, 'PRE.csv',sep='/'), header = TRUE, sep=',', stringsAsFactors = FALSE) 
pre <- pre[pre$sessionToken %in% keep, ]
ortT<- read.csv (paste(shareableRaw, 'ORTT.csv',sep='/'), header = TRUE, sep=',', stringsAsFactors = FALSE) 
ortT <- ortT[ortT$sessionToken %in% keep, ]
vsT<- read.csv (paste(shareableRaw, 'VST.csv',sep='/'), header = TRUE, sep=',', stringsAsFactors = FALSE)
vsT <- vsT[vsT$sessionToken %in% keep, ]
#check whether it's the unique extID And normal trialID (1~560/360),check the missing prePost, missing training data
post <- post %>% 
  cbind(check1 = grepl('^vwmt',post$extId)) %>% 
  cbind(check2 = nchar(post$extId)) %>% 
  subset(check1 == 'TRUE' & check2 == 7) %>% 
  subset(trialId >= 1 & trialId <= 560)
pre <- pre %>% 
  cbind(check1 = grepl('^vwmt',pre$extId)) %>% 
  cbind(check2 = nchar(pre$extId)) %>% 
  subset(check1 == 'TRUE' & check2 == 7) %>% 
  subset(trialId >= 1 & trialId <= 560) 

if (length(unique(post$extId))>length(unique(pre$extId))){
missingPrePost <- unique(post$extId)[!(unique(post$extId) %in% unique(pre$extId))]
inPrePost <- unique(post$extId)[!(unique(post$extId) %in% missingPrePost)]
}else {
  missingPrePost <- unique(pre$extId)[!(unique(pre$extId) %in% unique(post$extId))]
  inPrePost <- unique(pre$extId)[!(unique(pre$extId) %in% missingPrePost)]
            }

ortT <- ortT %>% 
  cbind(check1 = grepl('^vwmt',ortT$extId)) %>% 
  cbind(check2 = nchar(ortT$extId)) %>% 
  subset(check1 == 'TRUE' & check2 == 7) %>% 
  subset(trialId >= 1 & trialId <= 360)
vsT <- vsT %>% 
  cbind(check1 = grepl('^vwmt',vsT$extId)) %>% 
  cbind(check2 = nchar(vsT$extId)) %>% 
  subset(check1 == 'TRUE' & check2 == 7)%>% 
  subset(trialId >= 1 & trialId <= 360)
#!diagnostics off

## PRE and POST 
rowList <- c()
#ORT####
postORT <- post[grep('^tatoolContinuousOrientation_\\d',post$executableId),] 
preORT <- pre[grep('^tatoolContinuousOrientation_\\d',pre$executableId),] 


postORT <- postORT %>% 
  mutate(delta = as.numeric(postORT$givenResponse)  - as.numeric(postORT$probeAngle))
for(i in 1:length(postORT$delta)){
  if (postORT$delta[i] < -180){
    postORT$deltaAngle[i] = postORT$delta[i] + 360
  }else if(postORT$delta[i] > 180){
    postORT$deltaAngle[i] = postORT$delta[i] - 360
  }else{
    postORT$deltaAngle[i]=postORT$delta[i]
  }
}
preORT <- preORT %>% 
  mutate(delta = as.numeric(preORT$givenResponse)  - as.numeric(preORT$probeAngle))
for(i in 1:length(preORT$delta)){
  if (preORT$delta[i] < -180){
    preORT$deltaAngle[i] = preORT$delta[i] + 360
  }else if(preORT$delta[i] > 180){
    preORT$deltaAngle[i] = preORT$delta[i] - 360
  }else{
    preORT$deltaAngle[i]=preORT$delta[i]
  }
}


by_participant <- postORT %>% 
  group_by(extId) %>% 
  ungroup()%>% 
  select(extId,deltaAngle,reactionTime)
  
t <- split(by_participant, by_participant$extId)

ortPost <- data.frame()
#excluded data: Orientation and shape reproduction tasks: one-third of trials’ reaction time is shorter than 1500 ms
excludedOrtPost <- matrix('the list of exluded ID in ortPost')
rowList <- NA
for (i in 1:length(t)){
  N = 0
  for (j in 1: length(t[[i]]$reactionTime)){
    if (t[[i]]$reactionTime[j] < 1500){
      N = N+1
    }
  }
  
  if (N >= length(t[[i]]$reactionTime)/3){
    #ortPost <- ortPost #*if need to apply the strict exclusion criteria to exclude the data right now
    excludedOrtPost <- rbind(excludedOrtPost ,unique(t[[i]]$extId))
  }#*else{
    ortPost <- rbind(ortPost, t[[i]]$deltaAngle) 
    rowList[i] <- unique(t[[i]]$extId)
  #*}
}
row.names(ortPost) <- na.omit(rowList)
colnames(ortPost) <- c(1:120)


# !diagnostics off
by_participant <- preORT %>% 
  group_by(extId) %>% 
  ungroup()%>% 
  select(extId,deltaAngle,reactionTime)
  
t <- split(by_participant, by_participant$extId)

ortPre <- data.frame()
#excluded data: Orientation and shape reproduction tasks: one-third of trials’ reaction time is shorter than 1500 ms
#excluded data: Orientation and shape reproduction tasks: one-third of trials’ reaction time is shorter than 1500 ms
excludedOrtPre <- matrix('the list of exluded ID in ortPre')
rowList <- NA
# !diagnostics off
for (i in 1:length(t)){
  N = 0
  for (j in 1: length(t[[i]]$reactionTime)){
    if (t[[i]]$reactionTime[j] < 1500){
      N = N+1
    }
  }
  
  if (N >= length(t[[i]]$reactionTime)/3){
    ortPre <- ortPre
    excludedOrtPre <- rbind(excludedOrtPre ,unique(t[[i]]$extId))
  }#*else{
    ortPre <- rbind(ortPre, t[[i]]$deltaAngle) 
    rowList[i] <- unique(t[[i]]$extId)
 #* }
}
row.names(ortPre) <- na.omit(rowList)
colnames(ortPre) <- c(1:120)

#SRT####
postSRT <- post[grep('^tatoolShapeReproduction_b\\d',post$executableId),] 
preSRT <- pre[grep('^tatoolShapeReproduction_b\\d',pre$executableId),] 

# Angle changes [-180,180]
postSRT <- postSRT %>% 
  mutate(delta = as.numeric(postSRT$givenResponse)  - as.numeric(postSRT$probeAngle))
for(i in 1:length(postSRT$delta)){
  if (postSRT$delta[i] < -180){
    postSRT$deltaAngle[i] = postSRT$delta[i] + 360
  }else if(postSRT$delta[i] > 180){
    postSRT$deltaAngle[i] = postSRT$delta[i] - 360
  }else{
    postSRT$deltaAngle[i]=postSRT$delta[i]
  }
}
preSRT <- preSRT %>% 
  mutate(delta = as.numeric(preSRT$givenResponse)  - as.numeric(preSRT$probeAngle))
for(i in 1:length(preSRT$delta)){
  if (preSRT$delta[i] < -180){
    preSRT$deltaAngle[i] = preSRT$delta[i] + 360
  }else if(preSRT$delta[i] > 180){
    preSRT$deltaAngle[i] = preSRT$delta[i] - 360
  }else{
    preSRT$deltaAngle[i]=preSRT$delta[i]
  }
}

by_participant <- postSRT %>% 
  group_by(extId) %>% 
  ungroup()%>% 
  select(extId,deltaAngle,reactionTime) 
t <- split(by_participant, by_participant$extId)

srtPost <- data.frame()
#excluded data: Orientation and shape reproduction tasks: one-third of trials’ reaction time is shorter than 1500 ms
rowList <- NA
excludedSrtPost <- matrix('the list of exluded ID in srtPost')
for (i in 1:length(t)){
  N = 0
  for (j in 1: length(t[[i]]$reactionTime)){
    if (t[[i]]$reactionTime[j] < 1500){
      N = N+1
    }
  }
  
  if (N >= length(t[[i]]$reactionTime)/3){
    #srtPost <- srtPost
    excludedSrtPost <- rbind(excludedSrtPost ,unique(t[[i]]$extId))
  }#*else{
    srtPost <- rbind(srtPost, t[[i]]$deltaAngle) 
    rowList[i] <- unique(t[[i]]$extId)
  #•}
}
row.names(srtPost) <- na.omit(rowList)
colnames(srtPost) <- c(1:120)

by_participant <- preSRT %>% 
  group_by(extId) %>% 
  ungroup()%>% 
  select(extId,deltaAngle,reactionTime)
t <- split(by_participant, by_participant$extId)

srtPre <- data.frame()

#excluded data: Orientation and shape reproduction tasks: one-third of trials’ reaction time is shorter than 1500 ms
rowList <- NA
excludedSrtPre <- matrix('the list of exluded ID in srtPre')
for (i in 1:length(t)){
  N = 0
  for (j in 1: length(t[[i]]$reactionTime)){
    if (t[[i]]$reactionTime[j] < 1500){
      N = N+1
    }
  }
  
  if (N >= length(t[[i]]$reactionTime)/3){
    srtPre <- srtPre
    excludedSrtPre <- rbind(excludedSrtPre ,unique(t[[i]]$extId))
  }#•else{
    srtPre <- rbind(srtPre, t[[i]]$deltaAngle) 
    rowList[i] <- unique(t[[i]]$extId)
  #•}
}
row.names(srtPre) <- na.omit(rowList)
colnames(srtPre) <- c(1:120)


#ODT####
postODT <- post[grep('^tatoolOrientationDetection_\\d',post$executableId),]
preODT <- pre[grep('^tatoolOrientationDetection_\\d',pre$executableId),]

by_participant <- postODT %>%
  group_by(extId) %>%
  ungroup()%>%
  select(extId,detection, givenResponse,score,reactionTime)
t <- split(by_participant, by_participant$extId)

nH <- 0
nM <- 0
nFA <- 0
nCR <- 0
H <- 0
FA <- 0

#excluded data: Orientation change detection task: one-third of trials’ reaction time is shorter than 300 ms
rowList <- NA
excludedOdtPost <- matrix('the list of exluded ID in odtPost ')
for(i in 1:length(t)){
  N=0
  for (j in 1: length(t[[i]]$reactionTime)){
    if (t[[i]]$reactionTime[j] < 300){
      N = N+1
    }
  }

  if (N >= length(t[[i]]$reactionTime)/3){
    excludedOdtPost <- rbind(excludedOdtPost ,unique(t[[i]]$extId))
  }

  hits <- t[[i]] %>% filter(score == 1 & detection == 1 & givenResponse == 1)
  misses <- t[[i]] %>% filter(score == 0 & detection == 1 & givenResponse == 0)
  falseAlarms <- t[[i]] %>% filter(score == 0 & detection == 0 & givenResponse == 1)
  correctRejections <- t[[i]] %>% filter(score == 1 & detection == 0 & givenResponse == 0)
  nH[i] <- nrow(hits)
  nM[i] <- nrow(misses)
  nFA[i] <- nrow(falseAlarms)
  nCR[i] <- nrow(correctRejections)
  H[i] <- nH[i]/(nH[i]+nM[i])
  FA[i] <- nFA[i]/(nFA[i]+nCR[i])
  rowList[i] <- unique(t[[i]]$extId)

}
odtPost <- data.frame(nH,nM,nFA,nCR,H,FA)
colnames(odtPost) <- c('nH','nM','nFA','nCR','H','FA')
odtPost <- mutate(odtPost, K=4*(H-FA)/(1-FA)) # only set size 4 at prepost
row.names(odtPost) <- na.omit(rowList)

by_participant <- preODT %>%
  group_by(extId) %>%
  ungroup()%>%
  select(extId,detection, givenResponse,score,reactionTime)
t <- split(by_participant, by_participant$extId)


nH <- 0
nM <- 0
nFA <- 0
nCR <- 0
H <- 0
FA <- 0

#excluded data: Orientation change detection task: one-third of trials’ reaction time is shorter than 300 ms
rowList <- NA
excludedOdtPre <- matrix('the list of exluded ID in odtPre ')
for(i in 1:length(t)){
  N=0
  for (j in 1: length(t[[i]]$reactionTime)){
    if (t[[i]]$reactionTime[j] < 300){
      N = N+1
    }
  }
  
  if (N >= length(t[[i]]$reactionTime)/3){
    excludedOdtPre <- rbind(excludedOdtPre ,unique(t[[i]]$extId))
  }

  hits <- t[[i]] %>% filter(score == 1 & detection == 1 & givenResponse == 1)
  misses <- t[[i]] %>% filter(score == 0 & detection == 1 & givenResponse == 0)
  falseAlarms <- t[[i]] %>% filter(score == 0 & detection == 0 & givenResponse == 1)
  correctRejections <- t[[i]] %>% filter(score == 1 & detection == 0 & givenResponse == 0)
  nH[i] <- nrow(hits)
  nM[i] <- nrow(misses)
  nFA[i] <- nrow(falseAlarms)
  nCR[i] <- nrow(correctRejections)
  H[i] <- nH[i]/(nH[i]+nM[i])
  FA[i] <- nFA[i]/(nFA[i]+nCR[i])
  rowList[i] <- unique(t[[i]]$extId)

}
odtPre <- data.frame(nH,nM,nFA,nCR,H,FA)
colnames(odtPre) <- c('nH','nM','nFA','nCR','H','FA')
odtPre <- mutate(odtPre, K=4*(H-FA)/(1-FA))
row.names(odtPre) <- na.omit(rowList)


#visual search####
postVS <- post[grep('^tatoolVisualSearch_\\d',post$executableId),] 
preVS <- pre[grep('^tatoolVisualSearch_\\d',pre$executableId),] 
#post
by_participant <- postVS %>% 
  group_by(extId) %>% 
  ungroup()%>% 
  select(extId,score,reactionTime,givenResponse) 
t <- split(by_participant, by_participant$extId)

#excluded data: Visual search task: one-third of trials’ reaction time is shorter than 300 ms or one-third of trials’ responses are omissions (participants failed to respond within 5 s). 
meanRT <- vector()
acc <- vector()
excludedVsPost <- matrix('the list of exluded ID in vsPost ')
rowList <- NA
for(i in 1:length(t)){
  N=0
  for (j in 1: length(t[[i]]$reactionTime)){
    if (t[[i]]$reactionTime[j] < 300){
      N = N+1
    }
  }
  n=0
  for (j in 1: length(t[[i]]$givenResponse)){
    if (t[[i]]$givenResponse[j] == 'omission'){
      n = n+1
    }
  }
  
  if (N >= length(t[[i]]$reactionTime)/3||n>=length(t[[i]]$givenResponse)/3){
    excludedVsPost <- rbind(excludedVsPost ,unique(t[[i]]$extId))
  }
  
  acc[i] <- nrow(t[[i]] %>% filter(score == 1))/(nrow(t[[i]] %>% filter(score == 1))+nrow(t[[i]] %>% filter(score == 0)))
  temp <- t[[i]] %>% filter(givenResponse != 'omission') %>% select(reactionTime) 
  meanRT[i] <- mean(temp$reactionTime)
 
  rowList[i] <- unique(t[[i]]$extId)
}
vsPost <- data.frame(acc,meanRT)
row.names(vsPost) <- na.omit(rowList)

#pre
by_participant <- preVS %>% 
  group_by(extId) %>% 
  ungroup()%>% 
  select(extId,score,reactionTime,givenResponse) 
t <- split(by_participant, by_participant$extId)

#excluded data: Visual search task: one-third of trials’ reaction time is shorter than 300 ms or one-third of trials’ responses are omissions (participants failed to respond within 5 s). 
meanRT <- vector()
acc <- vector()
excludedVsPre <-  matrix('the list of exluded ID in vsPre ')
rowList <- NA
for(i in 1:length(t)){
  N=0
  for (j in 1: length(t[[i]]$reactionTime)){
    if (t[[i]]$reactionTime[j] < 300){
      N = N+1
    }
  }
  n=0
  for (j in 1: length(t[[i]]$givenResponse)){
    if (t[[i]]$givenResponse[j] == 'omission'){
      n = n+1
    }
  }
  
  if (N >= length(t[[i]]$reactionTime)/3||n>=length(t[[i]]$givenResponse)/3){
    excludedVsPre <- rbind(excludedVsPre ,unique(t[[i]]$extId))
  }
  
  acc[i] <- nrow(t[[i]] %>% filter(score == 1))/(nrow(t[[i]] %>% filter(score == 1))+nrow(t[[i]] %>% filter(score == 0)))
  temp <- t[[i]] %>% filter(givenResponse != 'omission') %>% select(reactionTime) 
  meanRT[i] <- mean(temp$reactionTime)
  
  rowList[i] <- unique(t[[i]]$extId)
}
vsPre <- data.frame(acc,meanRT)
row.names(vsPre) <- na.omit(rowList)


## training sessions
# ortTraining####
rowList <- c()
# the experiment trials only (in case)
ortT <- ortT[grep('^tatoolContinuousOrientation_b\\d',ortT$executableId),] 
excludedORTT <- matrix('the list of exluded ID in ortTraining')
missingORTT <- matrix('the list of ID has less than 4 completed session in ortTraining')
check <- ortT %>% group_by(extId)
check <- split(check,check$extId)
requiredTrials <- 360-12# changeable according to the design (Here 360-30)

for (i in 1:length(check)){
  
  participant = names(check)[i]
  checkByToken <- group_by(check[[i]], check[[i]]$sessionToken)
  checkByToken <- split(check[[i]], check[[i]]$sessionToken)
  
  excludedToken <- c()
  for (n in 1:length(checkByToken)){
    token <- names(checkByToken)[n]
    if (nrow(checkByToken[[n]]) < requiredTrials){ # deleted those with less trials than the required as imcompleted session(s)
      excludedToken <- cbind(excludedToken,token)
      ortT <- ortT %>% 
        subset(sessionToken!= token)
    }
  }
  excludedToken <-  as.character(excludedToken)
  checkByToken1 <- checkByToken[names(checkByToken)%in%excludedToken==FALSE]
  
 excludedToken1 <- c()
 for(k in 1:length(checkByToken1) ){
   N = 0
    token <- names(checkByToken1)[k]
    for (j in 1: length(checkByToken1[[k]]$reactionTime)){
      if (checkByToken1[[k]]$reactionTime[j] < 1500){#excluded data: Orientation and shape reproduction tasks: one-third of trials’ reaction time is shorter than 1500 ms
        N = N+1
      }
    }
    if (N >= length(checkByToken1[[k]]$reactionTime)/3){
      excludedToken1 <- cbind(excludedToken1,token)
      excludedORTT <- rbind(excludedORTT, participant)
    }
  }
 excludedToken1 <-  as.character(excludedToken1)
 checkByToken2 <- checkByToken1 #* excluded list for checking only, willl be included in the missing check


  L= length(checkByToken2)
  if (L < 4){ # have less than four completed training sessions: missing session in the study, so this participant's data should be excluded
    missingORTT <- rbind(missingORTT, participant)
    ortT <- ortT %>% 
      subset(extId!= participant)

  }
}

ortT <- ortT %>% 
  mutate(delta = as.numeric(ortT$givenResponse)  - as.numeric(ortT$probeAngle))
for(i in 1:length(ortT$delta)){
  if (ortT$delta[i] < -180){
    ortT$deltaAngle[i] = ortT$delta[i] + 360
  }else if(ortT$delta[i] > 180){
    ortT$deltaAngle[i] = ortT$delta[i] - 360
  }else{
    ortT$deltaAngle[i]=ortT$delta[i]
  }
}

# Giving session numbers if more than one userCode
ortT$sessionOrder <- -9999
by_participant <- ortT %>% 
  group_by(extId,sessionToken) 
t <- split(by_participant, by_participant$extId)
#save <- ortT
for(i in 1:length(t)){
  thisID <- names(t)[i]
  print(thisID)
  hasTokens <- unique(t[[thisID]]$sessionToken)
  tempdf <- t[[thisID]]
  hasSessionId <- unique(tempdf$sessionId)
  hasUserId <- unique(tempdf$userCode)
  if (length(hasUserId) >1 | length(hasSessionId) < 4){ # for different userCode completed by the same session to identify the sessionId
    for(k in 1: length( unique(hasUserId))){
      ortT$sessionId[ortT$userCode== hasUserId[rank(hasUserId)==k]] <- ortT$sessionId[ortT$userCode== hasUserId[rank(hasUserId)==k]]+1000*k
    }
  } # else if(hasUserId ==1 && length(hasSessionId) > 4){} # has been checked by pre-checking 
  
  sessionUniqueList <- unique(ortT$sessionId[ortT$extId== thisID])
  for(u in 1: length(sessionUniqueList)){
    ortT$sessionOrder[ortT$extId== thisID & ortT$sessionId == sessionUniqueList[u]]<- rank(sessionUniqueList)[u]
  }
 }



#by setsize
by_setsize <- ortT %>% 
  group_by(extId,setSize) %>% 
  ungroup()%>% 
  select(userCode,extId,setSize,deltaAngle,sessionToken,sessionOrder) 
by_setsize <- by_setsize %>% 
  split(by_setsize$setSize) 


# in each set size, by participant, by session
ortTraining <- data.frame()
rowList <-vector() 

for (i in 1: length(by_setsize)){
  t <- by_setsize[[i]] %>% 
  group_by(extId,sessionToken,sessionOrder) 
  t <-split(t,t$extId) 
    for (j in 1:length(t)){
      temp <- t[[j]]  %>% 
        group_by(sessionToken) 
      temp <- split(temp,temp$sessionToken)
     
        for (n in 1:length(temp)){
          extId <- unique(temp[[n]]$extId)
          setsize <- unique(temp[[n]]$setSize)
          thisToken <- unique(temp[[n]]$sessionToken)
          T <- unique(temp[[n]]$sessionOrder)
          ortTraining <- rbind(ortTraining, temp[[thisToken]][["deltaAngle"]]) 
          k <- nrow(ortTraining)
          ortTraining$type[k] <- paste(extId, 'T', T,'setsize',setsize,sep = '_')
        }
      }
  }
rowlist <- ortTraining$type
ortTraining <- ortTraining %>% select(-type)
row.names(ortTraining) <-  rowlist
colnames(ortTraining) <- c(1:120)



#visualSearchTraining####
rowList <- c()
vsT <- vsT[grep('^tatoolVisualSearch_b\\d',vsT$executableId),] 
excludedVST <- matrix('the list of exluded ID in vsTraining')
missingVST <- matrix('the list of ID has less than four completed sessions in vsTraining')
check <- vsT %>% group_by(extId)
check <- split(check,check$extId)
requiredTrials <- 360-12 # changeable according to the design (Here 360-30)
# Data exclusion
for (i in 1:length(check)){
  participant = names(check)[i]
  #print(participant)
  checkByToken <- group_by(check[[i]], check[[i]]$sessionToken)
  checkByToken <- split(check[[i]], check[[i]]$sessionToken)
  
  excludedToken <- c()
  for (n in 1:length(checkByToken)){
    token <- names(checkByToken)[n]
    if (nrow(checkByToken[[n]]) < requiredTrials){ # deleted those with less trials than the required as imcompleted session(s)
      excludedToken <- cbind(excludedToken,token)
      vsT <- vsT %>% 
        subset(sessionToken!= token)
    }
  }
  excludedToken <-  as.character(excludedToken)
  checkByToken1 <- checkByToken[names(checkByToken)%in%excludedToken==FALSE]
  
  excludedToken1 <- c()
  for(k in 1:length(checkByToken1) ){
    # print('which checkByToken is checking:')
    # print(k) 
    N = 0
    token <- names(checkByToken1)[k]
    for (j in 1: length(checkByToken1[[k]]$reactionTime)){
      if (checkByToken1[[k]]$reactionTime[j] < 300){#excluded data: one-third of trials’ reaction time is shorter than 300 ms or one-third of trials’ responses are omissions (participants failed to respond within 5 s)
        N = N+1
      }
    }
    
    n=0
    for (j in 1: length(checkByToken1[[k]]$givenResponse)){
      if (checkByToken1[[k]]$givenResponse[j] == 'omission'){
        n = n+1
      }
    }
    
    if (N >= length(checkByToken1[[k]]$reactionTime)/3||n >= length(checkByToken1[[k]]$givenResponse)/3){
      excludedToken1 <- cbind(excludedToken1,token)
      excludedVST <- rbind(excludedVST,participant)
    }
  }
  excludedToken1 <-  as.character(excludedToken1)
  checkByToken2 <- checkByToken1 #* excluded list for checking only, willl be included in the missing check
  
  L= length(checkByToken2)
  No <- c(1:L)
  trialNo <- c()
  
  if (L < 4){ # have less than four completed training sessions: missing session in the study, so this participant's data should be excluded
    missingVST <- rbind(missingVST, participant)
    vsT <- vsT %>% 
      subset(extId!= participant)
    print('deleted the data about this participant because of missing sessions')
  }else if (L > 4){ # have extra sessions,  take four sessions having closer to 360 trials /session
    # print('more than 4 that I need')
    for (m in 1:L){
      trialNo[m] <- nrow(checkByToken[[m]])
    }
    checkOrder <- as.data.frame(cbind(No,trialNo))
    checkOrder <- head(checkOrder[order(checkOrder$trialNo),],-4)
    invalid <- names(checkByToken2[checkOrder$No])
    # print('I do not need extra one(s)')
    # print(invalid)
    for(m in 1:length(invalid)){
      vsT <- vsT %>% 
        subset(sessionToken!= invalid[m])
    }
  }
}

# givig session Oder # Giving session numbers if more than one userCode
by_participant <- vsT %>% 
  group_by(extId) %>% 
  ungroup()%>% 
  select(extId,sessionToken)
t <- split(by_participant, by_participant$extId)
vsT$sessionOrder <- -9999
by_participant <- vsT %>% 
  group_by(extId,sessionToken) 
t <- split(by_participant, by_participant$extId)
for(i in 1:length(t)){
  thisID <- names(t)[i]
  print(thisID)
  hasTokens <- unique(t[[thisID]]$sessionToken)
  tempdf <- t[[thisID]]
  hasSessionId <- unique(tempdf$sessionId)
  hasUserId <- unique(tempdf$userCode)
  if (length(hasUserId) >1 | length(hasSessionId) < 4){ # for different userCode completed by the same session to identify the sessionId
    for(k in 1: length( unique(hasUserId))){
      vsT$sessionId[vsT$userCode== hasUserId[rank(hasUserId)==k]] <- vsT$sessionId[vsT$userCode== hasUserId[rank(hasUserId)==k]]+1000*k
    }
  } # else if(hasUserId ==1 && length(hasSessionId) > 4){} # has been checked by pre-checking 
  
  sessionUniqueList <- unique(vsT$sessionId[vsT$extId== thisID])
  for(u in 1: length(sessionUniqueList)){
    vsT$sessionOrder[vsT$extId== thisID & vsT$sessionId == sessionUniqueList[u]]<- rank(sessionUniqueList)[u]
  }
}

#get meanRT and ACC
meanRT <- vector()
acc <- vector()
rowList <- c()

#by setsize
by_setsize <- vsT %>%
  group_by(extId,setSize) %>% 
  ungroup()%>% 
  select(userCode,extId,setSize,sessionToken,sessionOrder,givenResponse,score,reactionTime) %>% ungroup
by_setsize <- by_setsize %>%
  split(by_setsize$setSize)
vsAcc <-c()
vsMrt <- c()
for (i in 1: length(by_setsize)){
  t <- by_setsize[[i]] 
  t <-split(t,t$extId)
  for (j in 1:length(t)){
    temp <- t[[j]]  
    temp <- split(temp,temp$sessionToken)
    for (n in 1:length(temp)){
      extId <- unique(temp[[n]]$extId)
      acc[n] <- nrow(temp[[n]] %>% filter(score == 1))/(nrow(temp[[n]] %>% filter(score == 1))+nrow(temp[[n]] %>% filter(score == 0)))
      tempRT <- temp[[n]] %>% filter(givenResponse != 'omission') %>% select(reactionTime)
      meanRT[n] <- mean(tempRT$reactionTime)
      setsize <- unique(temp[[n]]$setSize)
      T <- unique(temp[[n]]$sessionOrder)
      vsAcc <- rbind(vsAcc, acc[n])
      vsMrt <- rbind(vsMrt,meanRT[n])
      k=(j-1)*length(temp)+n
      k=(i-1)*length(temp)*length(t)+k
      rowList[k] <- paste(extId, 'T', T,'setsize',setsize,sep = '_')
      
    }
  }
}
vsTraining <- data.frame(vsAcc,vsMrt)
row.names(vsTraining) <- na.omit(rowList)

# Final data exclusion & check all sessions (less conservative)----
missingTrainings <- c( missingORTT[-1],missingVST[-1])
inTraiings <- unique(c(unique(vsT$extId),unique(ortT$extId))) # possible missing one(s) have been excluded in ORTT & VST separately whilist in prePost missing one is still kept in the big dataframe as well as task-specific dataframes
overal <- unique(c(inTraiings,inPrePost))
missingOveral <- overal[!(inTraiings %in% inPrePost)]
list <- c(excludedOrtPost[-1],excludedOrtPre[-1], excludedSrtPost[-1],excludedSrtPre[-1],excludedOdtPost[-1],excludedOdtPre[-1],excludedVsPost[-1], excludedVST[-1],excludedORTT[-1],excludedVsPre[-1])
excluddedList <- unique(list)
toExclude <- unique(c(missingPrePost,missingTrainings,missingOveral,excluddedList)) 

print('OVERAL, we exlcuded: ')
print(toExclude)
print('...in which ....')
print('Any Missing at Pre/Post test:')
print(missingPrePost)
print('Any Missing during traiing:')
print(missingTrainings)
print('Any Missing during traiing:')
print(missingOveral)
print('Who has been further excluded according to pre-registered exclusion criteria:')
print(excluddedList)

write.csv(toExclude,'more_registered_Exclusion.csv')

ortPost <- ortPost[!(row.names(ortPost) %in% toExclude),]
write.csv(ortPost, file=paste(cleanedPrePostFolder,'ort_post.csv',sep = "/"))

ortPre <- ortPre[!(row.names(ortPre) %in% toExclude),]
write.csv(ortPre, file=paste(cleanedPrePostFolder,'ort_pre.csv',sep = "/"))

srtPost <- srtPost[!(row.names(srtPost) %in% toExclude),]
write.csv(srtPost, file=paste(cleanedPrePostFolder,'srt_post.csv',sep = "/"))

srtPre <- srtPre[!(row.names(srtPre) %in% toExclude),]
write.csv(srtPre, file=paste(cleanedPrePostFolder,'srt_pre.csv',sep = "/"))

odtPost <- odtPost[!(row.names(odtPost) %in% toExclude),]
write.csv(odtPost, file=paste(cleanedPrePostFolder,'odt_post.csv',sep = "/"))

odtPre <- odtPre[!(row.names(odtPre) %in% toExclude),]
write.csv(odtPre, file=paste(cleanedPrePostFolder,'odt_pre.csv',sep = "/"))

vsPost <- vsPost[!(row.names(vsPost) %in% toExclude),]
write.csv(vsPost, file=paste(cleanedPrePostFolder,'vs_post.csv',sep = "/"))

vsPre <- vsPre[!(row.names(vsPre) %in% toExclude),]
write.csv(vsPre, file=paste(cleanedPrePostFolder,'vs_pre.csv',sep = "/"))



for(i in 1: length(toExclude)){
  ortTraining <- ortTraining[!grepl(toExclude[i], rownames(ortTraining)),]
  }
write.csv(ortTraining, file=paste(cleanedTrainingFolder,'ort_training.csv',sep = "/"))
for(i in 1: length(toExclude)){vsTraining <- vsTraining[!grepl(toExclude[i], rownames(vsTraining)),]}
write.csv(vsTraining, file=paste(cleanedTrainingFolder,'vs_training.csv',sep = "/"))
#get ready for ortT
setSize2 <- ortTraining[which(grepl('*setsize_2',rownames(ortTraining))),]
setSize4 <- ortTraining[which(grepl('*setsize_4',rownames(ortTraining))),]
setSize6 <- ortTraining[which(grepl('*setsize_6',rownames(ortTraining))),]
t1_ss2  <- setSize2[which(grepl('*T_1_*',rownames(setSize2))),]
t2_ss2  <- setSize2[which(grepl('*T_2_*',rownames(setSize2))),]
t3_ss2  <- setSize2[which(grepl('*T_3_*',rownames(setSize2))),]
t4_ss2  <- setSize2[which(grepl('*T_4_*',rownames(setSize2))),]
t1_ss4  <- setSize4[which(grepl('*T_1_*',rownames(setSize4))),]
t2_ss4  <- setSize4[which(grepl('*T_2_*',rownames(setSize4))),]
t3_ss4  <- setSize4[which(grepl('*T_3_*',rownames(setSize4))),]
t4_ss4  <- setSize4[which(grepl('*T_4_*',rownames(setSize4))),]
t1_ss6  <- setSize6[which(grepl('*T_1_*',rownames(setSize6))),]
t2_ss6  <- setSize6[which(grepl('*T_2_*',rownames(setSize6))),]
t3_ss6  <- setSize6[which(grepl('*T_3_*',rownames(setSize6))),]
t4_ss6  <- setSize6[which(grepl('*T_4_*',rownames(setSize6))),]
write.csv(t1_ss2, file=paste(cleanedTrainingFolder,'ortT_t1_ss2.csv',sep = "/"))
write.csv(t2_ss2, file=paste(cleanedTrainingFolder,'ortT_t2_ss2.csv',sep = "/"))
write.csv(t3_ss2, file=paste(cleanedTrainingFolder,'ortT_t3_ss2.csv',sep = "/"))
write.csv(t4_ss2, file=paste(cleanedTrainingFolder,'ortT_t4_ss2.csv',sep = "/"))
write.csv(t1_ss4, file=paste(cleanedTrainingFolder,'ortT_t1_ss4.csv',sep = "/"))
write.csv(t2_ss4, file=paste(cleanedTrainingFolder,'ortT_t2_ss4.csv',sep = "/"))
write.csv(t3_ss4, file=paste(cleanedTrainingFolder,'ortT_t3_ss4.csv',sep = "/"))
write.csv(t4_ss4, file=paste(cleanedTrainingFolder,'ortT_t4_ss4.csv',sep = "/"))
write.csv(t1_ss6, file=paste(cleanedTrainingFolder,'ortT_t1_ss6.csv',sep = "/"))
write.csv(t2_ss6, file=paste(cleanedTrainingFolder,'ortT_t2_ss6.csv',sep = "/"))
write.csv(t3_ss6, file=paste(cleanedTrainingFolder,'ortT_t3_ss6.csv',sep = "/"))
write.csv(t4_ss6, file=paste(cleanedTrainingFolder,'ortT_t4_ss6.csv',sep = "/"))

# ORT SRT go to matLab

# get ready for odt (pre-post)
 kPre = odtPre$K
 kPost = odtPost$K
 odt <- data.frame(kPre,kPost)
 row.names(odt) <- row.names(odtPost)
 write.csv(odt, file=paste(outputFolder,'capacity_odt_cleaned.csv',sep = "/"))
# get ready for vs (pre-post)
 accPre=vsPre$acc
 accPost=vsPost$acc
 mrtPre=vsPre$meanRT
 mrtPost=vsPost$meanRT
 accVs <- data.frame(accPre,accPost)
 row.names(accVs) <- row.names(vsPost)
 write.csv(accVs, file=paste(outputFolder,'ACC_vs_cleaned.csv',sep = "/"))
 mrtVs <- data.frame(mrtPre,mrtPost)
 row.names(mrtVs) <- row.names(vsPost)
 write.csv(mrtVs, file=paste(outputFolder,'meanRT_vs_cleaned.csv',sep = "/"))
#get ready for vs (training)
 
 setSize8 <- vsTraining[which(grepl('*setsize_8',rownames(vsTraining))),]
 setSize16 <- vsTraining[which(grepl('*setsize_16',rownames(vsTraining))),]
 setSize24 <- vsTraining[which(grepl('*setsize_24',rownames(vsTraining))),]
 
 t1_ss8  <- setSize8[which(grepl('*T_1_*',rownames(setSize8))),]
 t2_ss8  <- setSize8[which(grepl('*T_2_*',rownames(setSize8))),]
 t3_ss8  <- setSize8[which(grepl('*T_3_*',rownames(setSize8))),]
 t4_ss8  <- setSize8[which(grepl('*T_4_*',rownames(setSize8))),]
 t1_ss16 <- setSize16[which(grepl('*T_1_*',rownames(setSize16))),]
 t2_ss16 <- setSize16[which(grepl('*T_2_*',rownames(setSize16))),]
 t3_ss16 <- setSize16[which(grepl('*T_3_*',rownames(setSize16))),]
 t4_ss16 <- setSize16[which(grepl('*T_4_*',rownames(setSize16))),]
 t1_ss24 <- setSize24[which(grepl('*T_1_*',rownames(setSize24))),]
 t2_ss24 <- setSize24[which(grepl('*T_2_*',rownames(setSize24))),]
 t3_ss24 <- setSize24[which(grepl('*T_3_*',rownames(setSize24))),]
 t4_ss24 <- setSize24[which(grepl('*T_4_*',rownames(setSize24))),]
 vsList <- sub('*_T_\\d_setsize_\\d','',rownames( t1_ss8))
 acc_vsT <- data_frame(t1_ss8$vsAcc,t2_ss8$vsAcc, t3_ss8$vsAcc,t4_ss8$vsAcc,t1_ss16$vsAcc,t2_ss16$vsAcc,t3_ss16$vsAcc,t4_ss16$vsAcc,t1_ss24$vsAcc,t2_ss24$vsAcc,t3_ss24$vsAcc,t4_ss24$vsAcc) 
 mrt_vsT <- data_frame(t1_ss8$vsMrt,t2_ss8$vsMrt, t3_ss8$vsMrt,t4_ss8$vsMrt,t1_ss16$vsMrt,t2_ss16$vsMrt,t3_ss16$vsMrt,t4_ss16$vsMrt,t1_ss24$vsMrt,t2_ss24$vsMrt,t3_ss24$vsMrt,t4_ss24$vsMrt) 
 acc_vsT <- as.data.frame(acc_vsT)
 mrt_vsT <- as.data.frame(mrt_vsT)
 row.names(acc_vsT) <- vsList
 row.names(mrt_vsT) <- vsList
 write.csv(acc_vsT, file=paste(outputFolder,'ACC_vsTraining_cleaned.csv',sep = "/"))
 write.csv(mrt_vsT, file=paste(outputFolder,'meanRT_vsTraining_cleaned.csv',sep = "/"))




