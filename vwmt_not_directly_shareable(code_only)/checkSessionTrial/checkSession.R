#checking overall session n to get inclusion list by very strict session/trial control
set.seed(1234321)
# install.packages('pacman')
library(pacman)
pacman::p_load(pacman,dplyr,tidyr,plyr,tidyverse,digest)
rm(list=ls()) 
workspace = getwd()
readFolder = paste(workspace, "read", sep = "/")
inputFolder = paste(workspace, "rawData", sep = "/")
removeRead <- list.files(readFolder,include.dirs = F,full.names = T, recursive = T)
file.remove(removeRead)
zipFiles <- list.files(path=inputFolder, pattern = "*.zip", full.names = TRUE,recursive = TRUE)
llply(.data = zipFiles, .fun = unzip, exdir = readFolder)
readFiles <- list.files(path = readFolder, pattern="*.csv", full.names = TRUE)

readFileHashes <- c()
for (i in 1:length(readFiles)){
  readFileHashes[i] <- sha1(readChar(readFiles[i], file.info(readFiles[i])$size))
}
n_occur <- data.frame(table(readFileHashes))
duplicateHashes <- list()
for (i in 1:nrow(n_occur)){
  if (n_occur$Freq[i] > 1) {
    duplicateHashes <- append(duplicateHashes, n_occur$readFileHashes[i])  
  }
}
duplicates <- list()
checkInx <- NULL
for (i in 1:length(duplicateHashes)){
  checkInx <- rbind(checkInx,append(duplicates, grep(duplicateHashes[i], readFileHashes)))
}
removeIndx <- unlist(checkInx[,1]) 
print('we remove these duplicated files in order to read later:')
print(readFiles[removeIndx])
nonDuplicatedReadFiles<-readFiles[-removeIndx]
# in case 1:  forceuploaded replicated sessions  

# in case 2, the incompleted session also has too many trials plus standard sessions 360*4+560*2 could be excluded
all <- ldply(.data = nonDuplicatedReadFiles, .fun = read.csv,header = TRUE, sep=',', stringsAsFactors = FALSE) 
check_extId <- unique(all $extId)

all_1 <-all[!is.na(all$extId)&all$extId!='test',]
for (r in 1: nrow(all_1)){     # rename the messy extIds to make sure that the same participant has only one Id
  if (nchar(all_1$extId[r])>24){
    if (substring(all_1$extId[r][1], 1, 1) == "[") {
      all_1$extIdNew[r] <- substring(all_1$extId[r], 15, 38)
    } else {
      all_1$extIdNew[r] <- substring(all_1$extId[r], 14, 37)
    }
  } else {
    all_1$extIdNew[r] <- all_1$extId[r]
  }
}
check_extId2 <- unique(all_1$extIdNew)
all_1 <- all_1 %>% select(-extId)
names(all_1)[names(all_1) == "extIdNew"] <- "extId"
replace <- read.csv('groupInfo98withprolificID.csv', header = TRUE, sep = ',')
check <- all_1 %>% group_by(extId)
check <- split(check,check$extId)
summary<- list()
for (i in 1:length(check)){
  participant = names(check)[i]
  checkByToken <- group_by(check[[i]], check[[i]]$sessionToken)
  checkByToken <- split(check[[i]], check[[i]]$sessionToken)
  nSession <- length(checkByToken)
  tlb <-matrix(0, nrow = nSession, ncol = 2)
  for(j in 1:nSession) {
    thisToken <- names(checkByToken)[j]
    nTrial <- nrow(checkByToken[[thisToken]])
    tlb[j,1] <- thisToken
    tlb[j,2] <- nTrial
  }
  summary[[participant]] <- tlb
}


mayIDs <-NULL
mayTokens <- NULL
goodTokens <- NULL
goodIDs <- NULL
wellIDs <- NULL
perfect <- 360*4+560*2
participants <- names(summary)
for(m in 1:length(summary)){
  if(sum(summary[[m]][,2] %in% c('560','360'))==6 && dim(summary[[m]])[1]==6){ 
    goodTokens<- rbind(goodTokens,summary[[m]])
    goodIDs <- cbind(goodIDs,participants[m])
  }else{
    wellIDs <- cbind(wellIDs,participants[m])
  }
  if (sum(as.numeric (summary[[m]][,2])) > (perfect-12) && sum(as.numeric (summary[[m]][,2])) < (perfect+12)&& dim(summary[[m]])[1]>=6){
    mayIDs <- cbind(mayIDs,participants[m])
    mayTokens<- rbind(mayTokens,summary[[m]])
  }
  
}

capture.output(summary, file = "my_list.txt") 
includeID <- c(goodIDs,mayIDs)
includeTokens <- rbind(goodTokens,mayTokens)
extraOKToken <- NULL
for(t in 1: dim(includeTokens)[1]){ # remove the non-significant extra trials and session
  if (as.numeric(includeTokens[t,2]) < 12){
    extraOKToken <- cbind(extraOKToken,includeTokens[t,1])
  }}
includeTokensdf <- as.data.frame(includeTokens)
includeTokensdf<- includeTokensdf %>% subset(!includeTokensdf$V1 %in% extraOKToken)
write.csv(includeTokensdf, 'strictToken.csv')
write.csv(includeID, 'included_ID_summary.csv')

veryStrict <- replace[replace$Row.names %in% includeID, ]
write.csv(veryStrict,'strictGroup.csv')


