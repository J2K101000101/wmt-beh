# check remove some replica 
reduceReplica <- function (readFiles){
pacman::p_load(digest)
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
  if(length(duplicateHashes)>0){ checkInx <- rbind(checkInx,append(duplicates, grep(duplicateHashes[i], readFileHashes)))}
}
removeIndx <- unlist(checkInx[,1]) 
if (length(removeIndx)>0){
  print('we remove these duplicated files in order to read later:')
  print(readFiles[removeIndx])} else{print('No duplicated files:)')}
if(length(readFiles[removeIndx])>0){nonDuplicatedReadFiles<-readFiles[-removeIndx]}else{nonDuplicatedReadFiles <- readFiles}
}