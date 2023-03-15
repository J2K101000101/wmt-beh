goodName <- function(all){
all_1 <-all[!is.na(all$extId)&all$extId!='test',]
for (r in 1: nrow(all_1)){
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
return(all_1)
}