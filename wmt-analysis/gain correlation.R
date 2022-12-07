# get ready----

set.seed(1234321)
# install.packages('pacman')
# install.packages('pacman')
library(pacman)
pacman::p_load(dplyr,tidyr,plyr,tidyverse,ggpubr,rstatix,ggplot2, ez, BayesFactor)
rm(list=ls()) 

# read files ####
workspace = getwd()
outputFolder = paste(workspace, "output", sep = "/")
preFolder = paste(outputFolder, "pre", sep = "/")
prePostFolder =paste(outputFolder, "prePost", sep = "/")
demographics <- read_csv('Participants demographics(all).csv')
demographics <- demographics %>% 
  subset( `Completed the whole study or..?`== 'y')%>% 
  select(participant_id, age,Sex)
readFiles <- list.files(path = outputFolder, pattern="*.csv", full.names = TRUE)
final <- lapply(readFiles, read.csv, row.names = 1,header = TRUE, sep=',', stringsAsFactors = FALSE)
COL<-unique(unlist(lapply(final, colnames)))
ROW<-unique(unlist(lapply(final, rownames)))
TOTAL<-matrix(data=NA, nrow=length(ROW), ncol=length(COL), dimnames=list(ROW, COL))
for (df in final) { 
       TOTAL[rownames(df), colnames(df)] <- as.matrix(df)
  }
included <- as.data.frame(TOTAL)
includedDemographics <- demographics[(demographics$participant_id %in% row.names(included)),]
includedTable <- data.frame(includedDemographics$age,includedDemographics$Sex,row.names = includedDemographics$participant_id)
finalTable <- merge(included,includedTable,by="row.names")
experimentalGroup <- finalTable %>% 
  filter(!is.na(capacity_ortT_t1_ss2) & !is.na(capacity_ortT_t2_ss2) & !is.na(capacity_ortT_t3_ss2) & !is.na(capacity_ortT_t4_ss2)& !is.na(capacity_ortT_t1_ss4) & !is.na(capacity_ortT_t2_ss4) & !is.na(capacity_ortT_t3_ss4) & !is.na(capacity_ortT_t4_ss4) & !is.na(capacity_ortT_t1_ss6) & !is.na(capacity_ortT_t2_ss6) & !is.na(capacity_ortT_t3_ss6) & !is.na(capacity_ortT_t4_ss6) & !is.na(precision_ortT_t1_ss2) & !is.na(precision_ortT_t2_ss2) & !is.na(precision_ortT_t3_ss2) & !is.na(precision_ortT_t4_ss2)& !is.na(precision_ortT_t1_ss4) & !is.na(precision_ortT_t2_ss4) & !is.na(precision_ortT_t3_ss4) & !is.na(precision_ortT_t4_ss4) & !is.na(precision_ortT_t1_ss6) & !is.na(precision_ortT_t2_ss6) & !is.na(precision_ortT_t3_ss6) & !is.na(precision_ortT_t4_ss6)) %>% 
  cbind(group = 'experiment')
controlGroup <- finalTable %>% 
  filter(!is.na(t1_ss8.vsAcc) & !is.na(t2_ss8.vsAcc) & !is.na(t3_ss8.vsAcc) & !is.na(t4_ss8.vsAcc) & !is.na(t1_ss16.vsAcc) & !is.na(t2_ss16.vsAcc) & !is.na(t3_ss16.vsAcc) & !is.na(t4_ss16.vsAcc)  & !is.na(t1_ss24.vsAcc) & !is.na(t2_ss24.vsAcc) & !is.na(t3_ss24.vsAcc) & !is.na(t4_ss24.vsAcc)  & !is.na(t1_ss8.vsMrt) & !is.na(t2_ss8.vsMrt) & !is.na(t3_ss8.vsMrt) & !is.na(t4_ss8.vsMrt) & !is.na(t1_ss16.vsMrt) & !is.na(t2_ss16.vsMrt) & !is.na(t3_ss16.vsMrt) & !is.na(t4_ss16.vsMrt)  & !is.na(t1_ss24.vsMrt) & !is.na(t2_ss24.vsMrt) & !is.na(t3_ss24.vsMrt) & !is.na(t4_ss24.vsMrt)) %>% 
  cbind(group = 'control')
grouped <- rbind(experimentalGroup,controlGroup)
groupedFinal <- cbind(grouped, id = c(1:96))

# demographics####


groupedFinal %>%
  dplyr::group_by(group,includedDemographics.Sex) %>%
  dplyr::summarise(nSex = dplyr::n()) %>% 
  ungroup()

demSummary <- groupedFinal %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(mAge = mean(includedDemographics.age),
                   sdAge = sd(includedDemographics.age),
                   ) %>%  ungroup()
# group       mAge sdAge
# <fct>      <dbl> <dbl>
#   1 experiment  23.0  4.13
# 2 control     21.8  2.80


prePost <- groupedFinal %>% 
  select(group,accPre,mrtPre,kPre,accPost,mrtPost,kPost,capacity_ort_pre,capacity_srt_pre,precision_ort_pre,precision_srt_pre,capacity_ort_post,capacity_srt_post,precision_ort_post,precision_srt_post,Row.names,id ) %>% 
  mutate(capacity_ort_gain = capacity_ort_post - capacity_ort_pre, precision_ort_gain = precision_ort_post - precision_ort_pre, capacity_srt_gain = capacity_srt_post - capacity_srt_pre, precision_srt_gain = precision_srt_post - precision_srt_pre, k_gain = kPost - kPre, acc_gain = accPost - accPre, mrt_gain = mrtPost - mrtPre)

E <- prePost %>% subset(group == 'experiment')
C <- prePost %>% subset(group == 'control')

#capacity cor precision in both ORT and SRT####

#liner covariation?  yes 
ggscatter(prePost, x = 'capacity_ort_pre', y = 'precision_ort_pre', 
          add = "reg.line", conf.int = TRUE, 
          xlab = "capacity_ort_pre", ylab = "precision_ort_pre")
# normal distribution? No + No
shapiro.test(prePost$capacity_ort_pre)
shapiro.test(prePost$precision_ort_pre)
cor.test(prePost$capacity_ort_pre, prePost$precision_ort_pre, 
         method = "spearman")

cor.test(E$capacity_ort_pre, E$precision_ort_pre, 
         method = "spearman")

cor.test(C$capacity_ort_pre, C$precision_ort_pre, 
         method = "spearman")
#liner covariation?  yes 
ggscatter(prePost, x = 'capacity_srt_pre', y = 'precision_srt_pre', 
          add = "reg.line", conf.int = TRUE, 
          xlab = "capacity_srt_pre", ylab = "precision_srt_pre")
# normal distribution? yes + No
shapiro.test(prePost$capacity_srt_pre)
shapiro.test(prePost$precision_srt_pre)
cor.test(prePost$capacity_srt_pre, prePost$precision_srt_pre, 
         method = "spearman")

cor.test(E$capacity_srt_pre, E$precision_srt_pre, 
         method = "spearman")

cor.test(C$capacity_srt_pre, C$precision_srt_pre, 
         method = "spearman")


#liner covariation?  yes 
ggscatter(prePost, x = 'capacity_ort_post', y = 'precision_ort_post', 
          add = "reg.line", conf.int = TRUE, 
          xlab = "capacity_ort_post", ylab = "precision_ort_post")
# normal distribution? No + yes
shapiro.test(prePost$capacity_ort_post)
shapiro.test(prePost$precision_ort_post)
cor.test(prePost$capacity_ort_post, prePost$precision_ort_post, 
         method = "spearman")

cor.test(E$capacity_ort_post, E$precision_ort_post, 
         method = "spearman")

cor.test(C$capacity_ort_post, C$precision_ort_post, 
         method = "spearman")

#liner covariation?  yes 
ggscatter(prePost, x = 'capacity_srt_post', y = 'precision_srt_post', 
          add = "reg.line", conf.int = TRUE, 
          xlab = "capacity_srt_post", ylab = "precision_srt_post")
# normal distribution? yes + no
shapiro.test(prePost$capacity_srt_post)
shapiro.test(prePost$precision_srt_post)
cor.test(prePost$capacity_srt_post, prePost$precision_srt_post, 
         method = "spearman")

cor.test(E$capacity_srt_post, E$precision_srt_post, 
         method = "spearman")

cor.test(C$capacity_srt_post, C$precision_srt_post, 
         method = "spearman")

# capacity in ort ####
capOrt <- prePost %>% select(capacity_ort_pre,capacity_ort_post,group,id) %>% 
  gather(key = "session", value = "score",capacity_ort_post, capacity_ort_pre) %>% 
  convert_as_factor(session,id,group) %>%  
  mutate(time=fct_relevel(session, 'capacity_ort_pre','capacity_ort_post'))
 
ggplot(capOrt, aes(x = time, y = score, col = group, group = id)) +
  geom_point(aes(shape = group)) +
  geom_line(show.legend = F)+
  labs(title="Training-induced changes in capacity in ORT", x= "Time", y = "Capacity")+
  theme_classic()

ggscatter(prePost, x = 'capacity_ort_gain', y = 'precision_ort_gain', color = 'group', 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "capacity_ort_gain", ylab = "precision_ort_gain")

#liner covariation?  yes 
ggscatter(prePost, x = 'capacity_ort_gain', y = 'precision_ort_gain', 
          add = "reg.line", conf.int = TRUE, 
          xlab = "capacity_ort_gain", ylab = "precision_ort_gain")

# normal distribution? No + No
shapiro.test(prePost$capacity_ort_gain)
# Shapiro-Wilk normality test
# 
# data:  prePost$capacity_ort_gain
# W = 0.8752, p-value = 1.814e-07
shapiro.test(prePost$precision_ort_gain)
# Shapiro-Wilk normality test
# 
# data:  prePost$precision_ort_gain
# W = 0.94438, p-value = 0.0004854

cor.test(prePost$capacity_ort_gain, prePost$precision_ort_gain, 
         method = "spearman")

cor.test(E$capacity_ort_gain, E$precision_ort_gain, 
         method = "spearman")

cor.test(C$capacity_ort_gain, C$precision_ort_gain, 
         method = "spearman")
 
# Spearman's rank correlation rho
# 
# data:  prePost$capacity_ort_gain and prePost$precision_ort_gain
# S = 140348, p-value = 0.6411
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# 0.04810092 
# 
# > cor.test(E$capacity_ort_gain, E$precision_ort_gain, 
# +          method = "spearman")
# 
# 	Spearman's rank correlation rho
# 
# data:  E$capacity_ort_gain and E$precision_ort_gain
# S = 13272, p-value = 0.2266
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#   rho 
# 0.1814986 
# 
# > cor.test(C$capacity_ort_gain, C$precision_ort_gain, 
#            +          method = "spearman")
# 
# Spearman's rank correlation rho
# 
# data:  C$capacity_ort_gain and C$precision_ort_gain
# S = 25730, p-value = 0.09964
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# -0.2355342

#*
ggscatter(prePost, x = 'capacity_srt_gain', y = 'precision_srt_gain', color = 'group', 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "capacity_srt_gain", ylab = "precision_srt_gain")
#liner covariation?  yes 
ggscatter(prePost, x = 'capacity_srt_gain', y = 'precision_srt_gain', 
          add = "reg.line", conf.int = TRUE, 
          xlab = "capacity_srt_gain", ylab = "precision_srt_gain")

# normal distribution? No + No
shapiro.test(prePost$capacity_srt_gain)
shapiro.test(prePost$precision_srt_gain)

cor.test(prePost$capacity_srt_gain, prePost$precision_srt_gain, 
         method = "spearman")
cor.test(E$capacity_srt_gain, E$precision_srt_gain, 
         method = "spearman")
cor.test(C$capacity_srt_gain, C$precision_srt_gain, 
         method = "spearman")

# Spearman's rank correlation rho
# 
# data:  prePost$capacity_srt_gain and prePost$precision_srt_gain
# S = 253548, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# -0.719669 
# 
# > cor.test(E$capacity_srt_gain, E$precision_srt_gain, 
# +          method = "spearman")
# 
# 	Spearman's rank correlation rho
# 
# data:  E$capacity_srt_gain and E$precision_srt_gain
# S = 27708, p-value = 1.583e-07
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#   rho 
# -0.7087882 
# 
# > cor.test(C$capacity_srt_gain, C$precision_srt_gain, 
#            +          method = "spearman")
# 
# Spearman's rank correlation rho
# 
# data:  C$capacity_srt_gain and C$precision_srt_gain
# S = 36156, p-value = 3.687e-09
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# -0.7361825 



#** acc and mrt tradeoff
ggscatter(prePost, x = 'acc_gain', y = 'mrt_gain', color = 'group', 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "acc_gain", ylab = "mrt_gain")


ggscatter(prePost, x = 'capacity_ort_gain', y = 'capacity_srt_gain', color = 'group', 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "capacity_ort_gain", ylab = "capacity_srt_gain")


#liner covariation?  yes 
ggscatter(prePost, x = 'capacity_ort_gain', y = 'capacity_srt_gain', 
          add = "reg.line", conf.int = TRUE, 
          xlab = "capacity_ort_gain", ylab = "capacity_srt_gain")

# normal distribution? No + No
shapiro.test(prePost$capacity_ort_gain)
shapiro.test(prePost$capacity_srt_gain)

cor.test(prePost$capacity_ort_gain, prePost$capacity_srt_gain, 
         method = "spearman")
cor.test(E$capacity_ort_gain, E$capacity_srt_gain, 
         method = "spearman")
cor.test(C$capacity_ort_gain, C$capacity_srt_gain, 
         method = "spearman")

ggscatter(prePost, x = 'capacity_ort_gain', y = 'precision_srt_gain', color = 'group', 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "capacity_ort_gain", ylab = "precision_srt_gain")

cor.test(prePost$capacity_ort_gain, prePost$precision_srt_gain, 
         method = "spearman")

cor.test(E$capacity_ort_gain, E$precision_srt_gain, 
         method = "spearman")

cor.test(C$capacity_ort_gain, C$precision_srt_gain, 
         method = "spearman")

ggscatter(prePost, x = 'capacity_ort_gain', y = 'k_gain', color = 'group', 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "capacity_ort_gain", ylab = "k_gain")
cor.test(prePost$capacity_ort_gain, prePost$k_gain, 
         method = "spearman")

cor.test(E$capacity_ort_gain, E$k_gain, 
         method = "spearman")

cor.test(C$capacity_ort_gain, C$k_gain, 
         method = "spearman")

#*
ggscatter(prePost, x = 'capacity_srt_gain', y = 'k_gain', color = 'group', 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "capacity_srt_gain", ylab = "k_gain")

cor.test(prePost$capacity_srt_gain, prePost$k_gain, 
         method = "spearman")

cor.test(E$capacity_srt_gain, E$k_gain, 
         method = "spearman")

cor.test(C$capacity_srt_gain, C$k_gain, 
         method = "spearman")
#**
ggscatter(prePost, x = 'precision_srt_gain', y = 'k_gain', color = 'group', 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "precision_srt_gain", ylab = "k_gain")

cor.test(prePost$precision_srt_gain, prePost$k_gain, 
         method = "spearman")

cor.test(E$precision_srt_gain, E$k_gain, 
         method = "spearman")

cor.test(C$precision_srt_gain, C$k_gain, 
         method = "spearman")

ggscatter(prePost, x = 'capacity_ort_gain', y = 'acc_gain', color = 'group', 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "capacity_ort_gain", ylab = "acc_gain")

cor.test(prePost$capacity_ort_gain, prePost$acc_gain, 
         method = "spearman")

cor.test(E$capacity_ort_gain, E$acc_gain, 
         method = "spearman")

cor.test(C$capacity_ort_gain, C$acc_gain, 
         method = "spearman")

ggscatter(prePost, x = 'capacity_ort_gain', y = 'mrt_gain', color = 'group', 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "capacity_ort_gain", ylab = "mrt_gain")

cor.test(prePost$capacity_ort_gain, prePost$mrt_gain, 
         method = "spearman")

cor.test(E$capacity_ort_gain, E$mrt_gain, 
         method = "spearman")

cor.test(C$capacity_ort_gain, C$mrt_gain, 
         method = "spearman")
# precision in ort ####
preOrt <- prePost %>% select(precision_ort_pre,precision_ort_post,group,id) %>% 
  gather(key = "session", value = "score",precision_ort_post, precision_ort_pre) %>% 
  convert_as_factor(session,id,group) %>%  
  mutate(time=fct_relevel(session, 'precision_ort_pre','precision_ort_post'))

ggplot(preOrt, aes(x = time, y = score, col = group, group = id)) +
  geom_point(aes(shape = group)) +
  geom_line(show.legend = F)+
  labs(title="Training-induced changes in capacity in ORT", x= "Time", y = "Precision")+
  theme_classic()


ggscatter(prePost, x = 'precision_ort_gain', y = 'capacity_srt_gain', color = 'group', 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "precision_ort_gain", ylab = "capacity_srt_gain")

cor.test(prePost$precision_ort_gain, prePost$capacity_srt_gain, 
         method = "spearman")
cor.test(E$precision_ort_gain, E$capacity_srt_gain, 
         method = "spearman")
cor.test(C$precision_ort_gain, C$capacity_srt_gain, 
         method = "spearman")

ggscatter(prePost, x = 'precision_ort_gain', y = 'precision_srt_gain', color = 'group', 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "precision_ort_gain", ylab = "precision_srt_gain")

cor.test(prePost$precision_ort_gain, prePost$precision_srt_gain, 
         method = "spearman")
cor.test(E$precision_ort_gain, E$precision_srt_gain, 
         method = "spearman")
cor.test(C$precision_ort_gain, C$precision_srt_gain, 
         method = "spearman")

ggscatter(prePost, x = 'precision_ort_gain', y = 'k_gain', color = 'group', 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "precision_ort_gain", ylab = "k_gain")

ggscatter(prePost, x = 'precision_ort_gain', y = 'k_gain', 
          add = "reg.line", conf.int = TRUE, 
          xlab = "capacity_ort_gain", ylab = "capacity_srt_gain")

cor.test(prePost$precision_ort_gain, prePost$k_gain, 
         method = "spearman")
cor.test(E$precision_ort_gain, E$k_gain, 
         method = "spearman")
cor.test(C$precision_ort_gain, C$k_gain, 
         method = "spearman")


ggscatter(prePost, x = 'precision_ort_gain', y = 'acc_gain', color = 'group', 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "precision_ort_gain", ylab = "acc_gain")


cor.test(prePost$precision_ort_gain, prePost$acc_gain, 
         method = "spearman")
cor.test(E$precision_ort_gain, E$acc_gain, 
         method = "spearman")
cor.test(C$precision_ort_gain, C$acc_gain, 
         method = "spearman")

ggscatter(prePost, x = 'precision_ort_gain', y = 'mrt_gain', color = 'group', 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "precision_ort_gain", ylab = "mrt_gain")

cor.test(prePost$precision_ort_gain, prePost$mrt_gain, 
         method = "spearman")
cor.test(E$precision_ort_gain, E$mrt_gain, 
         method = "spearman")
cor.test(C$precision_ort_gain, C$mrt_gain, 
         method = "spearman")

#clear up----


p_unload(all)


