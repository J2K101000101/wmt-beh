# get ready----
# Doing all with non-parametric if the assumptions have been violated
set.seed(1234321)
# install.packages('pacman')
# install.packages('pacman')
library(pacman)
pacman::p_load(pacman,dplyr,tidyr,plyr,tidyverse,ggpubr,rstatix,ggplot2,broom,AICcmodavg,car,rcompanion,WRS2, apaTables)
rm(list=ls()) 

# read files ####
workspace = getwd()
outputFolder = paste(workspace, "output", sep = "/")
preFolder = paste(outputFolder, "pre", sep = "/")
prePostFolder =paste(outputFolder, "prePost", sep = "/") #include the prepost and training graphs 
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
groupedFinal <- rbind(experimentalGroup,controlGroup)

# demographics####
groupedFinal %>%
  dplyr::group_by(group,includedDemographics.Sex) %>%
  dplyr::summarise(nSex = dplyr::n()) %>% 
  ungroup()

demSummary <- groupedFinal %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(mAge = mean(includedDemographics.age),
                   sdAge = sd(includedDemographics.age),
                   ) %>% 
  ungroup()
# group      includedDemographics.Sex  nSex
# <fct>      <fct>                    <int>
#   1 experiment Female                      11
# 2 experiment Male                        34
# 3 control    Female                      21
# 4 control    Male                        29
# 1
# experiment
# 23.04444
# 4.128369
# 2
# control
# 21.82000
# 2.797156

#t-tests in pre-test####
pre <- groupedFinal %>% 
  select(group,accPre,mrtPre,kPre,capacity_ort_pre,capacity_srt_pre,precision_ort_pre,precision_srt_pre) %>% 
  group_by(group)

# ort_capacity
df_capacity_ort_pre <- groupedFinal %>% 
  select(group,capacity_ort_pre) 

pre_summary <- df_capacity_ort_pre %>% 
  group_by(group) %>% 
  get_summary_stats(capacity_ort_pre, type = "mean_sd")

ggplot(df_capacity_ort_pre,aes(x= capacity_ort_pre))+
  geom_histogram(aes(color =group, fill=group),position='identity', bins=30,alpha=0.4)+
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+ 
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))+
  geom_density(aes(linetype=group),color= 'red')
ggsave('capacity_ort_pre_hist',device = "png", path = preFolder)

ggboxplot(df_capacity_ort_pre, x = "group", y = "capacity_ort_pre",
          color = "group", palette =c("#00AFBB", "#E7B800"),
          add = "jitter", shape = "group") +
  labs(title="Baseline capacity between groups in ORT",x="Groups", y = "Capacity")
ggsave('capacity_ort_pre_box',device = "png", path = preFolder)

# Check outlier(s)
df_capacity_ort_pre %>% 
  group_by(group) %>% 
  identify_outliers(capacity_ort_pre)
# There were no extreme outlier
# # A tibble: 1 x 4
# group      df_capacity_ort_pre is.outlier is.extreme
# <fct>                 <dbl> <lgl>      <lgl>     
#   1 experiment            0.152 TRUE       FALSE 

# Check Normality
df_capacity_ort_pre %>% 
  group_by(group) %>% 
  shapiro_test(capacity_ort_pre)
# control group's data is not normally distributed!!!
# # A tibble: 2 x 4
# group      variable         statistic        p
# <fct>      <chr>                <dbl>    <dbl>
#   1 experiment df_capacity_ort_pre     0.953 0.0586  
# 2 control    df_capacity_ort_pre     0.893 0.000295
ggqqplot(df_capacity_ort_pre,x = "capacity_ort_pre",facet.by = "group",color = "group", palette = c("#00AFBB", "#E7B800"), shape = "group")
ggsave('capacity_ort_pre_qq',device = "png", path = preFolder)


# Check homogeneity 
df_capacity_ort_pre %>% levene_test(capacity_ort_pre ~ group)
# no significant difference between the variances of the two groups
#   df1   df2 statistic     p
# <int> <int>     <dbl> <dbl>
#   1     1    94    0.0179 0.894   

## because the distribution is not normal, so use non-parametric: Mann-Whitney test also called Wilcoxon rank sum test
wilcox.test(df_capacity_ort_pre$capacity_ort_pre ~ df_capacity_ort_pre$group)
rcompanion::wilcoxonR(x= df_capacity_ort_pre$capacity_ort_pre, g=df_capacity_ort_pre$group)
# Wilcoxon rank sum test with continuity correction
# 
# data:  df_capacity_ort_pre$capacity_ort_pre by df_capacity_ort_pre$group
# W = 1269, p-value = 0.2848
# alternative hypothesis: true location shift is not equal to 0
# r 
# 0.11 



# ort_precision
df_precision_ort_pre <- groupedFinal %>% 
  select(group,precision_ort_pre,Row.names) 

pre_summary <- rbind(pre_summary, df_precision_ort_pre %>% 
  group_by(group) %>% 
  get_summary_stats(precision_ort_pre, type = "mean_sd"))

ggplot(df_precision_ort_pre,aes(x= precision_ort_pre))+
  geom_histogram(aes(color =group, fill=group),position='identity', bins=30,alpha=0.4)+
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+ 
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))+
  geom_density(aes(linetype=group),color= 'red')
ggsave('precision_ort_pre_hist',device = "png", path = preFolder)

ggboxplot(df_precision_ort_pre, x = "group", y = "precision_ort_pre",
          color = "group", palette =c("#00AFBB", "#E7B800"),
          add = "jitter", shape = "group") +
  labs(title="Baseline precision between groups in ORT",x="Groups", y = "Precision")
ggsave('precision_ort_pre_box',device = "png", path = preFolder)


# Check outlier(s)
 df_precision_ort_pre %>% 
     group_by(group) %>% 
     identify_outliers(precision_ort_pre)
 
 # group      precision_ort_pre Row.names                is.outlier is.extreme
 # <fct>                  <dbl> <I<chr>>                 <lgl>      <lgl>     
 #   1 experiment            0.0969 5da9b30a3624c200157bd9f0 TRUE       FALSE     
 # 2 control               0.120  5bc7033bd2bea100010ca42e TRUE       TRUE      
 # 3 control               0.122  5e84f5b5f9dd660d5ee7437a TRUE       TRUE      

# Check Normality
 df_precision_ort_pre %>% 
   group_by(group) %>% 
   shapiro_test(precision_ort_pre)
 # # A tibble: 2 x 4
 # group      variable          statistic         p
 # <fct>      <chr>                 <dbl>     <dbl>
 #   1 experiment precision_ort_pre     0.981 0.656    
 # 2 control    precision_ort_pre     0.870 0.0000562
ggqqplot(df_precision_ort_pre,x = "precision_ort_pre",facet.by = "group",color = "group", palette = c("#00AFBB", "#E7B800"), shape = "group")
ggsave('precision_ort_pre_qq',device = "png", path = preFolder) 
 
#Check homogeneity
df_precision_ort_pre %>% levene_test(precision_ort_pre ~ group)
# df1   df2 statistic     p
# <int> <int>     <dbl> <dbl>
#   1     1    93   0.00978 0.921


wilcox.test(df_precision_ort_pre$precision_ort_pre ~ df_precision_ort_pre$group)
rcompanion::wilcoxonR(x= df_precision_ort_pre$precision_ort_pre, g=df_precision_ort_pre$group)
# Wilcoxon rank sum test with continuity correction
# 
# data:  df_precision_ort_pre$precision_ort_pre by df_precision_ort_pre$group
# W = 1046, p-value = 0.5585
# alternative hypothesis: true location shift is not equal to 0
# r 
# -0.0604 

# srt_capacity
df_capacity_srt_pre <- groupedFinal %>% 
  select(group,capacity_srt_pre,Row.names) 

pre_summary <- rbind(pre_summary,df_capacity_srt_pre %>% 
  group_by(group) %>% 
  get_summary_stats(capacity_srt_pre, type = "mean_sd"))

ggboxplot(df_capacity_srt_pre, x = "group", y = "capacity_srt_pre",
          color = "group", palette =c("#00AFBB", "#E7B800"),
          add = "jitter", shape = "group") +
  labs(title="Baseline capacity between groups in srt",x="Groups", y = "Capacity")
ggsave('capacity_srt_pre_box',device = "png", path = preFolder)

ggplot(df_capacity_srt_pre,aes(x= capacity_srt_pre))+
  geom_histogram(aes(color =group, fill=group),position='identity', bins=30,alpha=0.4)+
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+ 
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))+
  geom_density(aes(linetype=group),color= 'red')
ggsave('capacity_srt_pre_hist',device = "png", path = preFolder)

# Check outlier(s)
df_capacity_srt_pre %>% 
  group_by(group) %>% 
  identify_outliers(capacity_srt_pre)
# There were no extreme outlier
# group   capacity_srt_pre Row.names                is.outlier is.extreme
# <fct>              <dbl> <I<chr>>                 <lgl>      <lgl>     
#   1 control             4.00 5e84f5b5f9dd660d5ee7437a TRUE       FALSE      

# Check Normality
df_capacity_srt_pre %>% 
  group_by(group) %>% 
  shapiro_test(capacity_srt_pre)
ggqqplot(df_capacity_srt_pre,x = "capacity_srt_pre",facet.by = "group",color = "group", palette = c("#00AFBB", "#E7B800"), shape = "group")
ggsave('capacity_srt_pre_qq',device = "png", path = preFolder)
# group      variable         statistic     p
# <fct>      <chr>                <dbl> <dbl>
#   1 experiment capacity_srt_pre     0.982 0.688
# 2 control    capacity_srt_pre     0.991 0.967

# Check homogeneity 
df_capacity_srt_pre %>% levene_test(capacity_srt_pre ~ group)
# df1   df2 statistic     p
# <int> <int>     <dbl> <dbl>
#   1     1    93      2.42 0.123

# satisfied all the assumptions 
# Welch Two Sample t-test (different group size...)
df_capacity_srt_pre %>% t_test(capacity_srt_pre ~ group)
df_capacity_srt_pre %>% cohens_d(capacity_srt_pre ~ group)
# .y.              group1     group2     n1    n2 statistic    df     p
# * <chr>            <chr>      <chr>   <int> <int>     <dbl> <dbl> <dbl>
#   1 capacity_srt_pre experiment control    45    50     0.742  84.0  0.46
# .y.              group1     group2  effsize    n1    n2 magnitude 
# * <chr>            <chr>      <chr>     <dbl> <int> <int> <ord>     
#   1 capacity_srt_pre experiment control   0.153    45    50 negligible

# srt_precision
df_precision_srt_pre <- groupedFinal %>% 
  select(group,precision_srt_pre, Row.names) 

pre_summary <- rbind(pre_summary, df_precision_srt_pre %>% 
                       group_by(group) %>% 
                       get_summary_stats(precision_srt_pre, type = "mean_sd"))

ggboxplot(df_precision_srt_pre, x = "group", y = "precision_srt_pre",
          color = "group", palette =c("#00AFBB", "#E7B800"),
          add = "jitter", shape = "group") +
  labs(title="Baseline precision between groups in srt",x="Groups", y = "Precision")
ggsave('precision_srt_pre_box',device = "png", path = preFolder)

ggplot(df_precision_srt_pre,aes(x= precision_srt_pre))+
  geom_histogram(aes(color =group, fill=group),position='identity', bins=30,alpha=0.4)+
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+ 
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))+
  geom_density(aes(linetype=group),color= 'red')
ggsave('precision_srt_pre_hist',device = "png", path = preFolder)

# Check outlier(s)
df_precision_srt_pre %>% 
      group_by(group) %>% 
      identify_outliers(precision_srt_pre)
# group      precision_srt_pre Row.names                is.outlier is.extreme
# <fct>                  <dbl> <I<chr>>                 <lgl>      <lgl>     
#   1 experiment            0.0912 5da9b30a3624c200157bd9f0 TRUE       FALSE     
# 2 experiment            0.0924 5f3d71e21cdd47124dd87d88 TRUE       FALSE     
# 3 experiment            0.150  5f566c4ac45b0308940a000b TRUE       TRUE      
# 4 experiment            0.0995 5fb028d5e28a5c39c175eeda TRUE       FALSE     
# 5 control               0.127  5e8f8865b7c5540ed1d3fcd1 TRUE       FALSE     
# 6 control               0.110  5f2c18b525352005ad5e5c28 TRUE       FALSE     
# 7 control               0.121  5fbd2977679a7e08dbf5aec1 TRUE       FALSE     

# Check Normality
df_precision_srt_pre %>% 
  group_by(group) %>% 
  shapiro_test(precision_srt_pre)
ggqqplot(df_precision_srt_pre,x = "precision_srt_pre",facet.by = "group",color = "group", palette = c("#00AFBB", "#E7B800"), shape = "group")
ggsave('precision_srt_pre_qq',device = "png", path = preFolder) 
# group      variable          statistic        p
# <fct>      <chr>                 <dbl>    <dbl>
#   1 experiment precision_srt_pre     0.896 0.000720
# 2 control    precision_srt_pre     0.914 0.00144 

#Check homogeneity
df_precision_srt_pre %>% levene_test(precision_srt_pre ~ group)
# df1   df2 statistic     p
# <int> <int>     <dbl> <dbl>
#   1     1    93    0.0717 0.789

wilcox.test(df_precision_srt_pre$precision_srt_pre ~ df_precision_srt_pre$group)
rcompanion::wilcoxonR(x= df_precision_srt_pre$precision_srt_pre, g=df_precision_srt_pre$group)
# Wilcoxon rank sum test with continuity correction
# 
# data:  df_precision_srt_pre$precision_srt_pre by df_precision_srt_pre$group
# W = 1150, p-value = 0.8551
# alternative hypothesis: true location shift is not equal to 0
# r 
# 0.0191 

# odt_capacity
df_kPre <- groupedFinal %>% 
  select(group,kPre,Row.names) 

pre_summary <- rbind(pre_summary,df_kPre %>% 
                       group_by(group) %>% 
                       get_summary_stats(kPre, type = "mean_sd"))

ggboxplot(df_kPre, x = "group", y = "kPre",
          color = "group", palette =c("#00AFBB", "#E7B800"),
          add = "jitter", shape = "group") +
  labs(title="Baseline capacity between groups in odt",x="Groups", y = "Capacity (k)")
ggsave('kPre_box',device = "png", path = preFolder)

ggplot(df_kPre,aes(x= kPre))+
  geom_histogram(aes(color =group, fill=group),position='identity', bins=30,alpha=0.4)+
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+ 
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))+
  geom_density(aes(linetype=group),color= 'red')
ggsave('capacity_odt_pre_hist',device = "png", path = preFolder)

# Check outlier(s)
df_kPre %>% 
  group_by(group) %>% 
  identify_outliers(kPre)
# There were extreme outlier
# group        kPre Row.names                is.outlier is.extreme
# <fct>       <dbl> <I<chr>>                 <lgl>      <lgl>     
#   1 experiment -0.632 5fb028d5e28a5c39c175eeda TRUE       TRUE      
# 2 control     0     5f67904169a51c3b06d32fa3 TRUE       FALSE  

# Check Normality
df_kPre %>% 
  group_by(group) %>% 
  shapiro_test(kPre)
ggqqplot(df_kPre,x = "kPre",facet.by = "group",color = "group", palette = c("#00AFBB", "#E7B800"), shape = "group")
ggsave('kPre_qq',device = "png", path = preFolder)
# group      variable statistic          p
# <fct>      <chr>        <dbl>      <dbl>
#   1 experiment kPre         0.827 0.00000998
# 2 control    kPre         0.951 0.0358  

# Check homogeneity 
df_kPre %>% levene_test(kPre ~ group)

wilcox.test(df_kPre$kPre ~ df_kPre$group)
rcompanion::wilcoxonR(x= df_kPre$kPre, g=df_kPre$group)


# vs_acc
df_accPre <- groupedFinal %>% 
  select(group,accPre,Row.names) 

pre_summary <- rbind(pre_summary,df_accPre %>% 
                       group_by(group) %>% 
                       get_summary_stats(accPre, type = "mean_sd"))

ggboxplot(df_accPre, x = "group", y = "accPre",
          color = "group", palette =c("#00AFBB", "#E7B800"),
          add = "jitter", shape = "group") +
  labs(title="Baseline accuracy between groups in VS",x="Groups", y = "Accuracy")
ggsave('accPre_box',device = "png", path = preFolder)

ggplot(df_accPre,aes(x= accPre))+
  geom_histogram(aes(color =group, fill=group),position='identity', bins=30,alpha=0.4)+
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+ 
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))+
  geom_density(aes(linetype=group),color= 'red')
ggsave('acc_vs_pre_hist',device = "png", path = preFolder)

# Check outlier(s)
df_accPre %>% 
  group_by(group) %>% 
  identify_outliers(accPre)

# Check Normality
df_accPre %>% 
  group_by(group) %>% 
  shapiro_test(accPre)
ggqqplot(df_accPre,x = "accPre",facet.by = "group",color = "group", palette = c("#00AFBB", "#E7B800"), shape = "group")
ggsave('accPre_qq',device = "png", path = preFolder)


# Check homogeneity 
df_accPre %>% levene_test(accPre ~ group)


wilcox.test(df_accPre$accPre ~ df_accPre$group)
rcompanion::wilcoxonR(x= df_accPre$accPre, g=df_accPre$group)


# vs_mrt
df_mrtPre <- groupedFinal %>% 
  select(group,mrtPre) 

pre_summary <- rbind(pre_summary,df_mrtPre %>% 
                       group_by(group) %>% 
                       get_summary_stats(mrtPre, type = "mean_sd"))

ggboxplot(df_mrtPre, x = "group", y = "mrtPre",
          color = "group", palette =c("#00AFBB", "#E7B800"),
          add = "jitter", shape = "group") +
  labs(title="Baseline reaction time between groups in VS",x="Groups", y = "Reaction Time (ms)")
ggsave('mrtPre_box',device = "png", path = preFolder)

ggplot(df_mrtPre,aes(x= mrtPre))+
  geom_histogram(aes(color =group, fill=group),position='identity', bins=30,alpha=0.4)+
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+ 
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))+
  geom_density(aes(linetype=group),color= 'red')
ggsave('mrt_vs_pre_hist',device = "png", path = preFolder)
#the density line is not normal tho!!!???

# Check outlier(s)
df_mrtPre %>% 
  group_by(group) %>% 
  identify_outliers(mrtPre)
# There were extreme outliers
# group      mrtPre is.outlier is.extreme
# <fct>       <dbl> <lgl>      <lgl>     
#   1 experiment   281. TRUE       FALSE     
# 2 experiment   889. TRUE       FALSE     
# 3 experiment   627. TRUE       FALSE     
# 4 experiment   179. TRUE       FALSE     
# 5 experiment   936. TRUE       FALSE     
# 6 experiment   543. TRUE       FALSE     
# 7 control     1671. TRUE       FALSE     
# 8 control     1419. TRUE       FALSE     
# 9 control     1348. TRUE       FALSE     
# 10 control      376. TRUE       TRUE      
# 11 control      651. TRUE       TRUE      
# 12 control      434. TRUE       TRUE     

# Check Normality
df_mrtPre %>% 
  group_by(group) %>% 
  shapiro_test(mrtPre)
ggqqplot(df_mrtPre,x = "mrtPre",facet.by = "group",color = "group", palette = c("#00AFBB", "#E7B800"), shape = "group")
ggsave('mrtPre_qq',device = "png", path = preFolder)


# Check homogeneity 
df_mrtPre %>% levene_test(mrtPre ~ group)


wilcox.test(df_mrtPre$mrtPre ~ df_mrtPre$group)
rcompanion::wilcoxonR(x= df_mrtPre$mrtPre, g=df_mrtPre$group)


# two-way mixed anova in prepost####

prePost <- groupedFinal %>% 
  select(group,accPre,mrtPre,kPre,accPost,mrtPost,kPost,capacity_ort_pre,capacity_srt_pre,precision_ort_pre,precision_srt_pre,capacity_ort_post,capacity_srt_post,precision_ort_post,precision_srt_post,Row.names) %>% 
  group_by(group)

# capacity in ort ####
capOrt <- prePost %>% select(capacity_ort_pre,capacity_ort_post,group,Row.names) %>% 
  gather(key = "session", value = "score",capacity_ort_post, capacity_ort_pre) %>% 
  convert_as_factor(session) %>%  
  mutate(time=fct_relevel(session, 'capacity_ort_pre','capacity_ort_post')) %>% 
  group_by(time,group)

bxp <- ggboxplot(
  capOrt, x = 'time', y = "score", group = "session",
  color = "group", palette =c("#00AFBB", "#E7B800"),
  add = "jitter", shape = "group") +
  labs(title="Training-induced changes in capacity in ORT", x= "Time", y = "Capacity")+
  scale_x_discrete(labels=c("capacity_ort_pre"= "pre test",'capacity_ort_post'='post test'))
ggsave('ort_capacity_prepost',device = "png", path = prePostFolder)

summary <- capOrt %>% get_summary_stats(score, type = "mean_sd")


# outfilters
capOrt %>%
  group_by(time, group) %>%
  identify_outliers(score)
# group      time              session           score is.outlier is.extreme
# <fct>      <fct>             <fct>             <dbl> <lgl>      <lgl>     
#   1 experiment capacity_ort_pre  capacity_ort_pre  0.152 TRUE       FALSE     
# 2 experiment capacity_ort_post capacity_ort_post 0.345 TRUE       FALSE     
# 3 experiment capacity_ort_post capacity_ort_post 0.356 TRUE       FALSE     
# 4 control    capacity_ort_post capacity_ort_post 0.417 TRUE       FALSE     
# 5 control    capacity_ort_post capacity_ort_post 0.660 TRUE       FALSE 

#Normality:If the data is normally distributed, the p-value should be greater than 0.05.Note that, if your sample size is greater than 50, the normal QQ plot is preferred because at larger sample sizes the Shapiro-Wilk test becomes very sensitive even to a minor deviation from normality.
capOrt%>%
  group_by(time, group) %>%
  shapiro_test(score)
ggqqplot(capOrt, "score", ggtheme = theme_bw()) +
  facet_grid(time ~ group)
ggsave('ort_capacity_prepost_qq',device = "png", path = prePostFolder)
 

#Homogneity of variance assumption. There was homogeneity of variances, as assessed by Levene’s test (p > 0.05).
capOrt %>%
  group_by(time) %>%
  levene_test(score ~ group)

#keep the outliers in the data and perform robust ANOVA test in the situation where the assumptions are not me
bwtrim(formula = score ~ group * time,id = Row.names, data = capOrt)
# value df1     df2 p.value
# group       2.1150   1 51.4062  0.1519
# time       20.3025   1 50.4951  0.0000
# group:time  1.2639   1 50.4951  0.2662
sppbb(formula = score ~ group * time,id = Row.names, data = capOrt)
# Call:
#   sppbb(formula = score ~ group * time, id = Row.names, data = capOrt)
# 
# Test statistics:
#   Estimate
# capacity_ort_pre-capacity_ort_post  -0.2597
# 
# Test whether the corrresponding population parameters are the same:
#   p-value: 0 

akp.effect(formula = score ~  time, data = capOrt)
# $AKPeffect
# [1] -0.3356127
# 
# $AKPci
# [1] -0.63533108 -0.07511554
# 
# $call
# akp.effect(formula = score ~ time, data = capOrt)
# 
# attr(,"class")
# [1] "AKP"


#Two-way mixed ANOVA test
summary(capOrt)
two.way <- aov(score ~ group + time, data = capOrt)
summary(two.way)
interaction <- aov(score ~ group * time, data = capOrt)
summary(interaction)


#precision in ort####
preOrt <- prePost %>% select(precision_ort_pre,precision_ort_post,group,Row.names) %>% 
  gather(key = "session", value = "score",precision_ort_post, precision_ort_pre) %>% 
  convert_as_factor(session) %>%  
  mutate(time=fct_relevel(session, 'precision_ort_pre','precision_ort_post')) %>% 
  group_by(time,group)

bxp <- ggboxplot(
  preOrt, x = 'time', y = "score", group = "session",
  color = "group", palette =c("#00AFBB", "#E7B800"),
  add = "jitter", shape = "group") +
  labs(title="Training-induced changes in precision in ORT", x= "Time", y = "precision")+
  scale_x_discrete(labels=c("precision_ort_pre"= "pre test",'precision_ort_post'='post test'))
bxp
ggsave('ort_precision_prepost',device = "png", path = prePostFolder)

summary <- rbind(summary, preOrt %>% get_summary_stats(score, type = "mean_sd"))

# outfilters
preOrt %>%
  group_by(time, group) %>%
  identify_outliers(score)
# group      time             Row.names             session            score is.outlier is.extreme
# <fct>      <fct>            <I<chr>>              <fct>              <dbl> <lgl>      <lgl>     
#   1 experiment precision_ort_p… 5da9b30a3624c200157b… precision_ort_p… 0.0969  TRUE       FALSE     
# 2 control    precision_ort_p… 5bc7033bd2bea100010c… precision_ort_p… 0.120   TRUE       TRUE      
# 3 control    precision_ort_p… 5e84f5b5f9dd660d5ee7… precision_ort_p… 0.122   TRUE       TRUE      
# 4 experiment precision_ort_p… 5eaeb8ccea726058c6eb… precision_ort_p… 0.00762 TRUE       FALSE     
# 5 control    precision_ort_p… 5f5c0a54a38ae61d4c3b… precision_ort_p… 0.0941  TRUE       FALSE  

#Normality:If the data is normally distributed, the p-value should be greater than 0.05.Note that, if your sample size is greater than 50, the normal QQ plot is preferred because at larger sample sizes the Shapiro-Wilk test becomes very sensitive even to a minor deviation from normality.
preOrt%>%
  group_by(time, group) %>%
  shapiro_test(score)
ggqqplot(preOrt, "score", ggtheme = theme_bw()) +
  facet_grid(time ~ group)
ggsave('ort_precision_prepost_qq',device = "png", path = prePostFolder)

#Homogneity of variance assumption. There was homogeneity of variances, as assessed by Levene’s test (p > 0.05).
preOrt %>%
  group_by(time) %>%
  levene_test(score ~ group)

# Robust ANOVA
bwtrim(formula = score ~ group * time,id = Row.names, data = preOrt)
# value df1     df2 p.value
# group       4.1219   1 48.9459  0.0478
# time        3.0464   1 49.2129  0.0872
# group:time 18.4953   1 49.2129  0.0001

sppbi(formula = score ~ group * time,id = Row.names, data = preOrt)
# Test statistics:
#   Estimate
# precision_ort_pre-precision_ort_post experiment-control -0.01749
# 
# Test whether the corrresponding population parameters are the same:
#   p-value: 0 

akp.effect(formula = score ~ group * time, data = preOrt)
akp.effect(formula = score ~ group , data = preOrt)
akp.effect(formula = score ~time, data = preOrt)

# Two-way mixed ANOVA test
summary(preOrt)
two.way <- aov(score ~ group + time, data = preOrt)
summary(two.way)
interaction <- aov(score ~ group * time, data = preOrt)
summary(interaction)


# Effect of group at each time point
one.way <- preOrt %>% 
  group_by(time) %>% 
  anova_test(dv = score, wid = Row.names, between = group) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way
# time               Effect   DFn   DFd      F     p `p<.05`   ges p.adj
# <fct>              <chr>  <dbl> <dbl>  <dbl> <dbl> <chr>   <dbl> <dbl>
#   1 precision_ort_pre  group      1    94  0.633 0.428 ""      0.007 0.856
# 2 precision_ort_post group      1    94 11.2   0.001 "*"     0.106 0.002
# simple main effect of group is at post

#comparisons between group levels
preOrt %>%
  group_by(time) %>%
  pairwise_t_test(score ~ group,p.adjust.method = "bonferroni")

# time               .y.   group1     group2     n1    n2       p p.signif   p.adj p.adj.signif
# * <fct>              <chr> <chr>      <chr>   <int> <int>   <dbl> <chr>      <dbl> <chr>       
#   1 precision_ort_pre  score experiment control    46    50 0.428   ns       0.428   ns          
# 2 precision_ort_post score experiment control    46    50 0.00118 **       0.00118 **  
#pairwise comparisons shows that mean score is different between two groups at post

#effect of time at each level of group
preOrt <- select(preOrt,-session)
one.way2 <-preOrt %>%
  group_by(group) %>% 
  anova_test(dv = score, wid = Row.names, within= time) %>% 
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way2
# group      Effect   DFn   DFd     F     p `p<.05`   ges p.adj
# <fct>      <chr>  <dbl> <dbl> <dbl> <dbl> <chr>   <dbl> <dbl>
#   1 experiment time       1    45  9.30 0.004 "*"     0.067 0.008
# 2 control    time       1    49  3.62 0.063 ""      0.022 0.126
# Main time effect at experiment group

# Pairwise comparisons between time points at each group levels
# Paired t-test is used because we have repeated measures by time
 pwc2 <- preOrt %>%
  group_by(group) %>%
  pairwise_t_test(
    score ~ time, paired = TRUE, 
    p.adjust.method = "bonferroni"
  ) 
 
 preOrt %>%
   group_by(group) %>% 
   cohens_d(score ~ time, paired = TRUE)
# # A tibble: 2 x 8
# group      .y.   group1            group2                n1    n2 p.adj p.adj.signif
# * <fct>      <chr> <chr>             <chr>              <int> <int> <dbl> <chr>       
#   1 experiment score precision_ort_pre precision_ort_post    46    46 0.004 **          
#   2 control    score precision_ort_pre precision_ort_post    50    50 0.063 ns 
#experiment group pre vs post were sig different ]
 
 # .y.   group1            group2             effsize group         n1    n2 magnitude
 # * <chr> <chr>             <chr>                <dbl> <fct>      <int> <int> <ord>    
 #   1 score precision_ort_pre precision_ort_post  -0.450 experiment    46    46 small    
 # 2 score precision_ort_pre precision_ort_post   0.269 control       50    50 small    
 # 

#capacity in srt####
capSrt <- prePost %>% select(capacity_srt_pre,capacity_srt_post,group,Row.names) %>% 
  gather(key = "session", value = "score",capacity_srt_post, capacity_srt_pre) %>% 
  convert_as_factor(session) %>%  
  mutate(time=fct_relevel(session, 'capacity_srt_pre','capacity_srt_post')) %>% 
  group_by(time,group)

bxp <- ggboxplot(
  capSrt, x = 'time', y = "score", group = "session",
  color = "group", palette =c("#00AFBB", "#E7B800"),
  add = "jitter", shape = "group") +
  labs(title="Training-induced changes in capacity in srt", x= "Time", y = "capacity")+
  scale_x_discrete(labels=c("capacity_srt_pre"= "pre test",'capacity_srt_post'='post test'))
bxp
ggsave('srt_capacity_prepost',device = "png", path = prePostFolder)

summary <- rbind(summary, capSrt %>% get_summary_stats(score, type = "mean_sd"))


# outfilters
capSrt %>%
  group_by(time, group) %>%
  identify_outliers(score)
# group   time              Row.names                session            score is.outlier is.extreme
# <fct>   <fct>             <I<chr>>                 <fct>              <dbl> <lgl>      <lgl>     
#   1 control capacity_srt_pre  5e84f5b5f9dd660d5ee7437a capacity_srt_pre  4.00   TRUE       FALSE     
# 2 control capacity_srt_post 5f636c7645f61e036f74ccbf capacity_srt_post 0.0503 TRUE       FALSE 

#Normality:If the data is normally distributed, the p-value should be greater than 0.05.Note that, if your sample size is greater than 50, the normal QQ plot is preferred because at larger sample sizes the Shapiro-Wilk test becomes very sensitive even to a minor deviation from normality.
capSrt%>%
  group_by(time, group) %>%
  shapiro_test(score)
ggqqplot(capSrt, "score", ggtheme = theme_bw()) +
  facet_grid(time ~ group)
ggsave('srt_capacity_prepost_qq',device = "png", path = prePostFolder)
# group      time              variable statistic     p
# <fct>      <fct>             <chr>        <dbl> <dbl>
#   1 experiment capacity_srt_pre  score        0.982 0.688
# 2 control    capacity_srt_pre  score        0.991 0.967
# 3 experiment capacity_srt_post score        0.988 0.920
# 4 control    capacity_srt_post score        0.975 0.363

#Homogneity of variance assumption. There was homogeneity of variances, as assessed by Levene’s test (p > 0.05).
capSrt %>%
  group_by(time) %>%
  levene_test(score ~ group)
# time                df1   df2 statistic     p
# <fct>             <int> <int>     <dbl> <dbl>
#   1 capacity_srt_pre      1    94     2.11  0.150
# 2 capacity_srt_post     1    94     0.413 0.522

# Robust ANOVA
bwtrim(formula = score ~ group * time,id = Row.names, data = capSrt)

# Two-way mixed ANOVA test
summary(capSrt)
two.way <- aov(score ~ group + time, data = capSrt)
summary(two.way)
interaction <- aov(score ~ group * time, data = capSrt)
summary(interaction)
# Df Sum Sq Mean Sq F value Pr(>F)
# group         1   0.22  0.2182   0.339  0.561
# time          1   0.02  0.0229   0.036  0.851
# group:time    1   0.18  0.1799   0.279  0.598
# Residuals   188 121.01  0.6437   




#precision in srt####
preSrt <- prePost %>% select(precision_srt_pre,precision_srt_post,group,Row.names) %>% 
  gather(key = "session", value = "score",precision_srt_post, precision_srt_pre) %>% 
  convert_as_factor(session) %>%  
  mutate(time=fct_relevel(session, 'precision_srt_pre','precision_srt_post')) %>% 
  group_by(time,group)

ggboxplot(
  preSrt, x = 'time', y = "score", group = "session",
  color = "group", palette =c("#00AFBB", "#E7B800"),
  add = "jitter", shape = "group") +
  labs(title="Training-induced changes in precision in srt", x= "Time", y = "precision")+
  scale_x_discrete(labels=c("precision_srt_pre"= "pre test",'precision_srt_post'='post test'))+
  coord_cartesian(ylim=c(0,0.25)) 
  scale_y_continuous(breaks=seq(0, 0.25, 0.005))
ggsave('srt_precision_prepost',device = "png", path = prePostFolder)

summary <- rbind(summary, preSrt %>% get_summary_stats(score, type = "mean_sd"))

# outfilters
preSrt %>%
  group_by(time, group) %>%
  identify_outliers(score)
# group      time               Row.names                session             score is.outlier is.extreme
# <fct>      <fct>              <I<chr>>                 <fct>               <dbl> <lgl>      <lgl>     
#   1 experiment precision_srt_pre  5da9b30a3624c200157bd9f0 precision_srt_pre  0.0912 TRUE       FALSE     
# 2 experiment precision_srt_pre  5f3d71e21cdd47124dd87d88 precision_srt_pre  0.0924 TRUE       FALSE     
# 3 experiment precision_srt_pre  5f566c4ac45b0308940a000b precision_srt_pre  0.150  TRUE       TRUE      
# 4 experiment precision_srt_pre  5fb028d5e28a5c39c175eeda precision_srt_pre  0.0995 TRUE       FALSE     
# 5 control    precision_srt_pre  5e8f8865b7c5540ed1d3fcd1 precision_srt_pre  0.127  TRUE       FALSE     
# 6 control    precision_srt_pre  5f2c18b525352005ad5e5c28 precision_srt_pre  0.110  TRUE       FALSE     
# 7 control    precision_srt_pre  5fbd2977679a7e08dbf5aec1 precision_srt_pre  0.121  TRUE       FALSE     
# 8 control    precision_srt_post 5ddfc5a904ba3e03e96a5bd7 precision_srt_post 0.162  TRUE       TRUE      
# 9 control    precision_srt_post 5f0d98c1c8a2f100082295b8 precision_srt_post 0.161  TRUE       TRUE      
# 10 control    precision_srt_post 5f25b1d39ef53b275e113a56 precision_srt_post 0.113  TRUE       FALSE     
# 11 control    precision_srt_post 5f636c7645f61e036f74ccbf precision_srt_post 3.18   TRUE       TRUE      
# 12 control    precision_srt_post 5fbd2977679a7e08dbf5aec1 precision_srt_post 0.111  TRUE       FALSE  


#Normality:If the data is normally distributed, the p-value should be greater than 0.05.Note that, if your sample size is greater than 50, the normal QQ plot is preferred because at larger sample sizes the Shapiro-Wilk test becomes very sensitive even to a minor deviation from normality.
preSrt%>%
  group_by(time, group) %>%
  shapiro_test(score)
ggqqplot(preSrt, "score", ggtheme = theme_bw()) +
  facet_grid(time ~ group)
ggsave('srt_precision_prepost_qq',device = "png", path = prePostFolder)
# group      time               variable statistic        p
# <fct>      <fct>              <chr>        <dbl>    <dbl>
#   1 experiment precision_srt_pre  score        0.893 5.00e- 4
# 2 control    precision_srt_pre  score        0.914 1.44e- 3
# 3 experiment precision_srt_post score        0.950 4.72e- 2
# 4 control    precision_srt_post score        0.170 2.76e-15

#Homogneity of variance assumption. There was homogeneity of variances, as assessed by Levene’s test (p > 0.05).
preSrt %>%
  group_by(time) %>%
  levene_test(score ~ group)
# time                 df1   df2 statistic     p
# <fct>              <int> <int>     <dbl> <dbl>
#   1 precision_srt_pre      1    94    0.0947 0.759
# 2 precision_srt_post     1    94    0.997  0.321

# Robust ANOVA
bwtrim(formula = score ~ group * time,id = Row.names, data = preSrt)
# Call:
#   bwtrim(formula = score ~ group * time, id = Row.names, data = preSrt)
# 
# value df1     df2 p.value
# group      0.6882   1 53.9906  0.4104
# time       0.0009   1 53.5534  0.9762
# group:time 1.1715   1 53.5534  0.2839

# Two-way mixed ANOVA test
summary(preSrt)
two.way <- aov(score ~ group + time, data = preSrt)
summary(two.way)
interaction <- aov(score ~ group * time, data = preSrt)
summary(interaction)
# Df Sum Sq Mean Sq F value Pr(>F)
# group         1  0.043 0.04332   0.835  0.362
# time          1  0.054 0.05403   1.042  0.309
# group:time    1  0.042 0.04169   0.804  0.371
# Residuals   188  9.752 0.05187 


# accurracy in VS####
accVS <- prePost %>% select(accPre,accPost,group,Row.names) %>% 
  gather(key = "session", value = "score",accPost, accPre) %>% 
  convert_as_factor(session) %>%  
  mutate(time=fct_relevel(session, 'accPre','accPost')) %>% 
  group_by(time,group)

ggboxplot(
  accVS, x = 'time', y = "score", group = "session",
  color = "group", palette =c("#00AFBB", "#E7B800"),
  add = "jitter", shape = "group") +
  labs(title="Training-induced changes in VS", x= "Time", y = "Accuracy")+
  scale_x_discrete(labels=c("accPre"= "pre test",'accPost'='post test'))
ggsave('vs_acc_prepost',device = "png", path = prePostFolder)

summary <- rbind(summary, accVS %>% get_summary_stats(score, type = "mean_sd"))

# outfilters
accVS %>%
  group_by(time, group) %>%
  identify_outliers(score)
# group      time    Row.names                session score is.outlier is.extreme
# <fct>      <fct>   <I<chr>>                 <fct>   <dbl> <lgl>      <lgl>     
#   1 experiment accPre  5ea93196653dfc000b23516b accPre  0.275 TRUE       FALSE     
# 2 control    accPre  5ee10d5b0017d10ec8702d10 accPre  0.258 TRUE       FALSE     
# 3 control    accPre  5f0d98c1c8a2f100082295b8 accPre  0.283 TRUE       FALSE     
# 4 control    accPre  5fa0100e226a1a000c4c19c3 accPre  0.367 TRUE       FALSE     
# 5 experiment accPost 5eaeb8ccea726058c6eb9a16 accPost 0.533 TRUE       FALSE     
# 6 experiment accPost 5f2be37b1e52920009dadba6 accPost 0.5   TRUE       FALSE     
# 7 experiment accPost 5f5b7da324ea690ab36f58a5 accPost 0.408 TRUE       FALSE     
# 8 experiment accPost 5fad4db063bdee02d14d9511 accPost 0.217 TRUE       TRUE      
# 9 control    accPost 5cb1ed1ef3410900174a9838 accPost 0.542 TRUE       FALSE     
# 10 control    accPost 5f636c7645f61e036f74ccbf accPost 0.5   TRUE       FALSE 

#Normality:If the data is normally distributed, the p-value should be greater than 0.05.Note that, if your sample size is greater than 50, the normal QQ plot is preferred because at larger sample sizes the Shapiro-Wilk test becomes very sensitive even to a minor deviation from normality.
accVS%>%
  group_by(time, group) %>%
  shapiro_test(score)
ggqqplot(accVS, "score", ggtheme = theme_bw()) +
  facet_grid(time ~ group)
ggsave('vs_acc_prepost_qq',device = "png", path = prePostFolder)
# group      time    variable statistic          p
# <fct>      <fct>   <chr>        <dbl>      <dbl>
#   1 experiment accPre  score        0.906 0.00129   
# 2 control    accPre  score        0.852 0.0000174 
# 3 experiment accPost score        0.825 0.00000725
# 4 control    accPost score        0.897 0.000391

#Homogneity of variance assumption. There was homogeneity of variances, as assessed by Levene’s test (p > 0.05).
accVS %>%
  group_by(time) %>%
  levene_test(score ~ group)
# time      df1   df2 statistic     p
# <fct>   <int> <int>     <dbl> <dbl>
#   1 accPre      1    94     1.14  0.289
# 2 accPost     1    94     0.647 0.423

# Robust ANOVA
bwtrim(formula = score ~ group * time,id = Row.names, data = accVS)
# Call:
#   bwtrim(formula = score ~ group * time, id = Row.names, data = accVS)
# 
# value df1     df2 p.value
# group       1.4282   1 49.3328  0.2378
# time       43.7691   1 53.9221  0.0000
# group:time  2.2129   1 53.9221  0.1427

# Two-way mixed ANOVA test
summary(accVS)
two.way <- aov(score ~ group + time, data = accVS)
summary(two.way)
interaction <- aov(score ~ group * time, data = accVS)
summary(interaction)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# group         1  0.064  0.0636    3.04   0.0829 .  
# time          1  0.395  0.3953   18.91 2.25e-05 ***
#   group:time    1  0.028  0.0278    1.33   0.2502    
# Residuals   188  3.931  0.0209                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1




# Reaction time  in VS####
mrtVS <- prePost %>% select(mrtPre,mrtPost,group,Row.names) %>% 
  gather(key = "session", value = "score",mrtPost, mrtPre) %>% 
  convert_as_factor(session) %>%  
  mutate(time=fct_relevel(session, 'mrtPre','mrtPost')) %>% 
  group_by(time,group)

ggboxplot(
  mrtVS, x = 'time', y = "score", group = "session",
  color = "group", palette =c("#00AFBB", "#E7B800"),
  add = "jitter", shape = "group") +
  labs(title="Training-induced changes in VS", x= "Time", y = "Reaction Time")+
  scale_x_discrete(labels=c("mrtPre"= "pre test",'mrtPost'='post test'))
ggsave('vs_mrt_prepost',device = "png", path = prePostFolder)

summary <- rbind(summary, mrtVS %>% get_summary_stats(score, type = "mean_sd"))


# outfilters
mrtVS %>%
  group_by(time, group) %>%
  identify_outliers(score)
# 
# group      time    Row.names                session score is.outlier is.extreme
# <fct>      <fct>   <I<chr>>                 <fct>   <dbl> <lgl>      <lgl>     
#   1 experiment mrtPre  5da9b30a3624c200157bd9f0 mrtPre   281. TRUE       FALSE     
# 2 experiment mrtPre  5dae5c7158a2e500170f1cd8 mrtPre   889. TRUE       FALSE     
# 3 experiment mrtPre  5ee55de461410f0f2bc3977a mrtPre   627. TRUE       FALSE     
# 4 experiment mrtPre  5f566c4ac45b0308940a000b mrtPre   179. TRUE       FALSE     
# 5 experiment mrtPre  5f6247a964b0fe2746fb33d5 mrtPre   936. TRUE       FALSE     
# 6 experiment mrtPre  5fad4db063bdee02d14d9511 mrtPre   543. TRUE       FALSE     
# 7 control    mrtPre  5e84f5b5f9dd660d5ee7437a mrtPre  1671. TRUE       FALSE     
# 8 control    mrtPre  5ee10d5b0017d10ec8702d10 mrtPre  1419. TRUE       FALSE     
# 9 control    mrtPre  5f0d98c1c8a2f100082295b8 mrtPre  1348. TRUE       FALSE     
# 10 control    mrtPre  5f2c18b525352005ad5e5c28 mrtPre   376. TRUE       TRUE      
# 11 control    mrtPre  5f636c7645f61e036f74ccbf mrtPre   651. TRUE       TRUE      
# 12 control    mrtPre  5f64c18a0d65c4021a1c3370 mrtPre   434. TRUE       TRUE      
# 13 experiment mrtPost 5dae5c7158a2e500170f1cd8 mrtPost 1452. TRUE       FALSE     
# 14 experiment mrtPost 5eaeb8ccea726058c6eb9a16 mrtPost  292. TRUE       TRUE      
# 15 experiment mrtPost 5f2be37b1e52920009dadba6 mrtPost 1111. TRUE       FALSE     
# 16 experiment mrtPost 5fad4db063bdee02d14d9511 mrtPost 1248. TRUE       FALSE     
# 17 control    mrtPost 5f636c7645f61e036f74ccbf mrtPost  788. TRUE       FALSE

#Normality:If the data is normally distributed, the p-value should be greater than 0.05.Note that, if your sample size is greater than 50, the normal QQ plot is preferred because at larger sample sizes the Shapiro-Wilk test becomes very sensitive even to a minor deviation from normality.
mrtVS%>%
  group_by(time, group) %>%
  shapiro_test(score)
ggqqplot(mrtVS, "score", ggtheme = theme_bw()) +
  facet_grid(time ~ group)
ggsave('vs_mrt_prepost_qq',device = "png", path = prePostFolder)
# group      time    variable statistic           p
# <fct>      <fct>   <chr>        <dbl>       <dbl>
#   1 experiment mrtPre  score        0.783 0.000000831
# 2 control    mrtPre  score        0.795 0.000000684
# 3 experiment mrtPost score        0.841 0.0000183  
# 4 control    mrtPost score        0.967 0.176 


#Homogneity of variance assumption. There was homogeneity of variances, as assessed by Levene’s test (p > 0.05).
mrtVS %>%
  group_by(time) %>%
  levene_test(score ~ group)
# time      df1   df2 statistic     p
# <fct>   <int> <int>     <dbl> <dbl>
#   1 mrtPre      1    94     0.696 0.406
# 2 mrtPost     1    94     0.600 0.441
# Robust ANOVA
bwtrim(formula = score ~ group * time,id = Row.names, data = mrtVS)
# Call:
#   bwtrim(formula = score ~ group * time, id = Row.names, data = mrtVS)
# 
# value df1     df2 p.value
# group       2.8808   1 52.7213  0.0955
# time       16.7488   1 53.9719  0.0001 ***?????!!!!!
# group:time 10.1052   1 53.9719  0.0024 ***?????!!!!!
sppba(formula = score ~ group * time, id=Row.names, data = mrtVS, avg = FALSE)
sppbi(formula = score ~ group * time, id=Row.names, data = mrtVS)
sppbb(formula = score ~ group * time, id=Row.names, data = mrtVS)
# Call:
#   sppba(formula = score ~ group * time, id = Row.names, data = mrtVS, 
#         avg = FALSE)
# 
# Test statistics:
#   Estimate
# mrtPre experiment-control     7.706
# mrtPost experiment-control  442.631
# 
# Test whether the corrresponding population parameters are the same:
#   p-value: 0.008 


# Two-way mixed ANOVA test
summary(mrtVS)
two.way <- aov(score ~ group + time, data = mrtVS)
summary(two.way)
interaction <- aov(score ~ group * time, data = mrtVS)
summary(interaction)
# Df    Sum Sq Mean Sq F value Pr(>F)
# group         1    443109  443109   0.686  0.409
# time          1    715383  715383   1.108  0.294
# group:time    1   1406265 1406265   2.178  0.142
# Residuals   188 121411946  645808 

# time effect for robust anova???!!!

# Effect of group at each time point
one.way <- mrtVS %>%
  group_by(time) %>%
  anova_test(dv = score, wid = Row.names, between = group) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way
# time    Effect   DFn   DFd     F     p `p<.05`   ges p.adj
# <fct>   <chr>  <dbl> <dbl> <dbl> <dbl> <chr>   <dbl> <dbl>
#   1 mrtPre  group      1    94 0.159 0.691 ""      0.002 1    
# 2 mrtPost group      1    94 3.9   0.051 ""      0.04  0.102

# Pairwise comparisons between group levels
pwc <- mrtVS %>%
  group_by(time) %>%
  pairwise_t_test(score ~ group, p.adjust.method = "bonferroni")
pwc
# time    .y.   group1     group2     n1    n2      p p.signif  p.adj p.adj.signif
# * <fct>   <chr> <chr>      <chr>   <int> <int>  <dbl> <chr>     <dbl> <chr>       
#   1 mrtPre  score experiment control    46    50 0.691  ns       0.691  ns          
# 2 mrtPost score experiment control    46    50 0.0512 ns       0.0512 ns

# Effect of time at each level of exercises group
mrtVS <- select(mrtVS,-session)
one.way2 <- mrtVS %>%
  group_by(group) %>%
  anova_test(dv = score, wid = Row.names, within = time) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way2
# group      Effect   DFn   DFd     F     p `p<.05`   ges p.adj
# <fct>      <chr>  <dbl> <dbl> <dbl> <dbl> <chr>   <dbl> <dbl>
#   1 experiment time       1    45 0.196  0.66 ""      0.001  1   
# 2 control    time       1    49 5.76   0.02 "*"     0.039  0.04
# Pairwise comparisons between time points at each group levels
# Paired t-test is used because we have repeated measures by time
pwc2 <- mrtVS %>%
  group_by(group) %>%
  pairwise_t_test(
    score ~ time, paired = TRUE, 
    p.adjust.method = "bonferroni"
  ) 
pwc2
# group      .y.   group1 group2     n1    n2 p.adj p.adj.signif
# * <fct>      <chr> <chr>  <chr>   <int> <int> <dbl> <chr>       
#   1 experiment score mrtPre mrtPost    46    46  0.66 ns          
# 2 control    score mrtPre mrtPost    50    50  0.02 *    

# capacity  in ODT####
kODT <- prePost %>% select(kPre,kPost,group,Row.names) %>% 
  gather(key = "session", value = "score",kPost, kPre) %>% 
  convert_as_factor(session) %>%  
  mutate(time=fct_relevel(session, 'kPre','kPost')) %>% 
  group_by(time,group)

ggboxplot(
  kODT, x = 'time', y = "score", group = "session",
  color = "group", palette =c("#00AFBB", "#E7B800"),
  add = "jitter", shape = "group") +
  labs(title="Training-induced changes in ODT", x= "Time", y = "capacity")+
  scale_x_discrete(labels=c("kPre"= "pre test",'kPost'='post test'))
ggsave('ODT_k_prepost',device = "png", path = prePostFolder)

summary <- rbind(summary, kODT %>% get_summary_stats(score, type = "mean_sd"))

# outfilters
kODT %>%
  group_by(time, group) %>%
  identify_outliers(score)
# 1 experiment kPre  5fb028d5e28a5c39c175eeda kPre    -0.632  TRUE       TRUE      
# 2 control    kPre  5f67904169a51c3b06d32fa3 kPre     0      TRUE       FALSE     
# 3 control    kPost 5cb1ed1ef3410900174a9838 kPost    0.025  TRUE       FALSE     
# 4 control    kPost 5f67904169a51c3b06d32fa3 kPost   -0.0789 TRUE       FALSE     

#Normality:If the data is normally distributed, the p-value should be greater than 0.05.Note that, if your sample size is greater than 50, the normal QQ plot is preferred because at larger sample sizes the Shapiro-Wilk test becomes very sensitive even to a minor deviation from normality.
kODT%>%
  group_by(time, group) %>%
  shapiro_test(score)
ggqqplot(kODT, "score", ggtheme = theme_bw()) +
  facet_grid(time ~ group)
ggsave('ODT_k_prepost_qq',device = "png", path = prePostFolder)
# group      time  variable statistic          p
# <fct>      <fct> <chr>        <dbl>      <dbl>
#   1 experiment kPre  score        0.825 0.00000732
# 2 control    kPre  score        0.951 0.0358    
# 3 experiment kPost score        0.963 0.145     
# 4 control    kPost score        0.913 0.00133  

#Homogneity of variance assumption. There was homogeneity of variances, as assessed by Levene’s test (p > 0.05).
kODT %>%
  group_by(time) %>%
  levene_test(score ~ group)
# time    df1   df2 statistic     p
# <fct> <int> <int>     <dbl> <dbl>
#   1 kPre      1    94     0.285 0.595
# 2 kPost     1    94     1.64  0.204

# Robust ANOVA
bwtrim(formula = score ~ group * time,id = Row.names, data = kODT)
# Call:
#   bwtrim(formula = score ~ group * time, id = Row.names, data = kODT)
# 
# value df1     df2 p.value
# group      2.5701   1 52.1907  0.1149
# time       1.9796   1 53.9999  0.1652
# group:time 1.4401   1 53.9999  0.2354

# Two-way mixed ANOVA test
summary(kODT)
two.way <- aov(score ~ group + time, data = kODT)
summary(two.way)
interaction <- aov(score ~ group * time, data = kODT)
summary(interaction)
# Df Sum Sq Mean Sq F value Pr(>F)  
# group         1  0.125 0.12547   3.058  0.082 .
# time          1  0.024 0.02415   0.588  0.444  
# group:time    1  0.033 0.03325   0.810  0.369  
# Residuals   188  7.714 0.04103                 
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1













# during trainings with an extrem outlier####
# During trainings in experimental group ####
capT <- groupedFinal %>% 
  filter(group == 'experiment') %>% 
  select(Row.names, capacity_ortT_t1_ss2,capacity_ortT_t2_ss2,capacity_ortT_t3_ss2,capacity_ortT_t4_ss2,capacity_ortT_t1_ss4,capacity_ortT_t2_ss4,capacity_ortT_t3_ss4,capacity_ortT_t4_ss4,capacity_ortT_t1_ss6,capacity_ortT_t2_ss6,capacity_ortT_t3_ss6,capacity_ortT_t4_ss6)

capT2 <- capT %>% 
  gather(key = "type", value = "score",  capacity_ortT_t1_ss2,capacity_ortT_t2_ss2,capacity_ortT_t3_ss2,capacity_ortT_t4_ss2,capacity_ortT_t1_ss4,capacity_ortT_t2_ss4,capacity_ortT_t3_ss4,capacity_ortT_t4_ss4,capacity_ortT_t1_ss6,capacity_ortT_t2_ss6,capacity_ortT_t3_ss6,capacity_ortT_t4_ss6)

capT3 <- capT2 %>% 
  mutate(time = substr(type, 15, 16)) %>% 
  mutate(setsize = substr(type, 20, 20))

duringORTSummary <- capT3 %>% 
  group_by(setsize,time) %>% 
  get_summary_stats(score,type='mean_sd')

ggboxplot(
  capT3, x = 'time', y = "score", group = "setsize",
  color = "setsize", palette ="jco") +
  labs(title="Capacity changes during training", x= "Time", y = "Capacity")
ggsave('During ORT',device = "png", path = prePostFolder)        
 
#outliers
capT3 %>% 
  group_by(setsize,time) %>% 
  identify_outliers(score)
# time  setsize Row.names                type                 score is.outlier is.extreme
# <chr> <chr>   <I<chr>>                 <chr>                <dbl> <lgl>      <lgl>     
#   1 t1    2       5da9b30a3624c200157bd9f0 capacity_ortT_t1_ss2 1.57  TRUE       FALSE     
# 2 t1    2       5dfe7de0bf3d4eb1cc979a7a capacity_ortT_t1_ss2 1.54  TRUE       FALSE     
# 3 t1    2       5fad4db063bdee02d14d9511 capacity_ortT_t1_ss2 1.47  TRUE       FALSE     
# 4 t1    2       5fb028d5e28a5c39c175eeda capacity_ortT_t1_ss2 1.32  TRUE       TRUE      
# 5 t2    2       5da9b30a3624c200157bd9f0 capacity_ortT_t2_ss2 0.615 TRUE       TRUE      
# 6 t2    2       5eaeb8ccea726058c6eb9a16 capacity_ortT_t2_ss2 1.31  TRUE       TRUE      
# 7 t2    2       5f0b8fb4f142602fe1db398b capacity_ortT_t2_ss2 1.55  TRUE       FALSE     
# 8 t2    2       5f2be37b1e52920009dadba6 capacity_ortT_t2_ss2 1.14  TRUE       TRUE      
# 9 t2    2       5f411ec87e08c606616559cb capacity_ortT_t2_ss2 1.35  TRUE       TRUE      
# 10 t2    2       5fad4db063bdee02d14d9511 capacity_ortT_t2_ss2 1.44  TRUE       TRUE  
# … with 13 more rows

# normality
capT3 %>% 
  group_by(setsize,time) %>% 
  shapiro_test(score)
# time  setsize variable statistic        p
# <chr> <chr>   <chr>        <dbl>    <dbl>
#   1 t1    2       score        0.746 1.48e- 7
# 2 t2    2       score        0.618 1.05e- 9
# 3 t3    2       score        0.644 2.60e- 9
# 4 t4    2       score        0.611 8.31e-10
# 5 t1    4       score        0.969 2.54e- 1
# 6 t2    4       score        0.925 5.58e- 3
# 7 t3    4       score        0.935 1.28e- 2
# 8 t4    4       score        0.925 5.54e- 3
# 9 t1    6       score        0.974 3.96e- 1
# 10 t2    6       score        0.992 9.86e- 1
# 11 t3    6       score        0.974 3.75e- 1
# 12 t4    6       score        0.982 7.08e- 1
ggqqplot(capT3, "score", ggtheme = theme_bw()) +
  facet_grid(time ~ setsize, labeller = "label_both")
ggsave('During_ORT_qq',device = "png", path = prePostFolder) 

# capTTransformed <- capT3 %>% 
#   mutate(loged=log(score),
#          sqred=sqrt(score))
# capTTransformed %>% 
#   group_by(setsize,time) %>% 
#   shapiro_test(sqred)
#Transformed data is not meeting the assumption. 

#Friedman?
# capT3 %>% friedman_test(score~time|Row.names)
#Error in friedman.test.default(c(1.97800371115808, 1.94838376959915, 1.91969980458342,  : 
#                                  not an unreplicated complete block design


capT3 <- select(capT3,-type)
res.aov <- anova_test(data = capT3, dv = score, wid = Row.names, within = c(time,setsize))
get_anova_table(res.aov)

#ANOVA Table (type III tests)

# Effect  DFn    DFd      F        p p<.05      ges
# 1         time 3.00 135.00  0.282 8.38e-01       0.000469
# 2      setsize 1.17  52.78 54.019 1.96e-10     * 0.238000
# 3 time:setsize 2.58 115.97  1.709 1.76e-01       0.003000

# comparisons for setsize variable
capT3 %>% 
  pairwise_t_test(
    score ~ setsize, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )
# .y.   group1 group2    n1    n2 statistic    df        p    p.adj p.adj.signif
# * <chr> <chr>  <chr>  <int> <int>     <dbl> <dbl>    <dbl>    <dbl> <chr>       
#   1 score 2      4        184   184    -20.4    183 3.95e-49 1.18e-48 ****        
#   2 score 2      6        184   184    -13.1    183 5.60e-28 1.68e-27 ****        
#   3 score 4      6        184   184     -3.65   183 3.38e- 4 1.00e- 3 ** 

#precision
preT <- groupedFinal %>% 
  filter(group == 'experiment') %>% 
  select(Row.names, precision_ortT_t1_ss2,precision_ortT_t2_ss2,precision_ortT_t3_ss2,precision_ortT_t4_ss2,precision_ortT_t1_ss4,precision_ortT_t2_ss4,precision_ortT_t3_ss4,precision_ortT_t4_ss4,precision_ortT_t1_ss6,precision_ortT_t2_ss6,precision_ortT_t3_ss6,precision_ortT_t4_ss6)

preT2 <- preT %>% 
  gather(key = "type", value = "score",  precision_ortT_t1_ss2,precision_ortT_t2_ss2,precision_ortT_t3_ss2,precision_ortT_t4_ss2,precision_ortT_t1_ss4,precision_ortT_t2_ss4,precision_ortT_t3_ss4,precision_ortT_t4_ss4,precision_ortT_t1_ss6,precision_ortT_t2_ss6,precision_ortT_t3_ss6,precision_ortT_t4_ss6)

preT3 <- preT2 %>% 
  mutate(time = substr(type, 16, 17)) %>% 
  mutate(setsize = substr(type, 21, 21))

duringORTSummary <- rbind(duringORTSummary, preT3 %>% 
  group_by(setsize,time) %>% 
  get_summary_stats(score,type='mean_sd'))

bxp <- ggboxplot(
  preT3, x = 'time', y = "score", group = "setsize",
  color = "setsize", palette ="jco") +
  labs(title="precision changes during training", x= "Time", y = "precision")
ggsave('During ORT',device = "png", path = prePostFolder)        

#outliers
preT3 %>% 
  group_by(setsize,time) %>% 
  identify_outliers(score)
# time  setsize Row.names                type                   score is.outlier is.extreme
# <chr> <chr>   <I<chr>>                 <chr>                  <dbl> <lgl>      <lgl>     
#   1 t2    2       5eaeb8ccea726058c6eb9a16 precision_ortT_t2_ss2 0.122  TRUE       FALSE     
# 2 t2    2       5f0af097e7d15b3bf7734642 precision_ortT_t2_ss2 0.134  TRUE       FALSE     
# 3 t2    2       5fac47187dfda11d948deef7 precision_ortT_t2_ss2 0.123  TRUE       FALSE     
# 4 t2    2       5fb028d5e28a5c39c175eeda precision_ortT_t2_ss2 0.122  TRUE       FALSE     
# 5 t3    2       5e4ab1b4ca765d000edebd15 precision_ortT_t3_ss2 0.132  TRUE       FALSE     
# 6 t3    2       5f0af097e7d15b3bf7734642 precision_ortT_t3_ss2 0.147  TRUE       FALSE     
# 7 t1    4       5eea8ba19d0a1a155a3a67f7 precision_ortT_t1_ss4 0.0918 TRUE       FALSE     
# 8 t1    4       5f0af097e7d15b3bf7734642 precision_ortT_t1_ss4 0.122  TRUE       TRUE      
# 9 t1    4       5f63d49e68efb3138e72e6cc precision_ortT_t1_ss4 0.0999 TRUE       FALSE     
# 10 t1    4       5fb028d5e28a5c39c175eeda precision_ortT_t1_ss4 0.0897 TRUE       FALSE    


# normality
preT3 %>% 
  group_by(setsize,time) %>% 
  shapiro_test(score)
# # A tibble: 12 x 5
# time  setsize variable statistic        p
# <chr> <chr>   <chr>        <dbl>    <dbl>
#   1 t1    2       score        0.978 5.40e- 1
# 2 t2    2       score        0.929 7.54e- 3
# 3 t3    2       score        0.940 1.99e- 2
# 4 t4    2       score        0.960 1.16e- 1
# 5 t1    4       score        0.881 2.22e- 4
# 6 t2    4       score        0.860 5.78e- 5
# 7 t3    4       score        0.981 6.57e- 1
# 8 t4    4       score        0.961 1.26e- 1
# 9 t1    6       score        0.866 8.10e- 5
# 10 t2    6       score        0.232 3.91e-14
# 11 t3    6       score        0.903 1.01e- 3
#12 t4    6       score        0.949 4.14e- 2
ggqqplot(preT3, "score", ggtheme = theme_bw()) +
  facet_grid(time ~ setsize, labeller = "label_both")
ggsave('During_ORT_precision_qq',device = "png", path = prePostFolder) 

preT3 <- select(preT3,-type)
res.aov <- anova_test(data = preT3, dv = score, wid = Row.names, within = c(time,setsize))
get_anova_table(res.aov)
# ANOVA Table (type III tests)
# 
# Effect  DFn   DFd     F     p p<.05   ges
# 1         time 1.16 52.12 0.841 0.380       0.004
# 2      setsize 1.18 53.00 9.741 0.002     * 0.038
# 3 time:setsize 1.14 51.48 0.687 0.430       0.007

# comparisons for setsize variable
preT3 %>% 
  pairwise_t_test(
    score ~ setsize, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
# .y.   group1 group2    n1    n2 statistic    df        p    p.adj p.adj.signif
# * <chr> <chr>  <chr>  <int> <int>     <dbl> <dbl>    <dbl>    <dbl> <chr>       
#   1 score 2      4        184   184    15.8     183 6.34e-36 1.90e-35 ****        
#   2 score 2      6        184   184     3.08    183 2.00e- 3 7.00e- 3 **          
#   3 score 4      6        184   184    -0.616   183 5.39e- 1 1.00e+ 0 ns  



# During training in control group####
accT <- groupedFinal %>% 
  filter(group == 'control') %>% 
  select(Row.names, t1_ss8.vsAcc,t2_ss8.vsAcc,t3_ss8.vsAcc,t4_ss8.vsAcc,t1_ss16.vsAcc,t2_ss16.vsAcc,t3_ss16.vsAcc,t4_ss16.vsAcc,t1_ss24.vsAcc,t2_ss24.vsAcc,t3_ss24.vsAcc,t4_ss24.vsAcc)

accT2 <- accT %>% 
  gather(key = "type", value = "score", t1_ss8.vsAcc,t2_ss8.vsAcc,t3_ss8.vsAcc,t4_ss8.vsAcc,t1_ss16.vsAcc,t2_ss16.vsAcc,t3_ss16.vsAcc,t4_ss16.vsAcc,t1_ss24.vsAcc,t2_ss24.vsAcc,t3_ss24.vsAcc,t4_ss24.vsAcc)

accT3 <- accT2 %>% 
  mutate(time = substr(type, 1, 2)) %>% 
  mutate(setsize = substr(type, 6, 7))
setsize <- replace(accT3$setsize, accT3$setsize == '8.', '08')
accT3 <- select(accT3,-setsize)
accT3 <- cbind(accT3, setsize)

duringVSSummary <- accT3 %>% 
  group_by(setsize,time) %>% 
  get_summary_stats(score,type='mean_sd')

ggboxplot(
  accT3, x = 'time', y = "score", group = "setsize",
  color = "setsize", palette ="jco") +
  labs(title="Changes during training", x= "Time", y = "Accuracy")
ggsave('During VS acc',device = "png", path = prePostFolder)      


#outliers
accT3 %>% 
  group_by(setsize,time) %>% 
  identify_outliers(score)
# time  setsize Row.names                type         score is.outlier is.extreme
# <chr> <fct>   <I<chr>>                 <chr>        <dbl> <lgl>      <lgl>     
#   1 t1    08      5ee10d5b0017d10ec8702d10 t1_ss8.vsAcc 0.267 TRUE       TRUE      
# 2 t1    08      5f2c18b525352005ad5e5c28 t1_ss8.vsAcc 0.492 TRUE       TRUE      
# 3 t1    08      5f636c7645f61e036f74ccbf t1_ss8.vsAcc 0.525 TRUE       TRUE      
# 4 t1    08      5f67904169a51c3b06d32fa3 t1_ss8.vsAcc 0.625 TRUE       FALSE     
# 5 t2    08      5f636c7645f61e036f74ccbf t2_ss8.vsAcc 0.467 TRUE       TRUE      
# 6 t2    08      5f67904169a51c3b06d32fa3 t2_ss8.vsAcc 0.492 TRUE       TRUE      
# 7 t3    08      5f636c7645f61e036f74ccbf t3_ss8.vsAcc 0.525 TRUE       TRUE      
# 8 t4    08      5f0d98c1c8a2f100082295b8 t4_ss8.vsAcc 0.717 TRUE       FALSE     
# 9 t4    08      5f636c7645f61e036f74ccbf t4_ss8.vsAcc 0.408 TRUE       TRUE      
# 10 t4    08      5f67904169a51c3b06d32fa3 t4_ss8.vsAcc 0.783 TRUE       FALSE  


# normality
accT3 %>% 
  group_by(setsize,time) %>% 
  shapiro_test(score)
ggqqplot(accT3, "score", ggtheme = theme_bw()) +
  facet_grid(time ~ setsize, labeller = "label_both")
ggsave('During_ORT_precision_qq',device = "png", path = prePostFolder) 
# time  setsize variable statistic        p
# <chr> <fct>   <chr>        <dbl>    <dbl>
#   1 t1    08      score        0.635 6.67e-10
# 2 t2    08      score        0.686 4.72e- 9
# 3 t3    08      score        0.719 1.81e- 8
# 4 t4    08      score        0.632 6.06e-10
# 5 t1    16      score        0.779 2.97e- 7
# 6 t2    16      score        0.894 3.16e- 4
# 7 t3    16      score        0.904 6.35e- 4
# 8 t4    16      score        0.912 1.22e- 3
# 9 t1    24      score        0.895 3.22e- 4
# 10 t2    24      score        0.952 4.08e- 2
# 11 t3    24      score        0.950 3.56e- 2
# 12 t4    24      score        0.937 9.90e- 3

accT3 <- select(accT3,-type)
res.aov <- anova_test(data = accT3, dv = score, wid = Row.names, within = c(time,setsize))
get_anova_table(res.aov)

# ANOVA Table (type III tests)
# 
# Effect  DFn    DFd       F        p p<.05      ges
# 1         time 1.82  89.32   5.888 5.00e-03     * 0.022000
# 2      setsize 1.52  74.48 215.910 1.85e-28     * 0.300000
# 3 time:setsize 3.66 179.37   0.806 5.13e-01       0.000971

# comparisons for time variable
accT3 %>% 
  pairwise_t_test(
    score ~ time, paired = TRUE,
    p.adjust.method = "bonferroni")
# .y.   group1 group2    n1    n2 statistic    df         p      p.adj p.adj.signif
# * <chr> <chr>  <chr>  <int> <int>     <dbl> <dbl>     <dbl>      <dbl> <chr>       
#   1 score t1     t2       150   150    -2.21    149 0.029     0.172      ns          
# 2 score t1     t3       150   150    -4.17    149 0.0000506 0.000304   ***         
#   3 score t1     t4       150   150    -5.03    149 0.0000014 0.00000840 ****        
#   4 score t2     t3       150   150    -3.29    149 0.001     0.008      **          
#   5 score t2     t4       150   150    -3.58    149 0.000468  0.003      **          
#   6 score t3     t4       150   150    -0.644   149 0.521     1          ns  

# comparisons for setsize variable
accT3 %>% 
  pairwise_t_test(
    score ~ setsize, paired = TRUE,
    p.adjust.method = "bonferroni")
# .y.   group1 group2    n1    n2 statistic    df        p    p.adj p.adj.signif
# * <chr> <chr>  <chr>  <int> <int>     <dbl> <dbl>    <dbl>    <dbl> <chr>       
#   1 score 08     16       200   200      18.8   199 4.58e-46 1.37e-45 ****        
#   2 score 08     24       200   200      27.4   199 2.35e-69 7.05e-69 ****        
#   3 score 16     24       200   200      18.6   199 1.51e-45 4.53e-45 ****   


#mrt
mrtT <- groupedFinal %>% 
  filter(group == 'control') %>% 
  select(Row.names, t1_ss8.vsMrt,t2_ss8.vsMrt,t3_ss8.vsMrt,t4_ss8.vsMrt,t1_ss16.vsMrt,t2_ss16.vsMrt,t3_ss16.vsMrt,t4_ss16.vsMrt,t1_ss24.vsMrt,t2_ss24.vsMrt,t3_ss24.vsMrt,t4_ss24.vsMrt)

mrtT2 <- mrtT %>% 
  gather(key = "type", value = "score", t1_ss8.vsMrt,t2_ss8.vsMrt,t3_ss8.vsMrt,t4_ss8.vsMrt,t1_ss16.vsMrt,t2_ss16.vsMrt,t3_ss16.vsMrt,t4_ss16.vsMrt,t1_ss24.vsMrt,t2_ss24.vsMrt,t3_ss24.vsMrt,t4_ss24.vsMrt)

mrtT3 <- mrtT2 %>% 
  mutate(time = substr(type, 1, 2)) %>% 
  mutate(setsize = substr(type, 6, 7))
setsize <- replace(mrtT3$setsize, mrtT3$setsize == '8.', '08')
mrtT3 <- select(mrtT3,-setsize)
mrtT3 <- cbind(mrtT3, setsize)

duringVSSummary <- rbind(duringVSSummary,mrtT3 %>% 
  group_by(setsize,time) %>% 
  get_summary_stats(score,type='mean_sd'))

ggboxplot(
  mrtT3, x = 'time', y = "score", group = "setsize",
  color = "setsize", palette ="jco") +
  labs(title="Changes during training", x= "Time", y = "Reaction Time")
ggsave('During VS mrt',device = "png", path = prePostFolder) 

#outliers
mrtT3 %>% 
  group_by(setsize,time) %>% 
  identify_outliers(score)

# time  setsize Row.names                type          score is.outlier is.extreme
# <chr> <fct>   <I<chr>>                 <chr>         <dbl> <lgl>      <lgl>     
#   1 t1    08      5f31aac1304d250affe75b78 t1_ss8.vsMrt  2988. TRUE       FALSE     
# 2 t1    08      5f636c7645f61e036f74ccbf t1_ss8.vsMrt   722. TRUE       FALSE     
# 3 t2    08      5f636c7645f61e036f74ccbf t2_ss8.vsMrt   768. TRUE       FALSE     
# 4 t2    08      5f64c18a0d65c4021a1c3370 t2_ss8.vsMrt  1062. TRUE       FALSE     
# 5 t3    08      5f31aac1304d250affe75b78 t3_ss8.vsMrt  2729. TRUE       FALSE     
# 6 t3    08      5f3a76043bb11c6786e731bf t3_ss8.vsMrt  1150. TRUE       FALSE     
# 7 t3    08      5f636c7645f61e036f74ccbf t3_ss8.vsMrt   988. TRUE       FALSE     
# 8 t4    08      5f636c7645f61e036f74ccbf t4_ss8.vsMrt   367. TRUE       FALSE     
# 9 t1    16      5f2c18b525352005ad5e5c28 t1_ss16.vsMrt 1103. TRUE       FALSE     
# 10 t1    16      5f636c7645f61e036f74ccbf t1_ss16.vsMrt  641. TRUE       TRUE      
# # … with 12 more rows


# normality

mrtT3 %>%
  group_by(setsize,time) %>%
  shapiro_test(score)
ggqqplot(mrtT3, "score", ggtheme = theme_bw()) +
  facet_grid(time ~ setsize, labeller = "label_both")
ggsave('During_ORT_precision_qq',device = "png", path = prePostFolder)

# time  setsize variable statistic           p
# <chr> <fct>   <chr>        <dbl>       <dbl>
#   1 t1    08      score        0.956 0.0622     
# 2 t2    08      score        0.941 0.0151     
# 3 t3    08      score        0.971 0.246      
# 4 t4    08      score        0.909 0.000929   
# 5 t1    16      score        0.849 0.0000147  
# 6 t2    16      score        0.900 0.000494   
# 7 t3    16      score        0.963 0.124      
# 8 t4    16      score        0.893 0.000287   
# 9 t1    24      score        0.796 0.000000720
# 10 t2    24      score        0.865 0.0000414  
# 11 t3    24      score        0.953 0.0453     
# 12 t4    24      score        0.863 0.0000351  

mrtT3 <- select(mrtT3,-type)
res.aov <- anova_test(data = mrtT3, dv = score, wid = Row.names, within = c(time,setsize))
get_anova_table(res.aov)
# ANOVA Table (type III tests)
# 
# Effect  DFn    DFd       F        p p<.05      ges
# 1         time 2.01  98.37   4.494 1.30e-02     * 0.018000
# 2      setsize 1.17  57.36 345.189 1.84e-27     * 0.446000
# 3 time:setsize 3.51 171.98   0.533 6.89e-01       0.000414

# comparisons for time variable
mrtT3 %>% 
  pairwise_t_test(
    score ~ time, paired = TRUE,
    p.adjust.method = "bonferroni")
# # A tibble: 6 x 10
# .y.   group1 group2    n1    n2 statistic    df         p    p.adj p.adj.signif
# * <chr> <chr>  <chr>  <int> <int>     <dbl> <dbl>     <dbl>    <dbl> <chr>       
#   1 score t1     t2       150   150     1.79    149 0.075     0.449    ns          
# 2 score t1     t3       150   150     3.92    149 0.000133  0.000798 ***         
#   3 score t1     t4       150   150     4.06    149 0.0000798 0.000479 ***         
#   4 score t2     t3       150   150     3.27    149 0.001     0.008    **          
#   5 score t2     t4       150   150     3.97    149 0.000113  0.000678 ***         
#   6 score t3     t4       150   150     0.238   149 0.812     1        ns   

# comparisons for setsize variable
mrtT3 %>% 
  pairwise_t_test(
    score ~ setsize, paired = TRUE,
    p.adjust.method = "bonferroni")
# # A tibble: 3 x 10
# .y.   group1 group2    n1    n2 statistic    df        p    p.adj p.adj.signif
# * <chr> <chr>  <chr>  <int> <int>     <dbl> <dbl>    <dbl>    <dbl> <chr>       
#   1 score 08     16       200   200     -34.4   199 1.03e-85 3.09e-85 ****        
#   2 score 08     24       200   200     -34.1   199 5.27e-85 1.58e-84 ****        
#   3 score 16     24       200   200     -20.2   199 5.16e-50 1.55e-49 **** 







# during trainings with an extrem outlier excluded ####





#clear up----


p_unload(all)


