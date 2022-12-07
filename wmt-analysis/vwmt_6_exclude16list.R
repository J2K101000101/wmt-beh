# get ready----

# Re-run the analysis with all listed 16 participants excluded 

set.seed(1234321)
# install.packages('pacman')
library(pacman)
pacman::p_load(pacman,dplyr,tidyr,plyr,tidyverse,ggpubr,rstatix,ggplot2,broom,AICcmodavg,car,rcompanion,WRS2, apaTables,DescTools, ez, BayesFactor,psychReport,scales,cowplot,ggsci,ggpattern)
rm(list=ls()) 
trans_100 <- function(x) {   parse(text= x*100) }
workspace = getwd()
outputFolder = paste(workspace, "output", sep = "/")
preFolder = paste(outputFolder, "pre", sep = "/")
prePostFolder =paste(outputFolder, "prePost", sep = "/")
demographics <- read_csv('Participants demographics(all).csv')
groupInfo <- read_csv('groupInfo.csv') # need a groupInfo100!!!
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
# follow the conservative exclusion criteria (after a closer inspection; two was not engaged to the task '5f84302e2afc6203a3ac1180','5d3865fb0975a500013a1190'), we have 96 participants
# to check with the strict exclusion criteria, we need to remove the rest 14 on the list 
extraExclude <- c('5eaeb8ccea726058c6eb9a16','5ea93196653dfc000b23516b','5fad4db063bdee02d14d9511','5f0d98c1c8a2f100082295b8','5fa0100e226a1a000c4c19c3','5f566c4ac45b0308940a000b','5da9b30a3624c200157bd9f0','5f2c18b525352005ad5e5c28','5ee10d5b0017d10ec8702d10','5f636c7645f61e036f74ccbf','5cae09506d573900175d4727','5fa465fa1bae7e0d3724bfa2','5f64c18a0d65c4021a1c3370','5f9765e68112f6137e06db5d')
excludedAll <- includedTable[!(row.names(includedTable) %in% extraExclude),]
finalTable <- merge(included,excludedAll,by="row.names")


experimentalGroup <- finalTable %>% 
  filter(!is.na(capacity_ortT_t1_ss2) & !is.na(capacity_ortT_t2_ss2) & !is.na(capacity_ortT_t3_ss2) & !is.na(capacity_ortT_t4_ss2)& !is.na(capacity_ortT_t1_ss4) & !is.na(capacity_ortT_t2_ss4) & !is.na(capacity_ortT_t3_ss4) & !is.na(capacity_ortT_t4_ss4) & !is.na(capacity_ortT_t1_ss6) & !is.na(capacity_ortT_t2_ss6) & !is.na(capacity_ortT_t3_ss6) & !is.na(capacity_ortT_t4_ss6) & !is.na(precision_ortT_t1_ss2) & !is.na(precision_ortT_t2_ss2) & !is.na(precision_ortT_t3_ss2) & !is.na(precision_ortT_t4_ss2)& !is.na(precision_ortT_t1_ss4) & !is.na(precision_ortT_t2_ss4) & !is.na(precision_ortT_t3_ss4) & !is.na(precision_ortT_t4_ss4) & !is.na(precision_ortT_t1_ss6) & !is.na(precision_ortT_t2_ss6) & !is.na(precision_ortT_t3_ss6) & !is.na(precision_ortT_t4_ss6)) %>% 
  cbind(group = 'experiment')
controlGroup <- finalTable %>% 
  filter(!is.na(t1_ss8.vsAcc) & !is.na(t2_ss8.vsAcc) & !is.na(t3_ss8.vsAcc) & !is.na(t4_ss8.vsAcc) & !is.na(t1_ss16.vsAcc) & !is.na(t2_ss16.vsAcc) & !is.na(t3_ss16.vsAcc) & !is.na(t4_ss16.vsAcc)  & !is.na(t1_ss24.vsAcc) & !is.na(t2_ss24.vsAcc) & !is.na(t3_ss24.vsAcc) & !is.na(t4_ss24.vsAcc)  & !is.na(t1_ss8.vsMrt) & !is.na(t2_ss8.vsMrt) & !is.na(t3_ss8.vsMrt) & !is.na(t4_ss8.vsMrt) & !is.na(t1_ss16.vsMrt) & !is.na(t2_ss16.vsMrt) & !is.na(t3_ss16.vsMrt) & !is.na(t4_ss16.vsMrt)  & !is.na(t1_ss24.vsMrt) & !is.na(t2_ss24.vsMrt) & !is.na(t3_ss24.vsMrt) & !is.na(t4_ss24.vsMrt)) %>% 
  cbind(group = 'control')
groupedFinal <- rbind(experimentalGroup,controlGroup)
# experimental group: 38; control group 44

# demographics####
groupedFinal %>%
  dplyr::group_by(group,includedDemographics.Sex) %>%
  dplyr::summarise(nSex = dplyr::n()) %>% 
  ungroup()
# group      includedDemographics.Sex  nSex
# <chr>      <chr>                    <int>
#   1 control    Female                      20
# 2 control    Male                        24
# 3 experiment Female                      10
# 4 experiment Male                        28

demSummary <- groupedFinal %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(mAge = mean(includedDemographics.age),
                   sdAge = sd(includedDemographics.age),
  ) %>%  ungroup()
# group
# mAge
# sdAge
# 1
# control
# 21.97727
# 2.881284
# 2
# experiment
# 22.97368
# 4.200945

# chi-Square test for categorical variable gender
baseline <- merge(excludedAll,groupInfo,by.x = 0,by.y = 'Row.names')
bs.gender <- baseline %>% 
  select(gender = includedDemographics.Sex, group =  group)
bs.gender <- with(bs.gender, table(gender, group))
bs.gender.chis <- chisq.test(bs.gender)
bs.gender.bf <- contingencyTableBF(bs.gender, sampleType = 'indepMulti', fixedMargin = 'cols')
# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  bs.gender
# X-squared = 2.4472, df = 1, p-value = 0.1177
# Bayes factor analysis

#   [1] Non-indep. (a=1) : 1.262215 ±0%
# 
# Against denominator:
#   Null, independence, a = 1 
# ---
#   Bayes factor type: BFcontingencyTable, independent multinomial


# t test for age
bs.age <- baseline %>% 
  select(age= includedDemographics.age, group =  group)
bs.age %>% 
  group_by(group) %>% 
  identify_outliers(age)
# # A tibble: 2 × 4
# group     age is.outlier is.extreme
# <chr>   <dbl> <lgl>      <lgl>     
#   1 control    28 TRUE       FALSE     
# 2 control    32 TRUE       FALSE 
bs.age %>% 
  group_by(group) %>% 
  shapiro_test(age)
# # A tibble: 2 × 4
# group      variable statistic       p
# <chr>      <chr>        <dbl>   <dbl>
#   1 control    age          0.909 0.00204
# 2 experiment age          0.890 0.00132
bs.age %>% levene_test(age ~ group)
# # A tibble: 1 × 4
# df1   df2 statistic      p
# <int> <int>     <dbl>  <dbl>
#   1     1    80      3.94 0.0505
wilcox.test(bs.age$age ~ bs.age$group)
# Wilcoxon rank sum test with continuity correction
# 
# data:  bs.age$age by bs.age$group
# W = 774.5, p-value = 0.568
# alternative hypothesis: true location shift is not equal to 0
# 
# Warning message:
#   In wilcox.test.default(x = c(23, 28, 21, 24, 21, 22, 21, 19, 20,  :
#                                  cannot compute exact p-value with ties
yuen(age~group, data = bs.age)
akp.effect(age~group, data = bs.age)
# yuen(formula = age ~ group, data = bs.age)
# 
# Test statistic: 0.5902 (df = 33.02), p-value = 0.55908
# 
# Trimmed mean difference:  -0.51786 
# 95 percent confidence interval:
#   -2.303     1.2672 
# 
# Explanatory measure of effect size: 0.12 
# $AKPeffect
# [1] -0.1396019
# 
# $AKPci
# [1] -0.5534790  0.3402123
# 
# $alpha
# [1] 0.05


bf <- ttestBF(formula = age ~ group, data = bs.age)
bf
# Bayes factor analysis
#   [1] Alt., r=0.707 : 0.4621344 ±0.01%
# 
# Against denominator:
#   Null, mu1-mu2 = 0 
# ---
#   Bayes factor type: BFindepSample, JZS

1/bf[1]
# Bayes factor analysis
#   [1] Null, mu1-mu2=0 : 2.163873 ±0.01%
# 
# Against denominator:
#   Alternative, r = 0.707106781186548, mu =/= 0 
# ---
#   Bayes factor type: BFindepSample, JZS


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

pre_summary
# # A tibble: 2 × 5
# group      variable             n  mean    sd
# <chr>      <chr>            <dbl> <dbl> <dbl>
#   1 control    capacity_ort_pre    44  2.27 0.852
# 2 experiment capacity_ort_pre    38  2.63 0.724


ggplot(df_capacity_ort_pre,aes(x= capacity_ort_pre))+
  geom_histogram(aes(color =group, fill=group),position='identity', bins=30,alpha=0.4)+
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+ 
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))+
  geom_density(aes(linetype=group),color= 'red')
# ggsave('capacity_ort_pre_hist',device = "png", path = preFolder)

ggboxplot(df_capacity_ort_pre, x = "group", y = "capacity_ort_pre",
          color = "group", palette =c("#00AFBB", "#E7B800"),
          add = "jitter", shape = "group") +
  labs(title="Baseline capacity between groups in ORT",x="Groups", y = "Capacity")
# ggsave('capacity_ort_pre_box',device = "png", path = preFolder)

# Check outlier(s)
df_capacity_ort_pre %>% 
  group_by(group) %>% 
  identify_outliers(capacity_ort_pre)
# no outlier

# Check Normality
df_capacity_ort_pre %>% 
  group_by(group) %>% 
  shapiro_test(capacity_ort_pre)
# # A tibble: 2 × 4
# group      variable         statistic       p
# <chr>      <chr>                <dbl>   <dbl>
#   1 control    capacity_ort_pre     0.900 0.00112
# 2 experiment capacity_ort_pre     0.954 0.116 

ggqqplot(df_capacity_ort_pre,x = "capacity_ort_pre",facet.by = "group",color = "group", palette = c("#00AFBB", "#E7B800"), shape = "group")
# ggsave('capacity_ort_pre_qq',device = "png", path = preFolder)

# Check homogeneity 
df_capacity_ort_pre <- df_capacity_ort_pre %>% convert_as_factor(group)
df_capacity_ort_pre %>% levene_test(capacity_ort_pre ~ group)
# # A tibble: 1 × 4
# df1   df2 statistic     p
# <int> <int>     <dbl> <dbl>
#   1     1    80     0.343 0.560

# ## because the distribution is not normal, so use non-parametric: Mann-Whitney test
# pre_ttest <- wilcox.test(df_capacity_ort_pre$capacity_ort_pre ~ df_capacity_ort_pre$group)
# pre_se <- rcompanion::wilcoxonR(x= df_capacity_ort_pre$capacity_ort_pre, g=df_capacity_ort_pre$group)
# pre_ttest
# pre_se
# # Wilcoxon rank sum exact test
# # 
# # data:  df_capacity_ort_pre$capacity_ort_pre by df_capacity_ort_pre$group
# # W = 647, p-value = 0.07959
# # alternative hypothesis: true location shift is not equal to 0
# # r 
# # -0.194 

yuen(capacity_ort_pre~group, data = df_capacity_ort_pre)
akp.effect(capacity_ort_pre~group, data = df_capacity_ort_pre)

# Test statistic: 1.5717 (df = 49.82), p-value = 0.12235
# 
# Trimmed mean difference:  -0.30411 
# 95 percent confidence interval:
#   -0.6928     0.0846 
# 
# Explanatory measure of effect size: 0.26 
# $AKPeffect
# [1] -0.3522992
# 
# $AKPci
# [1] -0.81152979  0.04400996
# 
# $alpha
# [1] 0.05
# 
# $call
# akp.effect(formula = capacity_ort_pre ~ group, data = df_capacity_ort_pre)


ttestBF(formula = capacity_ort_pre ~ group, data = df_capacity_ort_pre)
# Bayes factor analysis

#   [1] Alt., r=0.707 : 1.326714 ±0.01%
# 
# Against denominator:
#   Null, mu1-mu2 = 0 
# ---
#   Bayes factor type: BFindepSample, JZS

# ort_precision
df_precision_ort_pre <- groupedFinal %>% 
  select(group,precision_ort_pre) %>% convert_as_factor(group)

pre_summary <- rbind(pre_summary, df_precision_ort_pre %>% 
                       group_by(group) %>% 
                       get_summary_stats(precision_ort_pre, type = "mean_sd"))
pre_summary
# # A tibble: 4 × 5
# group      variable              n  mean    sd
# <chr>      <chr>             <dbl> <dbl> <dbl>
#   1 control    capacity_ort_pre     44 2.27  0.852
# 2 experiment capacity_ort_pre     38 2.63  0.724
# 3 control    precision_ort_pre    44 0.059 0.019
# 4 experiment precision_ort_pre    38 0.056 0.014

ggplot(df_precision_ort_pre,aes(x= precision_ort_pre))+
  geom_histogram(aes(color =group, fill=group),position='identity', bins=30,alpha=0.4)+
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+ 
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))+
  geom_density(aes(linetype=group),color= 'red')
#ggsa vve('precision_ort_pre_hist',device = "png", path = preFolder)

ggboxplot(df_precision_ort_pre, x = "group", y = "precision_ort_pre",
          color = "group", palette =c("#00AFBB", "#E7B800"),
          add = "jitter", shape = "group") +
  labs(title="Baseline precision between groups in ORT",x="Groups", y = "Precision")
#ggsave('precision_ort_pre_box',device = "png", path = preFolder)


# Check outlier(s)
df_precision_ort_pre %>% 
  group_by(group) %>% 
  identify_outliers(precision_ort_pre)
# # A tibble: 4 × 4
# group      precision_ort_pre is.outlier is.extreme
# <chr>                  <dbl> <lgl>      <lgl>     
#   1 control               0.120  TRUE       TRUE      
# 2 control               0.122  TRUE       TRUE      
# 3 control               0.0897 TRUE       FALSE     
# 4 experiment            0.0843 TRUE       FALSE 

# Check Normality
df_precision_ort_pre %>% 
  group_by(group) %>% 
  shapiro_test(precision_ort_pre)
# # A tibble: 2 × 4
# group      variable          statistic        p
# <chr>      <chr>                 <dbl>    <dbl>
#   1 control    precision_ort_pre     0.864 0.000103
# 2 experiment precision_ort_pre     0.981 0.756 

ggqqplot(df_precision_ort_pre,x = "precision_ort_pre",facet.by = "group",color = "group", palette = c( "#E7B800","#00AFBB"), shape = "group")
#ggsave('precision_ort_pre_qq',device = "png", path = preFolder) 

#Check homogeneity
df_precision_ort_pre %>% levene_test(precision_ort_pre ~ group)
# # A tibble: 1 × 4
# df1   df2 statistic     p
# <int> <int>     <dbl> <dbl>
#   1     1    80     0.312 0.578

wilcox.test(df_precision_ort_pre$precision_ort_pre ~ df_precision_ort_pre$group)
rcompanion::wilcoxonR(x= df_precision_ort_pre$precision_ort_pre, g=df_precision_ort_pre$group)

# Wilcoxon rank sum exact test
# 
# data:  df_precision_ort_pre$precision_ort_pre by df_precision_ort_pre$group
# W = 869, p-value = 0.7637
# alternative hypothesis: true location shift is not equal to 0
# 
# > rcompanion::wilcoxonR(x= df_precision_ort_pre$precision_ort_pre, g=df_precision_ort_pre$group)
# r 
# 0.0339 

yuen(precision_ort_pre~group, data = df_precision_ort_pre)
akp.effect(precision_ort_pre~group, data = df_precision_ort_pre)
# Test statistic: 0.3463 (df = 49.52), p-value = 0.73056
# 
# Trimmed mean difference:  0.00105 
# 95 percent confidence interval:
#   -0.005     0.0071 
# 
# Explanatory measure of effect size: 0.06 
# $AKPeffect
# [1] 0.07786667
# 
# $AKPci
# [1] -0.3450697  0.5262435
# 
# $alpha
# [1] 0.05

bf <- ttestBF(formula = precision_ort_pre ~ group, data = df_precision_ort_pre)
bf
# Bayes factor analysis

#   [1] Alt., r=0.707 : 0.281314 ±0.02%
# 
# Against denominator:
#   Null, mu1-mu2 = 0 
# ---
#   Bayes factor type: BFindepSample, JZS
1/bf
# Bayes factor analysis

#   [1] Null, mu1-mu2=0 : 3.554746 ±0.02%
# 
# Against denominator:
#   Alternative, r = 0.707106781186548, mu =/= 0 
# ---
#   Bayes factor type: BFindepSample, JZS

# srt_capacity
df_capacity_srt_pre <- groupedFinal %>% 
  select(group,capacity_srt_pre) %>% convert_as_factor(group)

pre_summary <- rbind(pre_summary,df_capacity_srt_pre %>% 
                       group_by(group) %>% 
                       get_summary_stats(capacity_srt_pre, type = "mean_sd"))
pre_summary
# # A tibble: 6 × 5
# group      variable              n  mean    sd
# <chr>      <chr>             <dbl> <dbl> <dbl>
#   1 control    capacity_ort_pre     44 2.27  0.852
# 2 experiment capacity_ort_pre     38 2.63  0.724
# 3 control    precision_ort_pre    44 0.059 0.019
# 4 experiment precision_ort_pre    38 0.056 0.014
# 5 control    capacity_srt_pre     44 2.09  0.688
# 6 experiment capacity_srt_pre     38 2.28  0.745

ggboxplot(df_capacity_srt_pre, x = "group", y = "capacity_srt_pre",
          color = "group", palette =c("#00AFBB", "#E7B800"),
          add = "jitter", shape = "group") +
  labs(title="Baseline capacity between groups in srt",x="Groups", y = "Capacity")
#ggsave('capacity_srt_pre_box',device = "png", path = preFolder)

ggplot(df_capacity_srt_pre,aes(x= capacity_srt_pre))+
  geom_histogram(aes(color =group, fill=group),position='identity', bins=30,alpha=0.4)+
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+ 
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))+
  geom_density(aes(linetype=group),color= 'red')
#ggsave('capacity_srt_pre_hist',device = "png", path = preFolder)

# Check outlier(s)
df_capacity_srt_pre %>% 
  group_by(group) %>% 
  identify_outliers(capacity_srt_pre)
# # A tibble: 2 × 4
# group      capacity_srt_pre is.outlier is.extreme
# <fct>                 <dbl> <lgl>      <lgl>     
#   1 control               4.00  TRUE       FALSE     
# 2 experiment            0.294 TRUE       FALSE   

# Check Normality
df_capacity_srt_pre %>% 
  group_by(group) %>% 
  shapiro_test(capacity_srt_pre)
ggqqplot(df_capacity_srt_pre,x = "capacity_srt_pre",facet.by = "group",color = "group", palette = c("#00AFBB", "#E7B800"), shape = "group")
#ggsave('capacity_srt_pre_qq',device = "png", path = preFolder)
# group      variable         statistic     p
# <fct>      <chr>                <dbl> <dbl>
#   1 experiment capacity_srt_pre     0.982 0.688
# 2 control    capacity_srt_pre     0.991 0.967

# Check homogeneity 
df_capacity_srt_pre %>% levene_test(capacity_srt_pre ~ group)
# # A tibble: 1 × 4
# df1   df2 statistic     p
# <int> <int>     <dbl> <dbl>
#   1     1    80     0.358 0.551

# satisfied all the assumptions 
# Welch Two Sample t-test (different group size...)
df_capacity_srt_pre %>% t_test(capacity_srt_pre ~ group)
df_capacity_srt_pre %>% cohens_d(capacity_srt_pre ~ group)
# # A tibble: 1 × 8
# .y.              group1  group2        n1    n2 statistic    df     p
# * <chr>            <chr>   <chr>      <int> <int>     <dbl> <dbl> <dbl>
#   1 capacity_srt_pre control experiment    44    38     -1.18  76.1  0.24
# > df_capacity_srt_pre %>% cohens_d(capacity_srt_pre ~ group)
# # A tibble: 1 × 7
# .y.              group1  group2     effsize    n1    n2 magnitude
# * <chr>            <chr>   <chr>        <dbl> <int> <int> <ord>    
#   1 capacity_srt_pre control experiment  -0.263    44    38 small 

yuen(capacity_srt_pre~group, data = df_capacity_srt_pre)
akp.effect(capacity_srt_pre~group, data = df_capacity_srt_pre)
# Test statistic: 1.3786 (df = 45.2), p-value = 0.17481
# 
# Trimmed mean difference:  -0.24141 
# 95 percent confidence interval:
#   -0.5941     0.1112 
# 
# Explanatory measure of effect size: 0.23 
# AKPeffect
# [1] -0.3155973
# 
# $AKPci
# [1] -0.9026505  0.1637752
# 
# $alpha
# [1] 0.05

 bf <- ttestBF(formula = capacity_srt_pre ~ group, data = df_capacity_srt_pre)
# Bayes factor analysis

#   [1] Alt., r=0.707 : 0.4269159 ±0.02%
# 
# Against denominator:
#   Null, mu1-mu2 = 0 
# ---
#   Bayes factor type: BFindepSample, JZS
1/bf
# Bayes factor analysis

#   [1] Null, mu1-mu2=0 : 2.342382 ±0.02%
# 
# Against denominator:
#   Alternative, r = 0.707106781186548, mu =/= 0 
# ---
#   Bayes factor type: BFindepSample, JZS

# srt_precision
df_precision_srt_pre <- groupedFinal %>% 
  select(group,precision_srt_pre) 

pre_summary <- rbind(pre_summary, df_precision_srt_pre %>% 
                       group_by(group) %>% 
                       get_summary_stats(precision_srt_pre, type = "mean_sd"))
pre_summary
# # A tibble: 8 × 5
# group      variable              n  mean    sd
# <chr>      <chr>             <dbl> <dbl> <dbl>
#   1 control    capacity_ort_pre     44 2.27  0.852
# 2 experiment capacity_ort_pre     38 2.63  0.724
# 3 control    precision_ort_pre    44 0.059 0.019
# 4 experiment precision_ort_pre    38 0.056 0.014
# 5 control    capacity_srt_pre     44 2.09  0.688
# 6 experiment capacity_srt_pre     38 2.28  0.745
# 7 control    precision_srt_pre    44 0.05  0.024
# 8 experiment precision_srt_pre    38 0.05  0.018


ggboxplot(df_precision_srt_pre, x = "group", y = "precision_srt_pre",
          color = "group", palette =c("#00AFBB", "#E7B800"),
          add = "jitter", shape = "group") +
  labs(title="Baseline precision between groups in srt",x="Groups", y = "Precision")
#ggsave('precision_srt_pre_box',device = "png", path = preFolder)

ggplot(df_precision_srt_pre,aes(x= precision_srt_pre))+
  geom_histogram(aes(color =group, fill=group),position='identity', bins=30,alpha=0.4)+
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+ 
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))+
  geom_density(aes(linetype=group),color= 'red')
#ggsave('precision_srt_pre_hist',device = "png", path = preFolder)

# Check outlier(s)
df_precision_srt_pre %>% 
  group_by(group) %>% 
  identify_outliers(precision_srt_pre)
# # A tibble: 4 × 4
# group      precision_srt_pre is.outlier is.extreme
# <chr>                  <dbl> <lgl>      <lgl>     
#   1 control               0.127  TRUE       FALSE     
# 2 control               0.121  TRUE       FALSE     
# 3 experiment            0.0924 TRUE       FALSE     
# 4 experiment            0.0995 TRUE       FALSE    

# Check Normality
df_precision_srt_pre %>% 
  group_by(group) %>% 
  shapiro_test(precision_srt_pre)
ggqqplot(df_precision_srt_pre,x = "precision_srt_pre",facet.by = "group", palette = c("#00AFBB", "#E7B800"), shape = "group")
# ggsave('precision_srt_pre_qq',device = "png", path = preFolder) 
# # A tibble: 2 × 4
# group      variable          statistic        p
# <chr>      <chr>                 <dbl>    <dbl>
#   1 control    precision_srt_pre     0.896 0.000801
# 2 experiment precision_srt_pre     0.948 0.0783 

#Check homogeneity
df_precision_srt_pre %>% convert_as_factor(group)%>% levene_test(precision_srt_pre ~ group)
# # A tibble: 1 × 4
# df1   df2 statistic     p
# <int> <int>     <dbl> <dbl>
#   1     1    80     0.528 0.469

wilcox.test(df_precision_srt_pre$precision_srt_pre ~ df_precision_srt_pre$group)
rcompanion::wilcoxonR(x= df_precision_srt_pre$precision_srt_pre, g=df_precision_srt_pre$group)
# Wilcoxon rank sum exact test
# 
# data:  df_precision_srt_pre$precision_srt_pre by df_precision_srt_pre$group
# W = 796, p-value = 0.7148
# alternative hypothesis: true location shift is not equal to 0
# 
# > rcompanion::wilcoxonR(x= df_precision_srt_pre$precision_srt_pre, g=df_precision_srt_pre$group)
# r 
# -0.0411 

yuen(precision_srt_pre~group, data = df_precision_srt_pre)
akp.effect(precision_srt_pre~group, data = df_precision_srt_pre)

# Test statistic: 0.2361 (df = 48.87), p-value = 0.81435
# 
# Trimmed mean difference:  -0.00092 
# 95 percent confidence interval:
#   -0.0088     0.0069 
# 
# Explanatory measure of effect size: 0.03 
# 
# > akp.effect(precision_srt_pre~group, data = df_precision_srt_pre)
# $AKPeffect
# [1] -0.05207515
# 
# $AKPci
# [1] -0.5384687  0.3509642
# 
# $alpha
# [1] 0.05
# 
# $call
# akp.effect(formula = precision_srt_pre ~ group, data = df_precision_srt_pre)


bf <- ttestBF(formula = precision_srt_pre ~ group, data = df_precision_srt_pre)
# Bayes factor analysis

#   [1] Alt., r=0.707 : 0.2319246 ±0.02%
# 
# Against denominator:
#   Null, mu1-mu2 = 0 
# ---
#   Bayes factor type: BFindepSample, JZS
1/bf
# Bayes factor analysis

#   [1] Null, mu1-mu2=0 : 4.311746 ±0.02%
# 
# Against denominator:
#   Alternative, r = 0.707106781186548, mu =/= 0 
# ---
#   Bayes factor type: BFindepSample, JZS

# odt_capacity
df_kPre <- groupedFinal %>% 
  select(group,kPre) %>% convert_as_factor(group)

pre_summary <- rbind(pre_summary,df_kPre %>% 
                       group_by(group) %>% 
                       get_summary_stats(kPre, type = "mean_sd"))
# # A tibble: 10 × 5
# group      variable              n  mean    sd
# <chr>      <chr>             <dbl> <dbl> <dbl>
#   1 control    capacity_ort_pre     44 2.27  0.852
# 2 experiment capacity_ort_pre     38 2.63  0.724
# 3 control    precision_ort_pre    44 0.059 0.019
# 4 experiment precision_ort_pre    38 0.056 0.014
# 5 control    capacity_srt_pre     44 2.09  0.688
# 6 experiment capacity_srt_pre     38 2.28  0.745
# 7 control    precision_srt_pre    44 0.05  0.024
# 8 experiment precision_srt_pre    38 0.05  0.018
# 9 control    kPre                 44 0.521 0.192
# 10 experiment kPre                 38 0.554 0.262
ggboxplot(df_kPre, x = "group", y = "kPre",
          color = "group", palette =c("#00AFBB", "#E7B800"),
          add = "jitter", shape = "group") +
  labs(title="Baseline capacity between groups in odt",x="Groups", y = "Capacity (k)")
# ggsave('kPre_box',device = "png", path = preFolder)

ggplot(df_kPre,aes(x= kPre))+
  geom_histogram(aes(color =group, fill=group),position='identity', bins=30,alpha=0.4)+
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+ 
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))+
  geom_density(aes(linetype=group),color= 'red')
# ggsave('capacity_odt_pre_hist',device = "png", path = preFolder)

# Check outlier(s)
df_kPre %>% 
  group_by(group) %>% 
  identify_outliers(kPre)
# # A tibble: 2 × 4
# group        kPre is.outlier is.extreme
# <chr>       <dbl> <lgl>      <lgl>     
#   1 control     0     TRUE       FALSE     
# 2 experiment -0.632 TRUE       TRUE

# Check Normality
df_kPre %>% 
  group_by(group) %>% 
  shapiro_test(kPre)
ggqqplot(df_kPre,x = "kPre",facet.by = "group",color = "group", palette = c("#00AFBB", "#E7B800"), shape = "group")
# ggsave('kPre_qq',device = "png", path = preFolder)
# # A tibble: 2 × 4
# group      variable statistic          p
# <chr>      <chr>        <dbl>      <dbl>
#   1 control    kPre         0.955 0.0832    
# 2 experiment kPre         0.786 0.00000546

# Check homogeneity 
df_kPre %>% levene_test(kPre ~ group)
# # A tibble: 1 × 4
# df1   df2 statistic     p
# <int> <int>     <dbl> <dbl>
#   1     1    80     0.152 0.698

wilcox.test(df_kPre$kPre ~ df_kPre$group)
rcompanion::wilcoxonR(x= df_kPre$kPre, g=df_kPre$group)
# Wilcoxon rank sum test with continuity correction
# 
# data:  df_kPre$kPre by df_kPre$group
# W = 699, p-value = 0.2043
# alternative hypothesis: true location shift is not equal to 0
# 
# Warning message:
#   In wilcox.test.default(x = c(0.404255319148936, 0.5, 0.66, 0.510204081632653,  :
#                                  cannot compute exact p-value with ties
#                                > rcompanion::wilcoxonR(x= df_kPre$kPre, g=df_kPre$group)
#                                r
#                                -0.14
yuen(kPre~group, data = df_kPre)
akp.effect(kPre~group, data = df_kPre)
# Test statistic: 1.3328 (df = 49.53), p-value = 0.1887
# 
# Trimmed mean difference:  -0.21185 
# 95 percent confidence interval:
#   -0.5312     0.1075 
# 
# Explanatory measure of effect size: 0.21 
# 
# > akp.effect(kPre~group, data = df_kPre)
# $AKPeffect
# [1] -0.2996401
# 
# $AKPci
# [1] -0.7566865  0.1435686
# 
# $alpha
# [1] 0.05
# 
# $call
# akp.effect(formula = kPre ~ group, data = df_kPre)
# 
# attr(,"class")
# [1] "AKP"

bf <- ttestBF(formula = kPre ~ group, data = df_kPre)
# Bayes factor analysis

#   [1] Alt., r=0.707 : 0.2771426 ±0.02%
# 
# Against denominator:
#   Null, mu1-mu2 = 0 
# ---
#   Bayes factor type: BFindepSample, JZS
1/bf
# Bayes factor analysis

#   [1] Null, mu1-mu2=0 : 3.60825 ±0.02%
# 
# Against denominator:
#   Alternative, r = 0.707106781186548, mu =/= 0 
# ---
#   Bayes factor type: BFindepSample, JZS

# vs_acc
df_accPre <- groupedFinal %>% 
  select(group,accPre) %>% convert_as_factor(group)

pre_summary <- rbind(pre_summary,df_accPre %>% 
                       group_by(group) %>% 
                       get_summary_stats(accPre, type = "mean_sd"))
pre_summary
# 11 control    accPre               44 0.779 0.089
# 12 experiment accPre               38 0.768 0.135

ggboxplot(df_accPre, x = "group", y = "accPre",
          color = "group", palette =c("#00AFBB", "#E7B800"),
          add = "jitter", shape = "group") +
  labs(title="Baseline accuracy between groups in VS",x="Groups", y = "Accuracy")
#ggsave('accPre_box',device = "png", path = preFolder)

ggplot(df_accPre,aes(x= accPre))+
  geom_histogram(aes(color =group, fill=group),position='identity', bins=30,alpha=0.4)+
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+ 
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))+
  geom_density(aes(linetype=group),color= 'red')
#ggsave('acc_vs_pre_hist',device = "png", path = preFolder)

# Check outlier(s)
df_accPre %>% 
  group_by(group) %>% 
  identify_outliers(accPre)
# # A tibble: 4 × 4
# group      accPre is.outlier is.extreme
# <chr>       <dbl> <lgl>      <lgl>     
#   1 control     0.525 TRUE       FALSE     
# 2 experiment  0.442 TRUE       FALSE     
# 3 experiment  0.45  TRUE       FALSE     
# 4 experiment  0.425 TRUE       FALSE

# Check Normality
df_accPre %>% 
  group_by(group) %>% 
  shapiro_test(accPre)
ggqqplot(df_accPre,x = "accPre",facet.by = "group",color = "group", palette = c("#00AFBB", "#E7B800"), shape = "group")
# ggsave('accPre_qq',device = "png", path = preFolder)
# # A tibble: 2 × 4
# group      variable statistic       p
# <chr>      <chr>        <dbl>   <dbl>
#   1 control    accPre       0.957 0.0965 
# 2 experiment accPre       0.886 0.00104

# Check homogeneity 
df_accPre %>% levene_test(accPre ~ group)
# # A tibble: 1 × 4
# df1   df2 statistic     p
# <int> <int>     <dbl> <dbl>
#   1     1    80      2.24 0.138

wilcox.test(df_accPre$accPre ~ df_accPre$group)
rcompanion::wilcoxonR(x= df_accPre$accPre, g=df_accPre$group)
# Wilcoxon rank sum test with continuity correction
# 
# data:  df_accPre$accPre by df_accPre$group
# W = 799.5, p-value = 0.7374
# alternative hypothesis: true location shift is not equal to 0
# 
# Warning message:
#   In wilcox.test.default(x = c(0.758333333333333, 0.616666666666667,  :
#                                  cannot compute exact p-value with ties
#                                > rcompanion::wilcoxonR(x= df_accPre$accPre, g=df_accPre$group)
#                                r 
#                                -0.0375

yuen(accPre~group, data = df_accPre)
akp.effect(accPre~group, data = df_accPre)
# est statistic: 0.2471 (df = 43.54), p-value = 0.80597
# 
# Trimmed mean difference:  -0.0057 
# 95 percent confidence interval:
#   -0.0522     0.0408 
# 
# Explanatory measure of effect size: 0.05 
# $AKPeffect
# [1] -0.05684886
# 
# $AKPci
# [1] -0.5252507  0.4208057
# 
# $alpha
# [1] 0.05

bf <- ttestBF(formula = accPre ~ group, data = df_accPre)
# Bayes factor analysis

#   [1] Alt., r=0.707 : 0.2521261 ±0.02%
# 
# Against denominator:
#   Null, mu1-mu2 = 0 
# ---
#   Bayes factor type: BFindepSample, JZS
1/bf
# Bayes factor analysis

#   [1] Null, mu1-mu2=0 : 3.966269 ±0.02%
# 
# Against denominator:
#   Alternative, r = 0.707106781186548, mu =/= 0 
# ---
#   Bayes factor type: BFindepSample, JZS

# vs_mrt
df_mrtPre <- groupedFinal %>% 
  select(group,mrtPre) %>% convert_as_factor(group)

pre_summary <- rbind(pre_summary,df_mrtPre %>% 
                       group_by(group) %>% 
                       get_summary_stats(mrtPre, type = "mean_sd"))
# 13 control    mrtPre               44 3159.    471.   
# 14 experiment mrtPre               38 3066.    781. 

ggboxplot(df_mrtPre, x = "group", y = "mrtPre",
          color = "group", palette =c("#00AFBB", "#E7B800"),
          add = "jitter", shape = "group") +
  labs(title="Baseline reaction time between groups in VS",x="Groups", y = "Reaction Time (ms)")
#ggsave('mrtPre_box',device = "png", path = preFolder)

ggplot(df_mrtPre,aes(x= mrtPre))+
  geom_histogram(aes(color =group, fill=group),position='identity', bins=30,alpha=0.4)+
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+ 
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))+
  geom_density(aes(linetype=group),color= 'red')
# ggsave('mrt_vs_pre_hist',device = "png", path = preFolder)


# Check outlier(s)
df_mrtPre %>% 
  group_by(group) %>% 
  identify_outliers(mrtPre)
# # A tibble: 5 × 4
# group      mrtPre is.outlier is.extreme
# <chr>       <dbl> <lgl>      <lgl>     
#   1 control     1671. TRUE       FALSE     
# 2 control     1925. TRUE       FALSE     
# 3 experiment   889. TRUE       FALSE     
# 4 experiment   627. TRUE       TRUE      
# 5 experiment   936. TRUE       FALSE      

# Check Normality
df_mrtPre %>% 
  group_by(group) %>% 
  shapiro_test(mrtPre)
ggqqplot(df_mrtPre,x = "mrtPre",facet.by = "group",color = "group", palette = c("#00AFBB", "#E7B800"), shape = "group")
#ggsave('mrtPre_qq',device = "png", path = preFolder)
# # A tibble: 2 × 4
# group      variable statistic          p
# <chr>      <chr>        <dbl>      <dbl>
#   1 control    mrtPre       0.900 0.00105   
# 2 experiment mrtPre       0.752 0.00000127

# Check homogeneity 
df_mrtPre %>% levene_test(mrtPre ~ group)
# # A tibble: 1 × 4
# df1   df2 statistic     p
# <int> <int>     <dbl> <dbl>
#   1     1    80      1.75 0.189

wilcox.test(df_mrtPre$mrtPre ~ df_mrtPre$group)
rcompanion::wilcoxonR(x= df_mrtPre$mrtPre, g=df_mrtPre$group)
# Wilcoxon rank sum exact test
# 
# data:  df_mrtPre$mrtPre by df_mrtPre$group
# W = 785, p-value = 0.6404
# alternative hypothesis: true location shift is not equal to 0
# 
# > rcompanion::wilcoxonR(x= df_mrtPre$mrtPre, g=df_mrtPre$group)
# r 
# -0.0523 
yuen(mrtPre~group, data = df_mrtPre)
akp.effect(mrtPre~group, data = df_mrtPre)
# Test statistic: 0.2573 (df = 38.74), p-value = 0.79828
# 
# Trimmed mean difference:  -28.89449 
# 95 percent confidence interval:
#   -256.0527     198.2638 
# 
# Explanatory measure of effect size: 0.04 
# $AKPeffect
# [1] -0.05995553
# 
# $AKPci
# [1] -0.5413607  0.3595963
# 
# $alpha
# [1] 0.05


bf <- ttestBF(formula = mrtPre ~ group, data = df_mrtPre)
# Bayes factor analysis

#   [1] Alt., r=0.707 : 0.2791756 ±0.02%
# 
# Against denominator:
#   Null, mu1-mu2 = 0 
# ---
#   Bayes factor type: BFindepSample, JZS
1/bf
# Bayes factor analysis
# 
#   [1] Null, mu1-mu2=0 : 3.581975 ±0.02%
# 
# Against denominator:
#   Alternative, r = 0.707106781186548, mu =/= 0 
# 
#   Bayes factor type: BFindepSample, JZS

# two-way mixed anova in prepost####

prePost <- groupedFinal %>% 
  select(group,accPre,mrtPre,kPre,accPost,mrtPost,kPost,capacity_ort_pre,capacity_srt_pre,precision_ort_pre,precision_srt_pre,capacity_ort_post,capacity_srt_post,precision_ort_post,precision_srt_post,Row.names) %>% 
  group_by(group)

# capacity in ort ####
capOrt <- prePost %>% select(capacity_ort_pre,capacity_ort_post,group,Row.names) %>% 
  gather(key = "session", value = "score",capacity_ort_post, capacity_ort_pre) %>% 
  convert_as_factor(session) %>%  
  mutate(time=fct_relevel(session, 'capacity_ort_pre','capacity_ort_post')) 


capOrt %>%
  mutate(group = factor(group, levels=c("experiment", "control"))) %>%
  ggboxplot(
  x = 'time', y = "score", group = "session",
  #color = "group", palette =c("#00AFBB", "#E7B800"),
  color = "group", palette = c("#E69F00","#999999"),
  add = "jitter", shape = "group",size = 1) +
  labs( x= "Time", y = expression(~bold("Capacity"~bolditalic(K))), col = 'Group',shape = 'Group',size =1)+
  scale_x_discrete(labels=c("capacity_ort_pre"= "Pre-Test",'capacity_ort_post'='Post-Test'))+
  scale_y_continuous(labels = function(x) format(x, nsmall = 2)) +
  theme(legend.position = "none",legend.title=element_blank(),  element_line(colour = 'black',size = 1),panel.background=element_blank(),axis.line.y.left = element_line(size = 1),axis.line.x.bottom  = element_line(size = 1),axis.text = element_text(face="bold"),axis.title = element_text(face="bold"))
ggsave("capacity_pre_post.png",width = 4,height = 4)

# ggsave('ort_capacity_prepost',device = "png", path = prePostFolder)

preOrt %>%
  mutate(group = factor(group, levels=c("experiment", "control"))) %>%
  ggboxplot(
     x = 'time', y = "score", group = "session",
    #color = "group", palette =c("#00AFBB", "#E7B800"),
    color = "group", palette =c("#E69F00","#999999"),
    add = "jitter", shape = "group",size =1) +
    labs(x= "Time", y = expression(~bold('Precision'~bolditalic('SD'^-1))), col= 'Group',shape = 'Group',size =1)+
    scale_x_discrete(labels=c("precision_ort_pre"= "Pre-Test",'precision_ort_post'='Post-Test'))+
    scale_y_continuous(labels = function(x) x * 100) +
    theme(legend.position = "none",legend.title=element_blank(),  element_line(colour = 'black',size = 1),panel.background=element_blank(),axis.line.y.left = element_line(size = 1),axis.line.x.bottom  = element_line(size = 1),axis.text = element_text(face="bold"),axis.text.y.left = element_text(face="bold"),axis.title = element_text(face="bold"))
ggsave("precision_pre_post.png",width = 4,height = 4)

  
  
summary <- capOrt %>% get_summary_stats(score, type = "common")
summaryO <- capOrt %>% 
  group_by(group,session) %>% 
  get_summary_stats(score, type = "mean_sd")
# # A tibble: 4 × 6
# group      session           variable     n  mean    sd
# <chr>      <fct>             <chr>    <dbl> <dbl> <dbl>
#   1 control    capacity_ort_post score       44  2.58 0.672
# 2 control    capacity_ort_pre  score       44  2.27 0.852
# 3 experiment capacity_ort_post score       38  2.89 0.741
# 4 experiment capacity_ort_pre  score       38  2.63 0.724
names(summaryO)[names(summaryO) == "session"] <- "time"
setsize <- c(4,4,4,4)
summaryORT <- cbind(summaryO, setsize) 

# outfilters
capOrt %>%
  group_by(time, group) %>%
  identify_outliers(score)
# # A tibble: 1 × 7
# group   time              Row.names                session           score is.outlier is.extreme
# <chr>   <fct>             <I<chr>>                 <fct>             <dbl> <lgl>      <lgl>     
#   1 control capacity_ort_post 5f67904169a51c3b06d32fa3 capacity_ort_post 0.660 TRUE       FALSE 

#Normality:If the data is normally distributed, the p-value should be greater than 0.05.Note that, if your sample size is greater than 50, the normal QQ plot is preferred because at larger sample sizes the Shapiro-Wilk test becomes very sensitive even to a minor deviation from normality.
capOrt%>%
  group_by(time, group) %>%
  shapiro_test(score)
ggqqplot(capOrt, "score", ggtheme = theme_bw()) +
  facet_grid(time ~ group)
#ggsave('ort_capacity_prepost_qq',device = "png", path = prePostFolder)
# # A tibble: 4 × 5
# group      time              variable statistic       p
# <chr>      <fct>             <chr>        <dbl>   <dbl>
#   1 control    capacity_ort_pre  score        0.900 0.00112
# 2 experiment capacity_ort_pre  score        0.954 0.116  
# 3 control    capacity_ort_post score        0.941 0.0258 
# 4 experiment capacity_ort_post score        0.935 0.0280 

#Homogneity of variance assumption. There was homogeneity of variances, as assessed by Levene’s test (p > 0.05).
capOrt %>%
  group_by(time) %>%
  levene_test(score ~ group)
# # A tibble: 2 × 5
# time                df1   df2 statistic     p
# <fct>             <int> <int>     <dbl> <dbl>
#   1 capacity_ort_pre      1    80     0.343 0.560
# 2 capacity_ort_post     1    80     0.416 0.521


# Two-way mixed ANOVA test
capOrt$Row.names <- factor(capOrt$Row.names)
str(capOrt)
capOrt$group <- factor(capOrt$group)
# summary(capOrt)
#options(contrasts=c("contr.sum","contr.poly"))
anov <- ezANOVA(data = capOrt, dv=.(score), wid = .(Row.names), between = .(group), within = .(time),type = 3, detailed = TRUE)
anov
aT <- aovEffectSize(anov)
aovDispTable(aT)
# ═══════════════════════════════════════ ANOVA:aT ══════════════════════════════════════
# Effect DFn DFd          SSn      SSd            F            p p<.05         pes
# (Intercept)   1  80 1098.1251056 75.93997 1156.8349325 2.479262e-49     * 0.935318774
# group   1  80    4.4591560 75.93997    4.6975590 3.317923e-02     * 0.055462743
# time   1  80    3.3702204 14.42920   18.6855504 4.400865e-05     * 0.189344340
# group:time   1  80    0.0248147 14.42920    0.1375804 7.116788e-01       0.001716803
# ───────────────────────────────────────────────────────────────────────────────────────
aovEffectSize(aT,effectSize = "ges")
# Effect DFn DFd          SSn      SSd            F            p p<.05          ges
# 1 (Intercept)   1  80 1098.1251056 75.93997 1156.8349325 2.479262e-49     * 0.9239633082
# 2       group   1  80    4.4591560 75.93997    4.6975590 3.317923e-02     * 0.0470234587
# 3        time   1  80    3.3702204 14.42920   18.6855504 4.400865e-05     * 0.0359530849
# 4  group:time   1  80    0.0248147 14.42920    0.1375804 7.116788e-01       0.0002745172
# BF
str(capOrt)
bf <- anovaBF(score~group * time + Row.names,
              data = capOrt, whichRandom = 'Row.names')
bf
plot(bf)
bfInteraction = bf[4]/bf[3]
bfInteraction
1/bfInteraction
# [1] group + Row.names                     : 1.952022 ±0.96%
# [2] time + Row.names                      : 504.5483 ±0.96%
# [3] group + time + Row.names              : 1040.075 ±2.37%
# [4] group + time + group:time + Row.names : 260.0305 ±4.7%
# 
# Against denominator:
#   score ~ Row.names 
# ---
#   Bayes factor type: BFlinearModel, JZS
# 
# > plot(bf)
# > bfInteraction = bf[4]/bf[3]
# > bfInteraction
# Bayes factor analysis

#   [1] time + setsize + time:setsize + Row.names : 0.04235063 ±36.9%
# 
# Against denominator:
#   score ~ time + setsize + Row.names 
# ---
#   Bayes factor type: BFlinearModel, JZS

#precision in ort####
preOrt <- prePost %>% select(precision_ort_pre,precision_ort_post,group,Row.names) %>% 
  gather(key = "session", value = "score",precision_ort_post, precision_ort_pre) %>% 
  convert_as_factor(session) %>%  
  mutate(time=fct_relevel(session, 'precision_ort_pre','precision_ort_post')) %>% 
  group_by(time,group)

trans_100 <- function(x) {   
  
  parse(text= x*100) }

ort_pre <- ggboxplot(
  preOrt, x = 'time', y = "score", group = "session",
  #color = "group", palette =c("#00AFBB", "#E7B800"),
  color = "group", palette =c("#1B9E77","#D95F02"),
  add = "jitter", shape = "group") +
  labs(x= "Time", y = expression('Precision'~italic('SD')^-1), col= 'Group',shape = 'Group')+
  scale_x_discrete(labels=c("precision_ort_pre"= "pre test",'precision_ort_post'='post test'))+
  scale_y_continuous(labels = trans_100) +
  theme(legend.position = c(0.8,0.9))
#annotate("text", x=0, y=0.127, label= expression('          x'~'10'^'-2'))
ort_cap
ort_pre
#ggsave('ort_precision_prepost',device = "png", path = prePostFolder)


plot_grid(ort_cap,ort_pre,labels = 'AUTO')

summary <- rbind(summary, preOrt %>% get_summary_stats(score, type = "mean_sd"))
summaryORT2 <- preOrt %>% get_summary_stats(score, type = "mean_sd")
# A tibble: 4 × 6
# group      time               variable     n  mean    sd
# <chr>      <fct>              <chr>    <dbl> <dbl> <dbl>
#   1 control    precision_ort_pre  score       44 0.059 0.019
# 2 experiment precision_ort_pre  score       38 0.056 0.014
# 3 control    precision_ort_post score       44 0.054 0.013
# 4 experiment precision_ort_post score       38 0.069 0.016
setsize <- c(4,4,4,4)
summaryORT2 <- cbind(summaryORT2, setsize)

# outfilters
preOrt %>%
  group_by(time, group) %>%
  identify_outliers(score)

#Normality:If the data is normally distributed, the p-value should be greater than 0.05.Note that, if your sample size is greater than 50, the normal QQ plot is preferred because at larger sample sizes the Shapiro-Wilk test becomes very sensitive even to a minor deviation from normality.
preOrt%>%
  group_by(time, group) %>%
  shapiro_test(score)
ggqqplot(preOrt, "score", ggtheme = theme_bw()) +
  facet_grid(time ~ group)
#ggsave('ort_precision_prepost_qq',device = "png", path = prePostFolder)
 

#Homogneity of variance assumption. There was homogeneity of variances, as assessed by Levene’s test (p > 0.05).
preOrt %>%
  group_by(time) %>%
  levene_test(score ~ group)


# Two-way mixed ANOVA test
preOrt$Row.names <- factor(preOrt$Row.names) 
str(preOrt)
preOrt$group <- factor(preOrt$group)
anov <- ezANOVA(data = preOrt, dv=.(score), wid = .(Row.names), between = .(group), within = .(time),type = 3, detailed = TRUE)
aovEffectSize(anov)
# $ANOVA
# Effect DFn DFd          SSn         SSd           F            p p<.05        pes
# 1 (Intercept)   1  80 0.5796200786 0.029168026 1589.740993 1.503378e-54     * 0.95208838
# 2       group   1  80 0.0017467984 0.029168026    4.790995 3.152454e-02     * 0.05650358
# 3        time   1  80 0.0006048181 0.008902941    5.434772 2.225749e-02     * 0.06361311
# 4  group:time   1  80 0.0033089558 0.008902941   29.733599 5.362161e-07     * 0.27096166
aovEffectSize(anov,effectSize = "ges")
# $ANOVA
# Effect DFn DFd          SSn         SSd           F            p p<.05        ges
# 1 (Intercept)   1  80 0.5796200786 0.029168026 1589.740993 1.503378e-54     * 0.93836568
# 2       group   1  80 0.0017467984 0.029168026    4.790995 3.152454e-02     * 0.04386983
# 3        time   1  80 0.0006048181 0.008902941    5.434772 2.225749e-02     * 0.01563816
# 4  group:time   1  80 0.0033089558 0.008902941   29.733599 5.362161e-07     * 0.07996525
# BF
bf <- anovaBF(score~group * time + Row.names,
              data = preOrt, whichRandom = 'Row.names')
bf
plot(bf)
bfInteraction = bf[4]/bf[3]
bfInteraction

# Bayes factor analysis

#   [1] group + Row.names                     : 1.94138   ±4.28%
# [2] time + Row.names                      : 0.5965492 ±1.55%
# [3] group + time + Row.names              : 1.120337  ±3.99%
# [4] group + time + group:time + Row.names : 26997.49  ±4.65%
# Against denominator:
#   score ~ Row.names 
# ---
#   Bayes factor type: BFlinearModel, JZS
# 
# > plot(bf)
# > bfInteraction = bf[4]/bf[3]
# > bfInteraction
# Bayes factor analysis

# [1] group + time + group:time + Row.names : 24097.64 ±6.12%

# Against denominator:
#   score ~ group + time + Row.names 
# ---
#   Bayes factor type: BFlinearModel, JZS

# Effect of group at each time point
one.way <- capOrt %>% 
  group_by(time) %>% 
  anova_test(dv = score, wid = Row.names, between = group,effect.size = "ges") %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way


one.way2 <- preOrt %>% 
  group_by(time) %>% 
  anova_test(dv = score, wid = Row.names, between = group, effect.size = "pes") %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way2
# # A tibble: 2 × 9
# time               Effect   DFn   DFd      F          p `p<.05`   pes     p.adj
# <fct>              <chr>  <dbl> <dbl>  <dbl>      <dbl> <chr>   <dbl>     <dbl>
#   1 precision_ort_pre  group      1    80  0.458 0.5        ""      0.006 1        
# 2 precision_ort_post group      1    80 24.0   0.00000502 "*"     0.23  0.0000100


ext_pre <- preOrt %>% subset(group == "experiment" & time == "precision_ort_pre")
ext_post <- preOrt %>% subset(group == "experiment" & time == "precision_ort_post")
ctl_pre <- preOrt %>% subset(group == "control" & time == "precision_ort_pre")
ctl_post <- preOrt %>% subset(group == "control" & time == "precision_ort_post")
yuend(ext_pre$score,ext_post$score)
dep.effect(ext_pre$score,ext_post$score)
ttestBF(x = ext_pre$score,y=ext_post$score, paired=TRUE)

# Test statistic: -5.5766 (df = 23), p-value = 1e-05
# 
# Trimmed mean difference:  -0.01329 
# 95 percent confidence interval:
#   -0.0182     -0.0084 
# 
# Explanatory measure of effect size: 0.53 
# NULL        Est    S    M    L      ci.low      ci.up
# AKP          0.0 -1.0027428 0.10 0.30 0.50 -1.72831470 -0.7197986
# QS (median)  0.5  0.1315789 0.46 0.38 0.31  0.02631579  0.3421053
# QStr         0.5  0.1315789 0.46 0.38 0.31  0.02631579  0.2631579
# SIGN         0.5  0.8947368 0.54 0.62 0.69  0.75300000  0.9630000



yuend(ctl_pre$score,ctl_post$score)
dep.effect(ctl_pre$score,ctl_post$score)
ttestBF(x = ctl_pre$score,y=ctl_post$score, paired=TRUE)

# Test statistic: 2.289 (df = 27), p-value = 0.03013
# 
# Trimmed mean difference:  0.00461 
# 95 percent confidence interval:
#   5e-04     0.0088 
# 
# Explanatory measure of effect size: 0.25 
# NULL       Est    S    M    L      ci.low     ci.up
# AKP          0.0 0.2938150 0.10 0.30 0.50 -0.00759769 0.6304921
# QS (median)  0.5 0.7272727 0.54 0.62 0.69  0.50000000 0.8636364
# QStr         0.5 0.5454545 0.54 0.62 0.69  0.47727273 0.7727273
# SIGN         0.5 0.3636364 0.46 0.38 0.31  0.23400000 0.5150000
# attr(,"class")
# Bayes factor analysis
# --------------
#   [1] Alt., r=0.707 : 1.100276 ±0.02%
# 
# Against denominator:
#   Null, mu = 0 
# ---
#   Bayes factor type: BFoneSample, JZS


yuen(score~group, data = preOrt %>% subset(time == "precision_ort_post"))
# Test statistic: 5.1171 (df = 39.05), p-value = 1e-05
# 
# Trimmed mean difference:  -0.01685 
# 95 percent confidence interval:
#   -0.0235     -0.0102 
# 
# Explanatory measure of effect size: 0.66 
akp.effect(score~group, data = preOrt %>% subset(time == "precision_ort_post"))
# $AKPeffect
# [1] -1.191239
# 
# $AKPci
# [1] -1.6772425 -0.6075874
# 
# $alpha
# [1] 0.05
ttestBF(formula = score~group, data = preOrt %>% subset(time == "precision_ort_post"))
# Bayes factor analysis
# 
#   [1] Alt., r=0.707 : 3192.428 ±0%
# 
# Against denominator:
#   Null, mu1-mu2 = 0 


preOrt %>% 
  group_by(time) %>% 
  anova_test(dv = score, wid = Row.names, between = group, effect.size = "ges") %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
# # A tibble: 2 × 9
# time               Effect   DFn   DFd      F          p `p<.05`   ges     p.adj
# <fct>              <chr>  <dbl> <dbl>  <dbl>      <dbl> <chr>   <dbl>     <dbl>
#   1 precision_ort_pre  group      1    80  0.458 0.5        ""      0.006 1        
# 2 precision_ort_post group      1    80 24.0   0.00000502 "*"     0.23  0.0000100
anovaBF(score~group, data=preOrt)
# [1] group : 3.715357 ±0.01%
lmBF(score~group, data=subset(preOrt,preOrt$time=='precision_ort_post'))
# [1] group : 3192.428 ±0%

#effect of time at each level of group
capOrt <- select(capOrt,-session)
preOrt <- select(preOrt,-session)
one.way21 <-capOrt %>%
  group_by(group) %>% 
  anova_test(dv = score, wid = Row.names, within= time, effect.size = "ges") %>% 
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way21

one.way22 <-preOrt %>%
  group_by(group) %>% 
  anova_test(dv = score, wid = Row.names, within= time, effect.size = "pes") %>% 
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way22
# # A tibble: 2 × 9
# group      Effect   DFn   DFd     F           p `p<.05`   pes       p.adj
# <fct>      <chr>  <dbl> <dbl> <dbl>       <dbl> <chr>   <dbl>       <dbl>
#   1 control    time       1    43  4.22 0.046       *       0.089 0.092      
# 2 experiment time       1    37 39.5  0.000000259 *       0.516 0.000000518
preOrt %>%
  group_by(group) %>% 
  anova_test(dv = score, wid = Row.names, within= time, effect.size = "ges") %>% 
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
# # A tibble: 2 × 9
# group      Effect   DFn   DFd     F           p `p<.05`   ges       p.adj
# <fct>      <chr>  <dbl> <dbl> <dbl>       <dbl> <chr>   <dbl>       <dbl>
#   1 control    time       1    43  4.22 0.046       *       0.026 0.092      
# 2 experiment time       1    37 39.5  0.000000259 *       0.163 0.000000518
bf <- anovaBF(score~time, data=preOrt)
# [1] time : 0.3479411 ±0.03%
1/bf
# [1] Intercept only : 2.874049 ±0.03%
lmBF(score~time, data=subset(preOrt,preOrt$group=='experiment'))
# [1] time : 87.23431 ±0%
bf <- lmBF(score~time, data=subset(preOrt,preOrt$group=='control'))
# [1] time : 0.6054244 ±0.01%
1/bf
# [1] Intercept only : 1.651734 ±0.01%
preOrt %>%
  group_by(group) %>% 
  anova_test(dv = score, wid = Row.names, within= time, effect.size = "ges") %>% 
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
# group      Effect   DFn   DFd     F           p `p<.05`   ges       p.adj
# <fct>      <chr>  <dbl> <dbl> <dbl>       <dbl> <chr>   <dbl>       <dbl>
#   1 control    time       1    43  4.22 0.046       *       0.026 0.092      
# 2 experiment time       1    37 39.5  0.000000259 *       0.163 0.000000518

# > one.way
# # A tibble: 2 × 9
# time              Effect   DFn   DFd     F     p `p<.05`   ges p.adj
# <fct>             <chr>  <dbl> <dbl> <dbl> <dbl> <chr>   <dbl> <dbl>
#   1 capacity_ort_pre  group      1    80  4.07 0.047 "*"     0.048 0.094
# 2 capacity_ort_post group      1    80  3.84 0.053 ""      0.046 0.106
# 
#   > one.way2
# # A tibble: 2 × 9
# time               Effect   DFn   DFd      F          p `p<.05`   pes     p.adj
# <fct>              <chr>  <dbl> <dbl>  <dbl>      <dbl> <chr>   <dbl>     <dbl>
#   1 precision_ort_pre  group      1    80  0.458 0.5        ""      0.006 1        
# 2 precision_ort_post group      1    80 24.0   0.00000502 "*"     0.23  0.0000100
# 
# > one.way12
# # A tibble: 2 × 9
# group      Effect   DFn   DFd     F     p `p<.05`   ges p.adj
# <chr>      <chr>  <dbl> <dbl> <dbl> <dbl> <chr>   <dbl> <dbl>
#   1 control    time       1    43 11.5  0.002 *       0.041 0.004
# 2 experiment time       1    37  7.59 0.009 *       0.032 0.018
# 
# 
# > one.way22
# # A tibble: 2 × 9
# group      Effect   DFn   DFd     F           p `p<.05`   pes       p.adj
# <chr>      <chr>  <dbl> <dbl> <dbl>       <dbl> <chr>   <dbl>       <dbl>
#   1 control    time       1    43  4.22 0.046       *       0.089 0.092      
# 2 experiment time       1    37 39.5  0.000000259 *       0.516 0.000000518


#capacity in srt####
capSrt <- prePost %>% select(capacity_srt_pre,capacity_srt_post,group,Row.names) %>% 
  gather(key = "session", value = "score",capacity_srt_post, capacity_srt_pre) %>% 
  convert_as_factor(session,group) %>%  
  mutate(time=fct_relevel(session, 'capacity_srt_pre','capacity_srt_post')) %>% 
  group_by(time,group)

bxp <- ggboxplot(
  capSrt, x = 'time', y = "score", group = "session",
  #color = "group", palette =c("#00AFBB", "#E7B800"),
  color = "group", palette =c("#00AFBB", "#f88379"),
  add = "jitter", shape = "group") +
  labs(title="Training-induced changes in capacity in srt", x= "Time", y = "capacity")+
  scale_x_discrete(labels=c("capacity_srt_pre"= "pre test",'capacity_srt_post'='post test'))
bxp
#ggsave('srt_capacity_prepost',device = "png", path = prePostFolder)

summary <- rbind(summary, capSrt %>% get_summary_stats(score, type = "mean_sd"))
# # A tibble: 4 × 6
# group      time              variable     n  mean    sd
# <fct>      <fct>             <chr>    <dbl> <dbl> <dbl>
#   1 control    capacity_srt_pre  score       44  2.09 0.688
# 2 experiment capacity_srt_pre  score       38  2.28 0.745
# 3 control    capacity_srt_post score       44  2.23 0.729
# 4 experiment capacity_srt_post score       38  2.11 0.796


# outfilters
capSrt %>%
  group_by(time, group) %>%
  identify_outliers(score)

#Normality:If the data is normally distributed, the p-value should be greater than 0.05.Note that, if your sample size is greater than 50, the normal QQ plot is preferred because at larger sample sizes the Shapiro-Wilk test becomes very sensitive even to a minor deviation from normality.
capSrt%>%
  group_by(time, group) %>%
  shapiro_test(score)
ggqqplot(capSrt, "score", ggtheme = theme_bw()) +
  facet_grid(time ~ group)
#ggsave('srt_capacity_prepost_qq',device = "png", path = prePostFolder)


#Homogneity of variance assumption. There was homogeneity of variances, as assessed by Levene’s test (p > 0.05).
capSrt %>%
  group_by(time) %>%
  levene_test(score ~ group)


# # Two-way mixed ANOVA test
capSrt$Row.names <- factor(capSrt$Row.names) 
str(capSrt)
aT <- ezANOVA(data = capSrt, dv=.(score), wid = .(Row.names), between = .(group), within = .(time),type = 3, detailed = TRUE)
aovEffectSize(aT)
aovEffectSize(aT,effectSize = "ges")
# $ANOVA
# Effect DFn DFd          SSn      SSd            F            p p<.05          pes
# 1 (Intercept)   1  80 773.24393519 61.53813 1.005222e+03 4.656867e-47     * 0.9262823975
# 2       group   1  80   0.04333237 61.53813 5.633239e-02 8.129963e-01       0.0007036593
# 3        time   1  80   0.01044316 25.63553 3.258964e-02 8.571962e-01       0.0004072047
# 4  group:time   1  80   0.99327727 25.63553 3.099690e+00 8.212896e-02       0.0373008592
# 
# > aovEffectSize(aT,effectSize = "ges")
# $ANOVA
# Effect DFn DFd          SSn      SSd            F            p p<.05          ges
# 1 (Intercept)   1  80 773.24393519 61.53813 1.005222e+03 4.656867e-47     * 0.8986844762
# 2       group   1  80   0.04333237 61.53813 5.633239e-02 8.129963e-01       0.0004968341
# 3        time   1  80   0.01044316 25.63553 3.258964e-02 8.571962e-01       0.0001197828
# 4  group:time   1  80   0.99327727 25.63553 3.099690e+00 8.212896e-02       0.0112658702

bf <- anovaBF(score~group * time + Row.names,
              data = capSrt, whichRandom = 'Row.names')
bf
plot(bf)
bfInteraction = bf[4]/bf[3]
bfInteraction
# [1] group + Row.names                     : 0.2480268  ±0.71%
# [2] time + Row.names                      : 0.1645572  ±1.09%
# [3] group + time + Row.names              : 0.04119296 ±1.46%
# [4] group + time + group:time + Row.names : 0.04059774 ±8.87%

# [1] group + time + group:time + Row.names : 0.9855505 ±8.99%

1/bf[1]
# 4.031823 ±0.71%
1/bf[2]
# 6.076913 ±1.09%
1/bfInteraction
# 1.014661 ±8.99%
#precision in srt####
preSrt <- prePost %>% select(precision_srt_pre,precision_srt_post,group,Row.names) %>% 
  gather(key = "session", value = "score",precision_srt_post, precision_srt_pre) %>% 
  convert_as_factor(session,group) %>%  
  mutate(time=fct_relevel(session, 'precision_srt_pre','precision_srt_post')) %>% 
  group_by(time,group)
preSrt %>% get_summary_stats(score, type = "mean_sd")
# # A tibble: 4 × 6
# group      time               variable     n  mean    sd
# <fct>      <fct>              <chr>    <dbl> <dbl> <dbl>
#   1 control    precision_srt_pre  score       44 0.05  0.024
# 2 experiment precision_srt_pre  score       38 0.05  0.018
# 3 control    precision_srt_post score       44 0.047 0.028
# 4 experiment precision_srt_post score       38 0.056 0.023
preSrt$Row.names <- factor(preSrt$Row.names) 
str(preSrt)
aT <- ezANOVA(data = preSrt, dv=.(score), wid = .(Row.names), between = .(group), within = .(time),type = 3, detailed = TRUE)
aovEffectSize(aT)
aovEffectSize(aT,effectSize = "ges")
# $ANOVA
# Effect DFn DFd          SSn        SSd           F            p p<.05         pes
# 1 (Intercept)   1  80 0.4226366026 0.05915107 571.6029733 3.475531e-38     * 0.877225852
# 2       group   1  80 0.0007212315 0.05915107   0.9754433 3.263041e-01       0.012046162
# 3        time   1  80 0.0001755245 0.03106898   0.4519607 5.033424e-01       0.005617771
# 4  group:time   1  80 0.0009347176 0.03106898   2.4068188 1.247549e-01       0.029206549
# 
# Effect DFn DFd          SSn        SSd           F            p p<.05         ges
# 1 (Intercept)   1  80 0.4226366026 0.05915107 571.6029733 3.475531e-38     * 0.824083297
# 2       group   1  80 0.0007212315 0.05915107   0.9754433 3.263041e-01       0.007930738
# 3        time   1  80 0.0001755245 0.03106898   0.4519607 5.033424e-01       0.001941737
# 4  group:time   1  80 0.0009347176 0.03106898   2.4068188 1.247549e-01       0.010254182
bf <- anovaBF(score~group * time + Row.names,
              data = preSrt, whichRandom = 'Row.names')
bf
plot(bf)
bfInteraction = bf[4]/bf[3]
bfInteraction
1/bf[1]
1/bf[2]
1/bfInteraction
# [1] group + Row.names                     : 0.354426   ±3.34%
# [2] time + Row.names                      : 0.1946075  ±1.55%
# [3] group + time + Row.names              : 0.06669012 ±3.59%
# [4] group + time + group:time + Row.names : 0.04474212 ±4.78%
# [1] group + time + group:time + Row.names : 0.6708958 ±5.98%

# accurracy in VS####
accVS <- prePost %>% select(accPre,accPost,group,Row.names) %>% 
  gather(key = "session", value = "score",accPost, accPre) %>% 
  convert_as_factor(session,group) %>%  
  mutate(time=fct_relevel(session, 'accPre','accPost')) %>% 
  group_by(time,group)
accVS %>% get_summary_stats(score, type = "mean_sd")
# # A tibble: 4 × 6
# group      time    variable     n  mean    sd
# <fct>      <fct>   <chr>    <dbl> <dbl> <dbl>
#   1 control    accPre  score       44 0.779 0.089
# 2 experiment accPre  score       38 0.768 0.135
# 3 control    accPost score       44 0.865 0.096
# 4 experiment accPost score       38 0.811 0.12 

accVS$Row.names <- factor(accVS$Row.names) 
str(accVS)
aT <- ezANOVA(data = accVS, dv=.(score), wid = .(Row.names), between = .(group), within = .(time),type = 3, detailed = TRUE)
aovEffectSize(aT)
aovEffectSize(aT,effectSize = 'ges')
# $ANOVA
# Effect DFn DFd          SSn       SSd           F            p p<.05        pes
# 1 (Intercept)   1  80 105.90028387 1.5263088 5550.660953 1.130559e-75     * 0.98579208
# 2       group   1  80   0.04349505 1.5263088    2.279751 1.350127e-01       0.02770731
# 3        time   1  80   0.17144877 0.4154214   33.016842 1.592558e-07     * 0.29214090
# 4  group:time   1  80   0.01856176 0.4154214    3.574542 6.229229e-02       0.04277070
# $ANOVA
# Effect DFn DFd          SSn       SSd           F            p p<.05         ges
# 1 (Intercept)   1  80 105.90028387 1.5263088 5550.660953 1.130559e-75     * 0.981994678
# 2       group   1  80   0.04349505 1.5263088    2.279751 1.350127e-01       0.021909376
# 3        time   1  80   0.17144877 0.4154214   33.016842 1.592558e-07     * 0.081133106
# 4  group:time   1  80   0.01856176 0.4154214    3.574542 6.229229e-02       0.009468876

bf <- anovaBF(score~group * time + Row.names,
              data = accVS, whichRandom = 'Row.names')
bf
plot(bf)
bfInteraction = bf[4]/bf[3]
bfInteraction

# [1] group + Row.names                     : 0.6659423 ±1.36%
# [2] time + Row.names                      : 98621.49  ±3.83%
# [3] group + time + Row.names              : 67903.25  ±1.24%
# [4] group + time + group:time + Row.names : 74610.05  ±3.21%
# [1] group + time + group:time + Row.names : 1.09877 ±3.45%
1/bf[1]


# Reaction time  in VS####
mrtVS <- prePost %>% select(mrtPre,mrtPost,group,Row.names) %>% 
  gather(key = "session", value = "score",mrtPost, mrtPre) %>% 
  convert_as_factor(session, group) %>%  
  mutate(time=fct_relevel(session, 'mrtPre','mrtPost')) %>% 
  group_by(time,group)
sum <- mrtVS %>% get_summary_stats(score, type = "mean_sd")
# # A tibble: 4 × 6
# group      time    variable     n  mean    sd
# <fct>      <fct>   <chr>    <dbl> <dbl> <dbl>
#   1 control    mrtPre  score       44 3159.  471.
# 2 experiment mrtPre  score       38 3066.  781.
# 3 control    mrtPost score       44 2675.  497.
# 4 experiment mrtPost score       38 3010.  592.
# Two-way mixed ANOVA test
mrtVS$Row.names <- factor(mrtVS$Row.names) 
str(mrtVS)
aT <- ezANOVA(data = mrtVS, dv=.(score), wid = .(Row.names), between = .(group), within = .(time),type = 3, detailed = TRUE)
aovEffectSize(aT)
aovEffectSize(aT,effectSize = 'ges')
# $ANOVA
# Effect DFn DFd          SSn      SSd           F            p p<.05       pes
# 1 (Intercept)   1  80 1446098749.5 41751930 2770.839576 7.581483e-64     * 0.9719381
# 2       group   1  80     593363.5 41751930    1.136931 2.895090e-01       0.0140125
# 3        time   1  80    2971331.5 13961912   17.025356 8.974563e-05     * 0.1754733
# 4  group:time   1  80    1863510.0 13961912   10.677678 1.598933e-03     * 0.1177542
# Effect DFn DFd          SSn      SSd           F            p p<.05        ges
# 1 (Intercept)   1  80 1446098749.5 41751930 2770.839576 7.581483e-64     * 0.96290227
# 2       group   1  80     593363.5 41751930    1.136931 2.895090e-01       0.01053797
# 3        time   1  80    2971331.5 13961912   17.025356 8.974563e-05     * 0.05063172
# 4  group:time   1  80    1863510.0 13961912   10.677678 1.598933e-03     * 0.03236533
bf <- anovaBF(score~group * time + Row.names,
              data = mrtVS, whichRandom = 'Row.names')
bf
plot(bf)
bfInteraction = bf[4]/bf[3]
bfInteraction

# [1] group + Row.names                     : 0.3867708 ±0.81%
# [2] time + Row.names                      : 243.0138  ±1.68%
# [3] group + time + Row.names              : 99.49515  ±1.59%
# [4] group + time + group:time + Row.names : 2169.293  ±2.96%
# [1] group + time + group:time + Row.names : 21.803 ±3.36%
1/bf[1]



one.way <- mrtVS %>% 
  group_by(time) %>% 
  anova_test(dv = score, wid = Row.names, between = group,effect.size = "ges") %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

# > one.way
# # A tibble: 2 × 9
# time    Effect   DFn   DFd     F     p `p<.05`   ges p.adj
# <fct>   <chr>  <dbl> <dbl> <dbl> <dbl> <chr>   <dbl> <dbl>
#   1 mrtPre  group      1    80 0.441 0.509 ""      0.005 1    
# 2 mrtPost group      1    80 7.73  0.007 "*"     0.088 0.014

mrtVS <- select(mrtVS,-session)
one.way21 <-mrtVS %>%
  group_by(group) %>% 
  anova_test(dv = score, wid = Row.names, within= time, effect.size = "ges") %>% 
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way21
# # A tibble: 2 × 9
# group      Effect   DFn   DFd      F           p `p<.05`   ges      p.adj
# <fct>      <chr>  <dbl> <dbl>  <dbl>       <dbl> <chr>   <dbl>      <dbl>
#   1 control    time       1    43 42.4   0.000000065 "*"     0.203 0.00000013
# 2 experiment time       1    37  0.253 0.618       ""      0.002 1

# capacity  in ODT####
kODT <- prePost %>% select(kPre,kPost,group,Row.names) %>% 
  gather(key = "session", value = "score",kPost, kPre) %>% 
  convert_as_factor(session,group) %>%  
  mutate(time=fct_relevel(session, 'kPre','kPost')) %>% 
  group_by(time,group)
kODT %>% get_summary_stats(score, type = "mean_sd")
# group      time  variable     n  mean    sd
# <fct>      <fct> <chr>    <dbl> <dbl> <dbl>
#   1 control    kPre  score       44  2.08 0.768
# 2 experiment kPre  score       38  2.22 1.05 
# 3 control    kPost score       44  2.03 0.688
# 4 experiment kPost score       38  2.44 0.735
# Two-way mixed ANOVA test
kODT$Row.names <- factor(kODT$Row.names) 
str(kODT)
aT <- ezANOVA(data = kODT, dv=.(score), wid = .(Row.names), between = .(group), within = .(time),type = 3, detailed = TRUE)
aovEffectSize(aT)
aovEffectSize(aT,effectSize = 'ges')
# $ANOVA
# Effect DFn DFd         SSn      SSd          F            p p<.05        pes
# 1 (Intercept)   1  80 48.96495764 5.438091 720.325746 9.209764e-42     * 0.90004070
# 2       group   1  80  0.18525337 5.438091   2.725271 1.026920e-01       0.03294363
# 3        time   1  80  0.01699534 1.208878   1.124701 2.920998e-01       0.01386385
# 4  group:time   1  80  0.04902291 1.208878   3.244191 7.544516e-02       0.03897199
# 
# > aovEffectSize(aT,effectSize = 'ges')
# $ANOVA
# Effect DFn DFd         SSn      SSd          F            p p<.05         ges
# 1 (Intercept)   1  80 48.96495764 5.438091 720.325746 9.209764e-42     * 0.880475837
# 2       group   1  80  0.18525337 5.438091   2.725271 1.026920e-01       0.027114657
# 3        time   1  80  0.01699534 1.208878   1.124701 2.920998e-01       0.002550334
# 4  group:time   1  80  0.04902291 1.208878   3.244191 7.544516e-02       0.007321233
bf <- anovaBF(score~group * time + Row.names,
              data = kODT, whichRandom = 'Row.names')
bf
plot(bf)
bfInteraction = bf[4]/bf[3]
bfInteraction
1/bf[1]
1/bf[2]
1/bfInteraction
# [1] group + Row.names                     : 0.9744223 ±6.12%
# [2] time + Row.names                      : 0.2479615 ±1.52%
# [3] group + time + Row.names              : 0.227634  ±4.3%
# [4] group + time + group:time + Row.names : 0.2089046 ±4.7%
#[1] group + time + group:time + Row.names : 0.9177215 ±6.37%

# During training (four training sessions) ####
# in experimental group####
# capacity----
capT <- groupedFinal %>% 
  filter(group == 'experiment') %>% 
  select(Row.names, capacity_ortT_t1_ss2,capacity_ortT_t2_ss2,capacity_ortT_t3_ss2,capacity_ortT_t4_ss2,capacity_ortT_t1_ss4,capacity_ortT_t2_ss4,capacity_ortT_t3_ss4,capacity_ortT_t4_ss4,capacity_ortT_t1_ss6,capacity_ortT_t2_ss6,capacity_ortT_t3_ss6,capacity_ortT_t4_ss6)

capT2 <- capT %>% 
  gather(key = "type", value = "score",  capacity_ortT_t1_ss2,capacity_ortT_t2_ss2,capacity_ortT_t3_ss2,capacity_ortT_t4_ss2,capacity_ortT_t1_ss4,capacity_ortT_t2_ss4,capacity_ortT_t3_ss4,capacity_ortT_t4_ss4,capacity_ortT_t1_ss6,capacity_ortT_t2_ss6,capacity_ortT_t3_ss6,capacity_ortT_t4_ss6)

capT3 <- capT2 %>% 
  mutate(time = substr(type, 15, 16)) %>% 
  mutate(setsize = substr(type, 20, 20)) 

# capacity
ss <- as.numeric(capT3$setsize)
Pm <- capT3$score/ss
capT3 <- cbind(capT3,Pm)

duringSummary <- capT3 %>% 
  group_by(setsize,time) %>% 
  get_summary_stats(score,type='mean_sd')

ggline(duringSummary,x= 'time',y='mean',linetype = 'setsize',shape = 'setsize') 

TCapLine <- ggline(capT3,x='time', y='score',linetype = 'setsize',shape = 'setsize',add=c("mean"),color = 'setsize')+
  labs( x= ' ', y = expression("Capacity"~italic(K)))+
  scale_x_discrete(labels=c("t1"= "training 1","t2"= "training 2","t3"= "training 3","t4"= "training 4"))+
  theme(legend.position = "none")
TCapLine


ggline(capT3,x='time', y='Pm',linetype = 'setsize',shape = 'setsize',add=c("mean"),color = 'setsize')

ggplot(data = duringSummary,aes(x=time,y=mean,group=setsize))+
  geom_line(aes(linetype=setsize))+
  scale_shape_manual(values=c(15,16,17))+
  geom_point(aes(shape=setsize))



duringORTSummary <- capT3 %>% 
  group_by(setsize,time) %>% 
  get_summary_stats(score,type='mean_sd')

summaryORTDuring <- duringORTSummary
group <- c('experiment','experiment','experiment','experiment','experiment','experiment','experiment','experiment','experiment','experiment','experiment','experiment')
summaryORTDuring <- cbind(summaryORTDuring,group)

ggboxplot(
  capT3, x = 'time', y = "score", group = "setsize",
  color = "setsize", palette ="jco") +
  labs(title="Capacity changes during training", x= "Time", y = "Capacity(Pm)")
#ggsave('During ORT',device = "png", path = prePostFolder)        

#outliers
capT3 %>% 
  group_by(setsize,time) %>% 
  identify_outliers(score)

capT3%>% 
  group_by(setsize,time) %>% 
  identify_outliers(Pm)

# normality
capT3 %>% 
  group_by(setsize,time) %>% 
  shapiro_test(score)
capT3 %>% 
  group_by(setsize,time) %>% 
  shapiro_test(Pm)

ggqqplot(capT3, "score", ggtheme = theme_bw()) +
  facet_grid(time ~ setsize, labeller = "label_both")
#ggsave('During_ORT_qq',device = "png", path = prePostFolder) 

capT3 <- select(capT3,-type,score)
capT3$Row.names <- factor(capT3$Row.names)
capT3$time <- factor(capT3$time)
capT3$setsize <- factor(capT3$setsize)
str(capT3)
aT <- ezANOVA(data = capT3, dv=.(score), wid = .(Row.names), within = .(time, setsize),type = 1, detailed = TRUE)
aovEffectSize(aT,effectSize = "ges")
# $ANOVA
# Effect DFn DFd         SSn      SSd         F            p p<.05         ges
# 1         time   3 111   0.9899792 25.51387  1.435659 2.362518e-01       0.006779637
# 2      setsize   2  74 137.0852657 88.52122 57.298748 9.262848e-16     * 0.485915084
# 3 time:setsize   6 222   1.4995871 30.99739  1.789981 1.021861e-01       0.010233849
# 


aovEffectSize(aT,effectSize = "pes")
# $ANOVA
# Effect DFn DFd         SSn      SSd         F            p p<.05        pes
# 1         time   3 111   0.9899792 25.51387  1.435659 2.362518e-01       0.03735228
# 2      setsize   2  74 137.0852657 88.52122 57.298748 9.262848e-16     * 0.60763000
# 3 time:setsize   6 222   1.4995871 30.99739  1.789981 1.021861e-01       0.04614544    

bf <- anovaBF(score~setsize * time + Row.names,
              data = capT3, whichRandom = 'Row.names')
bf
bf[4]/bf[3]
plot(bf)

# Bayes factor analysis
# [1] time + Row.names                          : 0.01665796   ±0.74%
# [2] setsize + Row.names                       : 3.39158e+56  ±1.47%
# [3] time + setsize + Row.names                : 1.030828e+55 ±1.15%
# [4] time + setsize + time:setsize + Row.names : 3.193407e+53 ±37.78%
# Against denominator:
#   score ~ Row.names 
# ---
#   Bayes factor type: BFlinearModel, JZS
# 
# > bf[4]/bf[3]
# Bayes factor analysis
# [1] time + setsize + time:setsize + Row.names : 0.03097905 ±37.8%
# 
# Against denominator:
#   score ~ time + setsize + Row.names 
# ---
#   Bayes factor type: BFlinearModel, JZS


# capT4 <- select(capT3,-type,Pm)
# capT4$Row.names <- factor(capT4$Row.names)
# capT4$time <- factor(capT4$time)
# capT4$setsize <- factor(capT4$setsize)
# str(capT4)
# aT <- ezANOVA(data = capT4, dv=.(Pm), wid = .(Row.names), within = .(time, setsize),type = 1, detailed = TRUE)
# aovEffectSize(aT,effectSize = "ges")
# aovEffectSize(aT,effectSize = "pes")
## same pattern of results

# comparisons for setsize variable
capT3 %>% 
  pairwise_t_test(
    score ~ setsize, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )
capT3 %>%
  cohens_d(score ~ setsize, paired = TRUE)

# # A tibble: 3 x 10
# .y.   group1 group2    n1    n2 statistic    df        p    p.adj p.adj.signif
# * <chr> <chr>  <chr>  <int> <int>     <dbl> <dbl>    <dbl>    <dbl> <chr>       
#   1 score 2      4        152   152    -19.5    151 4.56e-43 1.37e-42 ****        
#   2 score 2      6        152   152    -13.4    151 1.66e-27 4.98e-27 ****        
#   3 score 4      6        152   152     -4.92   151 2.29e- 6 6.87e- 6 ****        
#   > capT3 %>%
#   +   cohens_d(score ~ setsize, paired = TRUE)
# # A tibble: 3 × 7
# .y.   group1 group2 effsize    n1    n2 magnitude
# * <chr> <chr>  <chr>    <dbl> <int> <int> <ord>    
#   1 score 2      4       -1.58    152   152 large    
# 2 score 2      6       -1.09    152   152 large    
# 3 score 4      6       -0.399   152   152 small   

ttestBF(x = capT3$score[capT3$setsize==2], y = capT3$score[capT3$setsize==4], paired=TRUE)
ttestBF(x = capT3$score[capT3$setsize==2], y = capT3$score[capT3$setsize==6], paired=TRUE)
ttestBF(x = capT3$score[capT3$setsize==4], y = capT3$score[capT3$setsize==6], paired=TRUE)
# > ttestBF(x = capT3$score[capT3$setsize==2], y = capT3$score[capT3$setsize==4], paired=TRUE)
# t is large; approximation invoked.
# Bayes factor analysis
#   [1] Alt., r=0.707 : 4.833156e+39 ±0%
# 
# Against denominator:
#   Null, mu = 0 
# ---
#   Bayes factor type: BFoneSample, JZS
# 
# > ttestBF(x = capT3$score[capT3$setsize==2], y = capT3$score[capT3$setsize==6], paired=TRUE)
# Bayes factor analysis
# [1] Alt., r=0.707 : 2.134867e+24 ±0%
# 
# Against denominator:
#   Null, mu = 0 
# ---
#   Bayes factor type: BFoneSample, JZS
# 
# > ttestBF(x = capT3$score[capT3$setsize==4], y = capT3$score[capT3$setsize==6], paired=TRUE)
# Bayes factor analysis
# [1] Alt., r=0.707 : 5492.88 ±0%
# 
# Against denominator:
#   Null, mu = 0 
# ---
#   Bayes factor type: BFoneSample, JZS
#precision ----
preT <- groupedFinal %>% 
  filter(group == 'experiment') %>% 
  select(Row.names, precision_ortT_t1_ss2,precision_ortT_t2_ss2,precision_ortT_t3_ss2,precision_ortT_t4_ss2,precision_ortT_t1_ss4,precision_ortT_t2_ss4,precision_ortT_t3_ss4,precision_ortT_t4_ss4,precision_ortT_t1_ss6,precision_ortT_t2_ss6,precision_ortT_t3_ss6,precision_ortT_t4_ss6)

preT2 <- preT %>% 
  gather(key = "type", value = "score",  precision_ortT_t1_ss2,precision_ortT_t2_ss2,precision_ortT_t3_ss2,precision_ortT_t4_ss2,precision_ortT_t1_ss4,precision_ortT_t2_ss4,precision_ortT_t3_ss4,precision_ortT_t4_ss4,precision_ortT_t1_ss6,precision_ortT_t2_ss6,precision_ortT_t3_ss6,precision_ortT_t4_ss6)

preT3 <- preT2 %>% 
  mutate(time = substr(type, 16, 17)) %>% 
  mutate(setsize = substr(type, 21, 21))

duringORTSummary2 <-  preT3 %>% 
  group_by(setsize,time) %>% 
  get_summary_stats(score,type='mean_sd')

ggline(duringORTSummary2,x= 'time',y='mean',linetype = 'setsize',shape = 'setsize')

ggline(preT3,x='time', y='score',linetype = 'setsize',shape = 'setsize',add=c("mean"),color = 'setsize')
#ggsave('During ORT_color_line',device = "png", path = prePostFolder)        

TPreLine <- ggline(preT3,x='time', y='score',linetype = 'setsize',shape = 'setsize',add=c("mean"),color = 'setsize')+
  labs( x= ' ', y = expression('Precision'~italic('SD')^-1))+
  scale_x_discrete(labels=c("t1"= "training 1","t2"= "training 2","t3"= "training 3","t4"= "training 4"))+
  theme(legend.position = "none")+
  scale_y_continuous(labels = trans_100) 
TPreLine
plot_grid(TCapLine,TPreLine,labels = "AUTO")

#setsize effect for both capacity and precision####

SSTCapLine <- ggline(capT3,x='setsize', y='score',add=c("mean"),shape  = 'time',color = 'time', palette =c("#1B9E77","#D95F02", "#7570B3", "#E7298A"))+
  labs( x= 'Set Size', y = expression("Capacity"~italic(K)), col = 'Time', shape = 'Time')+
  theme(legend.position = c(0.9,0.3))
SSTCapLine
SSTPreLine <- ggline(preT3,x='setsize', y='score',add=c("mean"),shape  = 'time',color = 'time', palette =c("#1B9E77","#D95F02", "#7570B3", "#E7298A"))+
  labs( x= 'Set Size', y = expression('Precision'~italic('SD')^-1), col = 'Time', shape = 'Time')+
  scale_y_continuous(labels = trans_100)+ 
  theme(legend.position = 'none')
SSTPreLine
plot_grid(SSTCapLine,SSTPreLine,labels = "AUTO")


# setsize effect end----

summaryORTDuring2 <- duringORTSummary2
group <- c('experiment','experiment','experiment','experiment','experiment','experiment','experiment','experiment','experiment','experiment','experiment','experiment')
summaryORTDuring2 <- cbind(summaryORTDuring2,group)

ggboxplot(
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
#gsave('During_ORT_precision_qq',device = "png", path = prePostFolder) 

preT3$Row.names <- factor(preT3$Row.names)
preT3$time <- factor(preT3$time)
preT3$setsize <- factor(preT3$setsize)
str(preT3)
aT <- ezANOVA(data = preT3, dv=.(score), wid = .(Row.names), within = .(time, setsize),type = 3, detailed = TRUE)
aovEffectSize(aT,effectSize = "ges")
# $ANOVA
# Effect DFn DFd         SSn       SSd           F            p p<.05        ges
# 1  (Intercept)   1  37 2.457603234 0.1321770 687.9509073 1.666121e-25     * 0.71527128
# 2         time   3 111 0.009107665 0.2346150   1.4363262 2.360615e-01       0.00922381
# 3      setsize   2  74 0.026902560 0.1771519   5.6188773 5.347978e-03     * 0.02676331
# 4 time:setsize   6 222 0.010344125 0.4343565   0.8811485 5.094600e-01       0.01046294

aovEffectSize(aT,effectSize = "pes")
# $ANOVA
# Effect DFn DFd         SSn       SSd           F            p p<.05        pes
# 1  (Intercept)   1  37 2.457603234 0.1321770 687.9509073 1.666121e-25     * 0.94896206
# 2         time   3 111 0.009107665 0.2346150   1.4363262 2.360615e-01       0.03736898
# 3      setsize   2  74 0.026902560 0.1771519   5.6188773 5.347978e-03     * 0.13184011
# 4 time:setsize   6 222 0.010344125 0.4343565   0.8811485 5.094600e-01       0.02326087
            

bf <- anovaBF(score~setsize * time + Row.names,
              data = preT3, whichRandom = 'Row.names')
bf
bf[4]/bf[3]
plot(bf)
# Bayes factor analysis
# --------------
#   [1] time + Row.names                          : 0.06234101 ±0.7%
# [2] setsize + Row.names                       : 11.62073   ±1.32%
# [3] time + setsize + Row.names                : 0.7732324  ±1.15%
# [4] time + setsize + time:setsize + Row.names : 0.03274688 ±36.88%
# > bf[4]/bf[3]
# Bayes factor analysis
# --------------
#   [1] time + setsize + time:setsize + Row.names : 0.04235063 ±36.9%
# 
# Against denominator:
#   score ~ time + setsize + Row.names 
# ---
#   Bayes factor type: BFlinearModel, JZS

# comparisons for setsize variable
preT3 %>% 
  pairwise_t_test(
    score ~ setsize, paired = TRUE,
    p.adjust.method = "bonferroni"
  )

preT3 %>%
  cohens_d(score ~ setsize, paired = TRUE)
# .y.   group1 group2    n1    n2 statistic    df        p    p.adj p.adj.signif
# * <chr> <chr>  <chr>  <int> <int>     <dbl> <dbl>    <dbl>    <dbl> <chr>       
#   1 score 2      4        184   184    15.8     183 6.34e-36 1.90e-35 ****        
#   2 score 2      6        184   184     3.08    183 2.00e- 3 7.00e- 3 **          
#   3 score 4      6        184   184    -0.616   183 5.39e- 1 1.00e+ 0 ns  
# .y.   group1 group2 effsize    n1    n2 magnitude 
# * <chr> <chr>  <chr>    <dbl> <int> <int> <ord>     
#   1 score 2      4       1.16     184   184 large     
# 2 score 2      6       0.227    184   184 small     
# 3 score 4      6      -0.0454   184   184 negligible

ttestBF(x = preT3$score[preT3$setsize==2], y = preT3$score[preT3$setsize==4], paired=TRUE)
ttestBF(x = preT3$score[preT3$setsize==2], y = preT3$score[preT3$setsize==6], paired=TRUE)
ttestBF(x = preT3$score[preT3$setsize==4], y = preT3$score[preT3$setsize==6], paired=TRUE)

#Experimental group ss=4 for all session ####

T_cap <- groupedFinal %>% 
  filter(group == 'experiment') %>% 
  select(Row.names,capacity_ortT_t1_ss4,capacity_ortT_t2_ss4,capacity_ortT_t3_ss4,capacity_ortT_t4_ss4,capacity_ortT_t1_ss2,capacity_ortT_t2_ss2,capacity_ortT_t3_ss2,capacity_ortT_t4_ss2,capacity_ortT_t1_ss6,capacity_ortT_t2_ss6,capacity_ortT_t3_ss6,capacity_ortT_t4_ss6)
T_cap2<- T_cap %>% 
  gather(key = "type", value = "score",  capacity_ortT_t1_ss4,capacity_ortT_t2_ss4,capacity_ortT_t3_ss4,capacity_ortT_t4_ss4,capacity_ortT_t1_ss2,capacity_ortT_t2_ss2,capacity_ortT_t3_ss2,capacity_ortT_t4_ss2,capacity_ortT_t1_ss6,capacity_ortT_t2_ss6,capacity_ortT_t3_ss6,capacity_ortT_t4_ss6) %>% 
  mutate(time = substr(type, 15, 16)) %>% 
  mutate(setsize = substr(type, 20, 20))

prepost_cap <- groupedFinal %>% 
  filter(group == 'experiment') %>% 
  select(Row.names, capacity_ort_pre,capacity_ort_post)
prepost_cap1 <- prepost_cap %>% 
  gather(key = "type", value = "score",  capacity_ort_pre,capacity_ort_post)
prepost_cap2 <- prepost_cap1 %>% 
  mutate(time = substr(type, 14, nchar(type))) %>% 
  mutate(setsize = "4")


all_cap <- rbind(T_cap2,prepost_cap2)
all_cap$time <- factor(all_cap$time, levels=c("pre", "t1", "t2","t3","t4","post"))
all_cap$setsize <- factor(all_cap$setsize, levels=c("2", "4", "6"))

str(all_cap)
  
summary <-all_cap %>% 
  group_by(time,setsize) %>% 
  get_summary_stats(score) 
ggplot() +
  geom_point(data = all_cap,aes(x=time,y=score,color = setsize,shape = setsize),position = position_jitterdodge(0.2),alpha = .2)+
  geom_errorbar(data = summary,aes(x = time, ymin=mean-se, ymax=mean+se,group=setsize), linetype = "solid", width=.2, position = position_dodge(0.8))+
  geom_point(data = summary,aes(x=time,y=mean, colour = setsize, shape = setsize),size = 3,position = position_dodge(0.8))+
  geom_line(data = summary,aes(x=time,y=mean, group = setsize, colour = setsize,linetype = setsize),position = position_dodge(0.8))+
  scale_linetype_manual(values=c("dotted", "solid","dashed"))+
  labs( x= "Time", y = expression(~bold("Mean Capacity"~bolditalic(K))),size =1)+
  scale_x_discrete(labels=c("pre"= "Pre-Test","t1"= "S1", "t2" = "S2" ,"t3" = "S3","t4" = "S4",'post'='Post-Test'))+
  scale_y_continuous(labels = function(x) format(x, nsmall = 1)) +
  theme(legend.position = "none",legend.title=element_blank(),  element_line(colour = 'black',size = 1),panel.background=element_blank(),axis.line.y.left = element_line(size = 1),axis.line.x.bottom  = element_line(size = 1),axis.text = element_text(face="bold"),axis.title = element_text(face="bold"),text = element_text(size = 12))
ggsave("cap_all.png",width = 4.06,height = 4.06)


T_pre <- groupedFinal %>% 
  filter(group == 'experiment') %>% 
  select(Row.names,precision_ortT_t1_ss4,precision_ortT_t2_ss4,precision_ortT_t3_ss4,precision_ortT_t4_ss4,precision_ortT_t1_ss2,precision_ortT_t2_ss2,precision_ortT_t3_ss2,precision_ortT_t4_ss2,precision_ortT_t1_ss6,precision_ortT_t2_ss6,precision_ortT_t3_ss6,precision_ortT_t4_ss6)
T_pre2<- T_pre %>% 
  gather(key = "type", value = "score",  precision_ortT_t1_ss4,precision_ortT_t2_ss4,precision_ortT_t3_ss4,precision_ortT_t4_ss4,precision_ortT_t1_ss2,precision_ortT_t2_ss2,precision_ortT_t3_ss2,precision_ortT_t4_ss2,precision_ortT_t1_ss6,precision_ortT_t2_ss6,precision_ortT_t3_ss6,precision_ortT_t4_ss6) %>% 
  mutate(time = substr(type, 16, 17)) %>% 
  mutate(setsize = substr(type, 21, 22))

prepost_pre <- groupedFinal %>% 
  filter(group == 'experiment') %>% 
  select(Row.names, precision_ort_pre,precision_ort_post)
prepost_pre1 <- prepost_pre %>% 
  gather(key = "type", value = "score",  precision_ort_pre,precision_ort_post)
prepost_pre2 <- prepost_pre1 %>% 
  mutate(time = substr(type, 15, nchar(type))) %>% 
  mutate(setsize = "4")


all_pre <- rbind(T_pre2,prepost_pre2)
all_pre$time <- factor(all_pre$time, levels=c("pre", "t1", "t2","t3","t4","post"))
all_pre$setsize <- factor(all_pre$setsize, levels=c("2", "4", "6"))

str(all_pre)

summary <-all_pre %>% 
  group_by(time,setsize) %>% 
  get_summary_stats(score) 

  
squish_trans <- function(from, to, factor) {
  
  trans <- function(x) {
    
    if (any(is.na(x))) return(x)
    
    # get indices for the relevant regions
    isq <- x > from & x < to
    ito <- x >= to
    
    # apply transformation
    x[isq] <- from + (x[isq] - from)/factor
    x[ito] <- from + (to - from)/factor + (x[ito] - to)
    
    return(x)
  }
  
  inv <- function(x) {
    
    if (any(is.na(x))) return(x)
    
    # get indices for the relevant regions
    isq <- x > from & x < from + (to - from)/factor
    ito <- x >= from + (to - from)/factor
    
    # apply transformation
    x[isq] <- from + (x[isq] - from) * factor
    x[ito] <- to + (x[ito] - (from + (to - from)/factor))
    
    return(x)
  }
  
  # return the transformation
  return(trans_new("squished", trans, inv))
}

ggplot() +
  geom_point(data = all_pre,aes(x=time,y=score,color = setsize,shape = setsize),position = position_jitterdodge(0.2),alpha = .2)+
  geom_errorbar(data = summary,aes(x = time, ymin=mean-se, ymax=mean+se,group=setsize), linetype = "solid", width=.2, position = position_dodge(0.8))+
  geom_point(data = summary,aes(x=time,y=mean, colour = setsize, shape = setsize),size = 3,position = position_dodge(0.8))+
  geom_line(data = summary,aes(x=time,y=mean, group = setsize, colour = setsize,linetype = setsize),position = position_dodge(0.8))+
  scale_linetype_manual(values=c("dotted", "solid","dashed"))+
  labs( x= "Time", y = expression(~bold("Mean Precision"~bolditalic(SD^-1))),size =1)+
  scale_x_discrete(labels=c("pre"= "Pre-Test","t1"= "S1", "t2" = "S2" ,"t3" = "S3","t4" = "S4",'post'='Post-Test'))+
  scale_y_continuous(labels = function(x) x * 100, limits = c(0,0.15)) +
  theme(
    legend.position = "none",
    legend.key = element_rect(fill = "transparent", colour = "transparent"),
    legend.title=element_blank(),  element_line(colour = 'black',size = 1),panel.background=element_blank(),axis.line.y.left = element_line(size = 1),axis.line.x.bottom  = element_line(size = 1),axis.text = element_text(face="bold"),axis.title = element_text(face="bold"),text = element_text(size = 12))
ggplot() +
  geom_point(data = all_pre,aes(x=time,y=score,color = setsize,shape = setsize),position = position_jitterdodge(0.2),alpha = .2)+
  geom_errorbar(data = summary,aes(x = time, ymin=mean-se, ymax=mean+se,group=setsize), linetype = "solid", width=.2, position = position_dodge(0.8))+
  geom_point(data = summary,aes(x=time,y=mean, colour = setsize, shape = setsize),size = 3,position = position_dodge(0.8))+
  geom_line(data = summary,aes(x=time,y=mean, group = setsize, colour = setsize,linetype = setsize),position = position_dodge(0.8))+
  scale_linetype_manual(values=c("dotted", "solid","dashed"))+
  labs( x= "Time", y = expression(~bold("Mean Precision"~bolditalic(SD^-1))),size =1)+
  scale_x_discrete(labels=c("pre"= "Pre-Test","t1"= "S1", "t2" = "S2" ,"t3" = "S3","t4" = "S4",'post'='Post-Test'))+
  scale_y_continuous(labels = function(x) x * 100
                     ) +
  theme(
    legend.position = "none",
    legend.key = element_rect(fill = "transparent", colour = "transparent"),
    legend.title=element_blank(),  element_line(colour = 'black',size = 1),panel.background=element_blank(),axis.line.y.left = element_line(size = 1),axis.line.x.bottom  = element_line(size = 1),axis.text = element_text(face="bold"),axis.title = element_text(face="bold"),text = element_text(size = 12))


ggsave("pre_all.png",width = 4.06,height = 4.06)

  



