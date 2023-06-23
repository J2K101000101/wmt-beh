# Run the analysis with participants who has very standard training dosage and further include those who potentially didn't follow the instruction according to the pre-registered exclusion criteria
# tables are just for checking purposes, need some editing in the manuscript
# 1. Demographics & comparison: table 1
# 2. Baseline performance comparison at pre-test: table 4
# 3. Pre-post comparisons Two-way mixed ANOVA test : Figure 4, table3,table 5
# 4. During four training sessions: Figure 3, table2, table6*as during anova in both groups
# 5. Exploratory Analysis for response density (Figure 5)

set.seed(1234321)
# install.packages('pacman')
library(pacman)
pacman::p_load(pacman,dplyr,tidyr,plyr,tidyverse,
               BayesFactor,rstatix,WRS2,ez,
               psychReport,
               raincloudplots,PupillometryR,ggpubr)
               
rm(list=ls()) 

workspace = getwd()
outputFolder = paste(workspace, "output", sep = "/")
shareableRawFolder =paste(workspace, "shareableRaw", sep = "/")
analysedResults =paste(workspace, "analysedResults", sep = "/")

#clear up
removeExisting <- list.files(analysedResults,include.dirs = F,full.names = T, recursive = T)
file.remove(removeExisting)

demographicsRead <- read.csv(paste(shareableRawFolder, 'demographics_pre_exclusion.csv',sep='/'), header = TRUE, sep= ',')
demographics <- demographicsRead %>% 
  subset( status== 'APPROVED')%>% 
  select(extId, age,Sex)
readFiles <- list.files(path = outputFolder, pattern="*.csv", full.names = TRUE)
final <- lapply(readFiles, read.csv, row.names = 1,header = TRUE, sep=',', stringsAsFactors = FALSE)
COL<-unique(unlist(lapply(final, colnames)))
ROW<-unique(unlist(lapply(final, rownames)))
TOTAL<-matrix(data=NA, nrow=length(ROW), ncol=length(COL), dimnames=list(ROW, COL))
for (df in final) { 
  TOTAL[rownames(df), colnames(df)] <- as.matrix(df)
}
included <- as.data.frame(TOTAL)
includedDemographics <- demographics[(demographics$extId %in% row.names(included)),]
includedTable <- data.frame(includedDemographics$age,includedDemographics$Sex,row.names = includedDemographics$extId)
finalTable <- merge(included,includedTable,by="row.names")


experimentalGroup <- finalTable %>% 
  filter(!is.na(capacity_ortT_t1_ss2) & !is.na(capacity_ortT_t2_ss2) & !is.na(capacity_ortT_t3_ss2) & !is.na(capacity_ortT_t4_ss2)& !is.na(capacity_ortT_t1_ss4) & !is.na(capacity_ortT_t2_ss4) & !is.na(capacity_ortT_t3_ss4) & !is.na(capacity_ortT_t4_ss4) & !is.na(capacity_ortT_t1_ss6) & !is.na(capacity_ortT_t2_ss6) & !is.na(capacity_ortT_t3_ss6) & !is.na(capacity_ortT_t4_ss6) & !is.na(precision_ortT_t1_ss2) & !is.na(precision_ortT_t2_ss2) & !is.na(precision_ortT_t3_ss2) & !is.na(precision_ortT_t4_ss2)& !is.na(precision_ortT_t1_ss4) & !is.na(precision_ortT_t2_ss4) & !is.na(precision_ortT_t3_ss4) & !is.na(precision_ortT_t4_ss4) & !is.na(precision_ortT_t1_ss6) & !is.na(precision_ortT_t2_ss6) & !is.na(precision_ortT_t3_ss6) & !is.na(precision_ortT_t4_ss6)) %>% 
  cbind(group = 'experiment')
controlGroup <- finalTable %>% 
  filter(!is.na(t1_ss8.vsAcc) & !is.na(t2_ss8.vsAcc) & !is.na(t3_ss8.vsAcc) & !is.na(t4_ss8.vsAcc) & !is.na(t1_ss16.vsAcc) & !is.na(t2_ss16.vsAcc) & !is.na(t3_ss16.vsAcc) & !is.na(t4_ss16.vsAcc)  & !is.na(t1_ss24.vsAcc) & !is.na(t2_ss24.vsAcc) & !is.na(t3_ss24.vsAcc) & !is.na(t4_ss24.vsAcc)  & !is.na(t1_ss8.vsMrt) & !is.na(t2_ss8.vsMrt) & !is.na(t3_ss8.vsMrt) & !is.na(t4_ss8.vsMrt) & !is.na(t1_ss16.vsMrt) & !is.na(t2_ss16.vsMrt) & !is.na(t3_ss16.vsMrt) & !is.na(t4_ss16.vsMrt)  & !is.na(t1_ss24.vsMrt) & !is.na(t2_ss24.vsMrt) & !is.na(t3_ss24.vsMrt) & !is.na(t4_ss24.vsMrt)) %>% 
  cbind(group = 'control')
groupedFinal <- rbind(experimentalGroup,controlGroup)
# experimental group: 38; control group 44
groupInfo <- groupedFinal %>% 
  select(Row.names,group)

# demographics(Table 1)####

# Table 1 start
table1 <- NULL

sex <- groupedFinal %>%
  dplyr::group_by(group,includedDemographics.Sex) %>%
  dplyr::summarise(nSex = dplyr::n()) %>% 
  ungroup()

age <- groupedFinal %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(mAge = mean(includedDemographics.age),
                   sdAge = sd(includedDemographics.age),
  ) %>%  ungroup()

demSummary <- merge(sex,age)

# chi-Square test for categorical variable gender
baseline <- merge(includedTable,groupInfo,by.x = 0,by.y = 'Row.names')
bs.gender <- baseline %>% 
  select(gender = includedDemographics.Sex, group =  group)
bs.gender <- with(bs.gender, table(gender, group))
bs.gender.chis <- chisq.test(bs.gender)
bs.gender.bf <- contingencyTableBF(bs.gender, sampleType = 'poisson')
genderStat <- c('genderStat_test_p_bf_err',  bs.gender.chis[["statistic"]][["X-squared"]],bs.gender.chis[["p.value"]],exp(bs.gender.bf@bayesFactor[["bf"]]),bs.gender.bf@bayesFactor[["error"]])
table1 <- rbind(demSummary,genderStat)
# t test for age
bs.age <- baseline %>% 
  select(age= includedDemographics.age, group =  group)
bs.age %>% 
  group_by(group) %>% 
  identify_outliers(age)
bs.age %>% 
  group_by(group) %>% 
  shapiro_test(age)
bs.age %>% levene_test(age ~ group)
# wilcox.test(bs.age$age ~ bs.age$group)

bs.age.yuen <- yuen(age~group, data = bs.age)
bs.age.yuenEffects <- akp.effect(age~group, data = bs.age)
bf<- ttestBF(formula = age ~ group, data = bs.age)
bf
1/bf[1]
ageStat <- c('ageStat_test_p_bf_err', bs.age.yuen[["test"]], bs.age.yuen[["p.value"]],exp(bf@bayesFactor[["bf"]]),bf@bayesFactor[["error"]])
table1 <- rbind(table1,ageStat)
write.csv(table1,paste(analysedResults,'table1.csv',sep='/'))
# Table 1 end

#t-tests in pre-test (Table 4)####
pre <- groupedFinal %>% 
  select(group,accPre,mrtPre,kPre,capacity_ort_pre,capacity_srt_pre,precision_ort_pre,precision_srt_pre) %>% 
  group_by(group)

table4<- data.frame(matrix(ncol = 6, nrow = 7))
rownames(table4) <- c("capacity_ort_pre", "precision_ort_pre","acc_vs_pre","mrt_vs_pre", "capacity_srt_pre","precision_srt_pre","k_odt_pre")
colnames(table4) <- c('df','yuen_t','p','δt','BF10','BFerror' )
# ort_capacity
df_capacity_ort_pre <- groupedFinal %>% 
  select(group,capacity_ort_pre) 

pre_summary <- df_capacity_ort_pre %>% 
  group_by(group) %>% 
  get_summary_stats(capacity_ort_pre, type = "mean_sd")

# Check outlier(s)
df_capacity_ort_pre %>% 
  group_by(group) %>% 
  identify_outliers(capacity_ort_pre)

# Check Normality
df_capacity_ort_pre %>% 
  group_by(group) %>% 
  shapiro_test(capacity_ort_pre)

# Check homogeneity 
df_capacity_ort_pre <- df_capacity_ort_pre %>% convert_as_factor(group)
df_capacity_ort_pre %>% levene_test(capacity_ort_pre ~ group)

yuendata <- yuen(capacity_ort_pre~group, data = df_capacity_ort_pre)
yuendata
effect <- akp.effect(capacity_ort_pre~group, data = df_capacity_ort_pre)
effect
bf <- ttestBF(formula = capacity_ort_pre ~ group, data = df_capacity_ort_pre)
bf

table4[1,1] <- yuendata[["df"]]
table4[1,2] <- yuendata[["test"]]
table4[1,3] <- yuendata[["p.value"]]
table4[1,4] <- effect[["AKPeffect"]]
table4[1,5] <- exp(bf@bayesFactor[["bf"]])
table4[1,6] <- bf@bayesFactor[["error"]]

# ort_precision
df_precision_ort_pre <- groupedFinal %>% 
  select(group,precision_ort_pre) %>% convert_as_factor(group)

pre_summary <- rbind(pre_summary, df_precision_ort_pre %>% 
                       group_by(group) %>% 
                       get_summary_stats(precision_ort_pre, type = "mean_sd"))
pre_summary

# Check outlier(s)
df_precision_ort_pre %>% 
  group_by(group) %>% 
  identify_outliers(precision_ort_pre)
# Check Normality
df_precision_ort_pre %>% 
  group_by(group) %>% 
  shapiro_test(precision_ort_pre)
#Check homogeneity
df_precision_ort_pre %>% levene_test(precision_ort_pre ~ group)

yuendata <- yuen(precision_ort_pre~group, data = df_precision_ort_pre)
yuendata
effect <- akp.effect(precision_ort_pre~group, data = df_precision_ort_pre)
effect
bf <- ttestBF(formula = precision_ort_pre ~ group, data = df_precision_ort_pre)
bf
1/bf

table4[2,1] <- yuendata[["df"]]
table4[2,2] <- yuendata[["test"]]
table4[2,3] <- yuendata[["p.value"]]
table4[2,4] <- effect[["AKPeffect"]]
table4[2,5] <- exp(bf@bayesFactor[["bf"]])
table4[2,6] <- bf@bayesFactor[["error"]]

# srt_capacity
df_capacity_srt_pre <- groupedFinal %>% 
  select(group,capacity_srt_pre) %>% convert_as_factor(group)

pre_summary <- rbind(pre_summary,df_capacity_srt_pre %>% 
                       group_by(group) %>% 
                       get_summary_stats(capacity_srt_pre, type = "mean_sd"))
pre_summary

# Check outlier(s)
df_capacity_srt_pre %>% 
  group_by(group) %>% 
  identify_outliers(capacity_srt_pre)
# Check Normality
df_capacity_srt_pre %>% 
  group_by(group) %>% 
  shapiro_test(capacity_srt_pre)
# Check homogeneity 
df_capacity_srt_pre %>% levene_test(capacity_srt_pre ~ group)

yuendata <- yuen(capacity_srt_pre~group, data = df_capacity_srt_pre)
yuendata
effect <- akp.effect(capacity_srt_pre~group, data = df_capacity_srt_pre)
effect
bf <- ttestBF(formula = capacity_srt_pre ~ group, data = df_capacity_srt_pre)
bf
1/bf

table4[5,1] <- yuendata[["df"]]
table4[5,2] <- yuendata[["test"]]
table4[5,3] <- yuendata[["p.value"]]
table4[5,4] <- effect[["AKPeffect"]]
table4[5,5] <- exp(bf@bayesFactor[["bf"]])
table4[5,6] <- bf@bayesFactor[["error"]]

# srt_precision
df_precision_srt_pre <- groupedFinal %>% 
  select(group,precision_srt_pre) 

pre_summary <- rbind(pre_summary, df_precision_srt_pre %>% 
                       group_by(group) %>% 
                       get_summary_stats(precision_srt_pre, type = "mean_sd"))
pre_summary

# Check outlier(s)
df_precision_srt_pre %>% 
  group_by(group) %>% 
  identify_outliers(precision_srt_pre)
# Check Normality
df_precision_srt_pre %>% 
  group_by(group) %>% 
  shapiro_test(precision_srt_pre)
#Check homogeneity
df_precision_srt_pre %>% convert_as_factor(group)%>% levene_test(precision_srt_pre ~ group)

yuendata <- yuen(precision_srt_pre~group, data = df_precision_srt_pre)
yuendata
effect <- akp.effect(precision_srt_pre~group, data = df_precision_srt_pre)
effect
bf <- ttestBF(formula = precision_srt_pre ~ group, data = df_precision_srt_pre)
bf
1/bf

table4[6,1] <- yuendata[["df"]]
table4[6,2] <- yuendata[["test"]]
table4[6,3] <- yuendata[["p.value"]]
table4[6,4] <- effect[["AKPeffect"]]
table4[6,5] <- exp(bf@bayesFactor[["bf"]])
table4[6,6] <- bf@bayesFactor[["error"]]

# odt_capacity
df_kPre <- groupedFinal %>% 
  select(group,kPre) %>% convert_as_factor(group)

pre_summary <- rbind(pre_summary,df_kPre %>% 
                       group_by(group) %>% 
                       get_summary_stats(kPre, type = "mean_sd"))

# Check outlier(s)
df_kPre %>% 
  group_by(group) %>% 
  identify_outliers(kPre)
# Check Normality
df_kPre %>% 
  group_by(group) %>% 
  shapiro_test(kPre)
# Check homogeneity 
df_kPre %>% levene_test(kPre ~ group)

yuendata <- yuen(kPre~group, data = df_kPre)
yuendata
effect <- akp.effect(kPre~group, data = df_kPre)
effect
bf <- ttestBF(formula = kPre ~ group, data = df_kPre)
bf
1/bf
table4[7,1] <- yuendata[["df"]]
table4[7,2] <- yuendata[["test"]]
table4[7,3] <- yuendata[["p.value"]]
table4[7,4] <- effect[["AKPeffect"]]
table4[7,5] <- exp(bf@bayesFactor[["bf"]])
table4[7,6] <- bf@bayesFactor[["error"]]

# vs_acc
df_accPre <- groupedFinal %>% 
  select(group,accPre) %>% convert_as_factor(group)

pre_summary <- rbind(pre_summary,df_accPre %>% 
                       group_by(group) %>% 
                       get_summary_stats(accPre, type = "mean_sd"))
pre_summary

# Check outlier(s)
df_accPre %>% 
  group_by(group) %>% 
  identify_outliers(accPre)
# Check Normality
df_accPre %>% 
  group_by(group) %>% 
  shapiro_test(accPre)
# Check homogeneity 
df_accPre %>% levene_test(accPre ~ group)

yuendata <- yuen(accPre~group, data = df_accPre)
yuendata
effect <- akp.effect(accPre~group, data = df_accPre)
effect
bf <- ttestBF(formula = accPre ~ group, data = df_accPre)
bf
1/bf

table4[3,1] <- yuendata[["df"]]
table4[3,2] <- yuendata[["test"]]
table4[3,3] <- yuendata[["p.value"]]
table4[3,4] <- effect[["AKPeffect"]]
table4[3,5] <- exp(bf@bayesFactor[["bf"]])
table4[3,6] <- bf@bayesFactor[["error"]]

# vs_mrt
df_mrtPre <- groupedFinal %>% 
  select(group,mrtPre) %>% convert_as_factor(group)

pre_summary <- rbind(pre_summary,df_mrtPre %>% 
                       group_by(group) %>% 
                       get_summary_stats(mrtPre, type = "mean_sd"))


# Check outlier(s)
df_mrtPre %>% 
  group_by(group) %>% 
  identify_outliers(mrtPre)
# Check Normality
df_mrtPre %>% 
  group_by(group) %>% 
  shapiro_test(mrtPre)
# Check homogeneity 
df_mrtPre %>% levene_test(mrtPre ~ group)

yuendata <- yuen(mrtPre~group, data = df_mrtPre)
yuendata
effect <- akp.effect(mrtPre~group, data = df_mrtPre)
effect
bf <- ttestBF(formula = mrtPre ~ group, data = df_mrtPre)
bf
1/bf

table4[4,1] <- yuendata[["df"]]
table4[4,2] <- yuendata[["test"]]
table4[4,3] <- yuendata[["p.value"]]
table4[4,4] <- effect[["AKPeffect"]]
table4[4,5] <- exp(bf@bayesFactor[["bf"]])
table4[4,6] <- bf@bayesFactor[["error"]]
write.csv(table4,paste(analysedResults,'table4.csv',sep='/'))

# two-way mixed anova in prepost####

prePost <- groupedFinal %>% 
  select(group,accPre,mrtPre,kPre,accPost,mrtPost,kPost,capacity_ort_pre,capacity_srt_pre,precision_ort_pre,precision_srt_pre,capacity_ort_post,capacity_srt_post,precision_ort_post,precision_srt_post,Row.names) %>% 
  group_by(group)

capOrt <- prePost %>% select(capacity_ort_pre,capacity_ort_post,group,Row.names) %>% 
  gather(key = "session", value = "score",capacity_ort_post, capacity_ort_pre) %>% 
  convert_as_factor(session) %>%  
  mutate(time=fct_relevel(session, 'capacity_ort_pre','capacity_ort_post')) 
preOrt <- prePost %>% select(precision_ort_pre,precision_ort_post,group,Row.names) %>% 
  gather(key = "session", value = "score",precision_ort_post, precision_ort_pre) %>% 
  convert_as_factor(session) %>%  
  mutate(time=fct_relevel(session, 'precision_ort_pre','precision_ort_post'))

# draw Figure 4 ####
# get dfs ready
capORT_ext_pre <- capOrt %>% subset(group == "experiment" & time == "capacity_ort_pre")
capORT_ext_post <- capOrt %>% subset(group == "experiment" & time == "capacity_ort_post")
capORT_ctl_pre <- capOrt %>% subset(group == "control" & time == "capacity_ort_pre")
capORT_ctl_post <- capOrt %>% subset(group == "control" & time == "capacity_ort_post")

preORT_ext_pre <- preOrt %>% subset(group == "experiment" & time == "precision_ort_pre")
preORT_ext_post <- preOrt %>% subset(group == "experiment" & time == "precision_ort_post")
preORT_ctl_pre <- preOrt %>% subset(group == "control" & time == "precision_ort_pre")
preORT_ctl_post <- preOrt %>% subset(group == "control" & time == "precision_ort_post")

capORT_df_2x2 <- data_2x2(
  array_1 = capORT_ext_pre$score,
  array_2= capORT_ctl_pre$score,
  array_3 = capORT_ext_post$score,
  array_4 = capORT_ctl_post$score,
  labels = (c('experiment','control')),
  jit_distance = .09,
  jit_seed = 321,
  spread_x_ticks = FALSE) 
capORT_df <- capORT_df_2x2%>% 
  mutate(time = case_when(x_axis == "1"|x_axis == "1.01" ~ 'capacity_ort_pre',
                          x_axis == "2"|x_axis == "2.01" ~ 'capacity_ort_post'))

preORT_df_2x2 <- data_2x2(
  array_1 = preORT_ext_pre$score,
  array_2= preORT_ctl_pre$score,
  array_3 = preORT_ext_post$score,
  array_4 = preORT_ctl_post$score,
  labels = (c('experiment','control')),
  jit_distance = .09,
  jit_seed = 321,
  spread_x_ticks = FALSE) 
preORT_df <- preORT_df_2x2%>% 
  mutate(time = case_when(x_axis == "1"|x_axis == "1.01" ~ 'precision_ort_pre',
                          x_axis == "2"|x_axis == "2.01" ~ 'precision_ort_post'))
# get some defaults ready
colors = (c("#7D8B48","#C7A9AB"))
fills = (c("#7D8B48","#C7A9AB"))
shapes = (c(24,21))
line_color = (c('gray','black'))
line_alpha = .3
size = 1.5
alpha = .6
groupGap=0.35
gap2 <- .6

# get summary dfs ready
capSum <- capOrt %>% 
  mutate(x_axis = case_when(group == "experiment" & time == "capacity_ort_pre" ~ '1',
                            group == "experiment" & time == "capacity_ort_post" ~ '2',
                            group == "control" & time == "capacity_ort_pre"~ '1.01',
                            group == "control" & time == "capacity_ort_post" ~ '2.01')) %>% 
  group_by(x_axis,group,time) %>% 
  get_summary_stats(score)
preSum <- preOrt %>% 
  mutate(x_axis = case_when(group == "experiment" & time == "precision_ort_pre" ~ '1',
                            group == "experiment" & time == "precision_ort_post" ~ '2',
                            group == "control" & time == "precision_ort_pre"~ '1.01',
                            group == "control" & time == "precision_ort_post" ~ '2.01')) %>% 
  group_by(x_axis,group,time) %>% 
  get_summary_stats(score)

# cap 
capORT_df$group <- factor(capORT_df$group, levels=c("experiment", "control"))
capORT_df$time <- factor(capORT_df$time,levels = c("capacity_ort_pre","capacity_ort_post"))
capORT_df$x_axis <- factor(capORT_df$x_axis , levels = c('1','1.01','2','2.01'))
capSum$group <- factor(capSum$group, levels=c("experiment", "control"))
capSum$time <- factor(capSum$time,levels = c("capacity_ort_pre","capacity_ort_post"))
capSum$x_axis <- factor(capSum$x_axis , levels = c('1','1.01','2','2.01'))
str(capSum)
cap <- ggplot()+
  geom_point(data = capORT_df, aes(x = time, y = y_axis,
                                    color = group, fill=group,
                                    shape =group),
             size = size*0.8,
             alpha = 0.3, position = position_jitterdodge(jitter.width = 0.2,dodge.width = 0.3))+
  geom_errorbar(data = capSum, aes(x = time, ymin=mean-se, ymax=mean+se, group = group), linetype = "solid", width=.2,position = position_dodge(0.3))+
  geom_point(data = capSum, aes(x = time, y = mean, group=group,fill=group,shape =group,color=group),  size = 3.5,alpha = 1,position = position_dodge(0.3))+
  geom_flat_violin(
    data = capORT_df,aes(x = time, y = y_axis, color = group,fill=group), position = position_nudge(x = 0.3),alpha = alpha)+
  scale_color_manual(labels = c("Experimental", "Active Control"),values= colors)+
  scale_fill_manual(labels = c("Experimental", "Active Control"),values= fills)+
  scale_shape_manual(labels = c("Experimental", "Active Control"),values=shapes)+
  labs( x= "Time", y = expression(~bold("Capacity"~bolditalic(K))), col = 'Group',shape = 'Group',size =1)+
  scale_x_discrete(labels=c("capacity_ort_pre"= "Pre-Test",'capacity_ort_post'='Post-Test'))+
  guides(color=guide_legend(title.position="top", title.hjust = 0.5,title="Group"),fill=guide_legend(title.position="top", title.hjust = 0.5,title = "Group"), shape = guide_legend(title.position="top", title.hjust = 0.5,title = "Group"))+
  #scale_y_continuous(labels = function(x) format(x, nsmall = 2)) +
  theme(
    legend.position = "top",
    legend.background=element_blank(),
    legend.key = element_blank(),
    legend.title=element_text(face="bold"),
    legend.text =element_text(face="bold"),
    element_line(colour = 'black',size = 1),
    panel.background=element_blank(),
    axis.line.y.left = element_line(size = 1),
    axis.line.x.bottom  = element_line(size = 1),
    axis.text = element_text(face="bold"),
    axis.title = element_text(face="bold"),
    axis.title.x = element_text(vjust=2,hjust = 2),
    text = element_text(size = 12))
cap

# pre
preORT_df$group <- factor(preORT_df$group, levels=c("experiment", "control"))
preORT_df$time <- factor(preORT_df$time,levels = c("precision_ort_pre","precision_ort_post"))
preORT_df$x_axis <- factor(preORT_df$x_axis , levels = c('1','1.01','2','2.01'))
preSum$group <- factor(preSum$group, levels=c("experiment", "control"))
preSum$time <- factor(preSum$time,levels = c("precision_ort_pre","precision_ort_post"))
preSum$x_axis <- factor(preSum$x_axis , levels = c('1','1.01','2','2.01'))
str(preORT_df)
str(preSum)

pre <- ggplot()+
  geom_point(data = preORT_df, aes(x = time, y = y_axis,
                                    color = group, fill=group,
                                    shape =group),
             size = size*0.8,
             alpha = 0.3, position = position_jitterdodge(jitter.width = 0.2,dodge.width = 0.3))+
  geom_errorbar(data = preSum,aes(x = time, ymin=mean-se, ymax=mean+se, group = group), linetype = "solid", width=.2,position = position_dodge(0.3))+
  geom_point(data = preSum, aes(x = time, y = mean, group=group,fill=group,shape =group,color=group),  size = 3.5,alpha = 1,position = position_dodge(0.3))+
  geom_flat_violin(
    data = preORT_df,aes(x = time, y = y_axis, color = group,fill=group), position = position_nudge(x = 0.3),alpha = alpha)+
  scale_color_manual(labels = c("Experimental", "Active Control"),values= colors)+
  scale_fill_manual(labels = c("Experimental", "Active Control"),values= fills)+
  scale_shape_manual(labels = c("Experimental", "Active Control"),values=shapes)+
  labs(x= "Time", y = expression(~bold('Precision'~bolditalic('SD'^-1))), col= 'Group',shape = 'Group',size =1)+
  scale_x_discrete(labels=c("precision_ort_pre"= "Pre-Test",'precision_ort_post'='Post-Test'))+
  guides(color=guide_legend(title.position="top", title.hjust = 0.5,title="Group"),fill=guide_legend(title.position="top", title.hjust = 0.5,title = "Group"))+
  #scale_y_continuous(limits= c(0,0.125))+
  scale_y_continuous(labels = function(x) format(x, nsmall = 2)) +
  theme(
    legend.position = "top",
    legend.background=element_blank(),
    legend.key = element_blank(),
    legend.title=element_text(face="bold"),
    legend.text =element_text(face="bold"),
    element_line(colour = 'black',size = 1),
    panel.background=element_blank(),
    axis.line.y.left = element_line(size = 1),
    axis.line.x.bottom  = element_line(size = 1),
    axis.text = element_text(face="bold"),
    axis.title = element_text(face="bold"),
    axis.title.x = element_text(vjust=2,hjust = 2),
    text = element_text(size = 12))

pre

cappreORT <- ggarrange(cap,pre, 
                               labels = c("A", "B"),
                               ncol = 2, nrow = 1, 
                               common.legend = TRUE, legend="top")+
  theme(
    legend.background=element_blank(),
    legend.key = element_blank(),
    legend.title=element_text(face="bold"),
    legend.text =element_text(face="bold"), element_line(colour = 'black',size = 1),panel.background=element_blank(),axis.text = element_text(face="bold"),axis.title = element_text(face="bold"),text = element_text(size = 12))
ggsave(paste(analysedResults,"Figure4.png",sep = '/'),width = 9,height = 4.5)


# capacity in ort ####
table3row <- function(n,summaryO,type){
  typePre <- paste(type,'pre',sep = '_')
  typePost <- paste(type,'post',sep = '_')
  table3[n,] <- c(
    summaryO$mean[summaryO$group=='experiment'&summaryO$time==typePre],
    summaryO$sd[summaryO$group=='experiment'&summaryO$time==typePre],
    summaryO$mean[summaryO$group=='experiment'&summaryO$time==typePost],
    summaryO$sd[summaryO$group=='experiment'&summaryO$time==typePost],
    summaryO$mean[summaryO$group=='control'&summaryO$time==typePre],
    summaryO$sd[summaryO$group=='control'&summaryO$time==typePre],
    summaryO$mean[summaryO$group=='control'&summaryO$time==typePost],
    summaryO$sd[summaryO$group=='control'&summaryO$time==typePost]
  )
}
table3row0 <- function(n,summaryO,type){
  typePre <- paste0(type,'Pre')
  typePost <- paste0(type,'Post')
  table3[n,] <- c(
    summaryO$mean[summaryO$group=='experiment'&summaryO$time==typePre],
    summaryO$sd[summaryO$group=='experiment'&summaryO$time==typePre],
    summaryO$mean[summaryO$group=='experiment'&summaryO$time==typePost],
    summaryO$sd[summaryO$group=='experiment'&summaryO$time==typePost],
    summaryO$mean[summaryO$group=='control'&summaryO$time==typePre],
    summaryO$sd[summaryO$group=='control'&summaryO$time==typePre],
    summaryO$mean[summaryO$group=='control'&summaryO$time==typePost],
    summaryO$sd[summaryO$group=='control'&summaryO$time==typePost]
  )
}

table3<- data.frame(matrix(ncol = 8, nrow = 9))
rownames(table3) <- c('variable','stats',"capacity_ort", "precision_ort","acc_vs","mrt_vs", "capacity_srt","precision_srt","k_odt")
colnames(table3) <- c('Experimental','Experimental','Experimental','Experimental','Active_Control','Active_Control','Active_Control','Active_Control')
table3[1,] <- c('pre','pre','pre','pre','post','post','post','post')
table3[2,] <- c('M','SD','M','SD','M','SD','M','SD')

summary <- capOrt %>% get_summary_stats(score, type = "common")
summaryO <- capOrt %>% 
  group_by(group,time) %>% 
  get_summary_stats(score, type = "mean_sd")
summaryO
table3[3,] <- table3row(3,summaryO,'capacity_ort')


# outfilters
capOrt %>%
  group_by(time, group) %>%
  identify_outliers(score)
#Normality
capOrt%>%
  group_by(time, group) %>%
  shapiro_test(score)
#Homogneity of variance assumption.
capOrt %>%
  group_by(time) %>%
  levene_test(score ~ group)

# Two-way mixed ANOVA test (Table3 & Table 5)
aTlist <- function(df){
  anov <- ezANOVA(data = df, dv=.(score), wid = .(Row.names), between = .(group), within = .(time),type = 3, detailed = TRUE)
  aT<- aovEffectSize(anov)
  aT2 <- aovEffectSize(aT,effectSize = "ges")
  aT[["ANOVA"]][["ges"]] <- aT2[["ANOVA"]][["ges"]]
  print(aT)
  bf <- anovaBF(score~group * time + Row.names,
                data = df, whichRandom = 'Row.names')
  print(bf)
  bfInteraction = bf[4]/bf[3]
  bfInteraction
  print('interaction')
  print(bfInteraction)
  1/bfInteraction
  bfDF <- bf@bayesFactor[1:2,1:2]%>% mutate(bf=exp(bf))
  bfInterDF <- bfInteraction@bayesFactor[1,1:2]%>% mutate(bf=exp(bf))
  bfTemp <- rbind(bfDF,bfInterDF)
  rownames(bfTemp) <- c(gsub(' + *.*.','',rownames(bfTemp)[1:2]), paste0(gsub(' + *.*.','',rownames(bfTemp)[1]),':',gsub(' + *.*.','',rownames(bfTemp)[2])))
  aT[["bf"]] <- bfTemp
  return(aT)
}
aT <- NULL
capOrt$Row.names <- factor(capOrt$Row.names)
str(capOrt)
capOrt$group <- factor(capOrt$group)
aT_capOrt <- aTlist(capOrt)
aT_capOrt 
aT$aT_capOrt  <- aT_capOrt 


#precision in ort####
summaryORT2 <- preOrt %>% 
  group_by(time, group) %>%
  get_summary_stats(score, type = "mean_sd")
summaryORT2
table3[4,] <- table3row(4,summaryORT2,'precision_ort')


# outfilters
preOrt %>%
  group_by(time, group) %>%
  identify_outliers(score)
#Normality:If the data is normally distributed, the p-value should be greater than 0.05.Note that, if your sample size is greater than 50, the normal QQ plot is preferred because at larger sample sizes the Shapiro-Wilk test becomes very sensitive even to a minor deviation from normality.
preOrt%>%
  group_by(time, group) %>%
  shapiro_test(score)
#Homogneity of variance assumption. There was homogeneity of variances, as assessed by Levene’s test (p > 0.05).
preOrt %>%
  group_by(time) %>%
  levene_test(score ~ group)


# Two-way mixed ANOVA test
preOrt$Row.names <- factor(preOrt$Row.names) 
str(preOrt)
preOrt$group <- factor(preOrt$group)
aT_preOrt <- aTlist(preOrt)
aT_preOrt
aT$aT_preOrt <- aT_preOrt

#posthoc1_for_precision_ort----
# Effect of time in each group
# Effect of group at each time point
one.way <-preOrt %>% 
  group_by(group) %>% 
  anova_test(dv = score, wid = Row.names, within= time,effect.size = "ges") %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

one.way2 <- preOrt %>% 
  group_by(time) %>% 
  anova_test(dv = score, wid = Row.names, between = group, effect.size = "pes") %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way2

pre_posthoc <- c()
ext_pre <- preOrt %>% subset(group == "experiment" & time == "precision_ort_pre")
ext_post <- preOrt %>% subset(group == "experiment" & time == "precision_ort_post")
ctl_pre <- preOrt %>% subset(group == "control" & time == "precision_ort_pre")
ctl_post <- preOrt %>% subset(group == "control" & time == "precision_ort_post")
pre_posthoc$yuendenpent <- yuend(ext_pre$score,ext_post$score)
# > yuend(ext_pre$score,ext_post$score)

depeffects <- dep.effect(ext_pre$score,ext_post$score)
ttestBF(x = ext_pre$score,y=ext_post$score, paired=TRUE)



yuend(ctl_pre$score,ctl_post$score)
dep.effect(ctl_pre$score,ctl_post$score)
ttestBF(x = ctl_pre$score,y=ctl_post$score, paired=TRUE)
yuenatpost <- yuen(score~group, data = preOrt %>% subset(time == "precision_ort_post"))

yuen.effect.ci(score~group, data = preOrt %>% subset(time == "precision_ort_post"))
ttestBF(formula = score~group, data = preOrt %>% subset(time == "precision_ort_post"))
# Bayes factor analysis
# 
#[1] Alt., r=0.707 : 3192.426 ±0%
# 
# Against denominator:
#   Null, mu1-mu2 = 0 


#capacity in srt####
capSrt <- prePost %>% select(capacity_srt_pre,capacity_srt_post,group,Row.names) %>% 
  gather(key = "session", value = "score",capacity_srt_post, capacity_srt_pre) %>% 
  convert_as_factor(session,group) %>%  
  mutate(time=fct_relevel(session, 'capacity_srt_pre','capacity_srt_post')) %>% 
  group_by(time,group)
preSrt <- prePost %>% select(precision_srt_pre,precision_srt_post,group,Row.names) %>% 
  gather(key = "session", value = "score",precision_srt_post, precision_srt_pre) %>% 
  convert_as_factor(session,group) %>%  
  mutate(time=fct_relevel(session, 'precision_srt_pre','precision_srt_post')) %>% 
  group_by(time,group)

summary0 <- capSrt %>% get_summary_stats(score, type = "mean_sd")
summary0
table3[7,] <- table3row(7,summary0,'capacity_srt')

# outfilters
capSrt %>%
  group_by(time, group) %>%
  identify_outliers(score)

#Normality
capSrt%>%
  group_by(time, group) %>%
  shapiro_test(score)
#Homogneity of variance assumption. 
capSrt %>%
  group_by(time) %>%
  levene_test(score ~ group)

# # Two-way mixed ANOVA test
capSrt$Row.names <- factor(capSrt$Row.names) 
str(capSrt)
capSrt$group <- factor(capSrt$group)
aT_capSrt <- aTlist(capSrt)
aT_capSrt
aT$aT_capSrt <- aT_capSrt

#precision in srt####
summarySRT2 <- preSrt %>% get_summary_stats(score, type = "mean_sd")
summarySRT2
table3[8,] <- table3row(8,summarySRT2,'precision_srt')

# outfilters
preSrt %>%
  group_by(time, group) %>%
  identify_outliers(score)

#Normality
preSrt%>%
  group_by(time, group) %>%
  shapiro_test(score)
#Homogneity of variance assumption. 
preSrt %>%
  group_by(time) %>%
  levene_test(score ~ group)


preSrt$Row.names <- factor(preSrt$Row.names) 
str(preSrt)
preSrt$group <- factor(preSrt$group)
aT_preSrt <- aTlist(preSrt)
aT_preSrt
aT$aT_preSrt <- aT_preSrt

# accurracy in VS####
accVS <- prePost %>% select(accPre,accPost,group,Row.names) %>% 
  gather(key = "session", value = "score",accPost, accPre) %>% 
  convert_as_factor(session,group) %>%  
  mutate(time=fct_relevel(session, 'accPre','accPost')) %>% 
  group_by(time,group)
summary <- accVS %>% get_summary_stats(score, type = "mean_sd")
summary
table3[5,] <- table3row0(5,summary,'acc')
# outfilters
accVS %>%
  group_by(time, group) %>%
  identify_outliers(score)

#Normality
accVS%>%
  group_by(time, group) %>%
  shapiro_test(score)
#Homogneity of variance assumption. 
accVS %>%
  group_by(time) %>%
  levene_test(score ~ group)


accVS$Row.names <- factor(accVS$Row.names) 
str(accVS)
accVS$group <- factor(accVS$group)
aT_accVS<- aTlist(accVS)
aT_accVS
aT$aT_accVS <- aT_accVS

# Reaction time  in VS####
mrtVS <- prePost %>% select(mrtPre,mrtPost,group,Row.names) %>% 
  gather(key = "session", value = "score",mrtPost, mrtPre) %>% 
  convert_as_factor(session, group) %>%  
  mutate(time=fct_relevel(session, 'mrtPre','mrtPost')) %>% 
  group_by(time,group)
sum <- mrtVS %>% get_summary_stats(score, type = "mean_sd")
sum
table3[6,] <- table3row0(6,sum,'mrt')

# outfilters
mrtVS %>%
  group_by(time, group) %>%
  identify_outliers(score)

#Normality
mrtVS%>%
  group_by(time, group) %>%
  shapiro_test(score)
#Homogneity of variance assumption. 
mrtVS %>%
  group_by(time) %>%
  levene_test(score ~ group)


mrtVS$Row.names <- factor(mrtVS$Row.names) 
str(mrtVS)
mrtVS$group <- factor(mrtVS$group)
aT_mrtVS<- aTlist(mrtVS)
aT_mrtVS
aT$aT_mrtVS <- aT_mrtVS


# posthoc2_for_mrt_vs----
one.way <- mrtVS %>% 
  group_by(time) %>% 
  anova_test(dv = score, wid = Row.names, between = group,effect.size = "ges") %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way


mrtVS <- select(mrtVS,-session)
one.way21 <-mrtVS %>%
  group_by(group) %>% 
  anova_test(dv = score, wid = Row.names, within= time, effect.size = "ges") %>% 
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way21
      

# capacity  in ODT####
kODT <- prePost %>% select(kPre,kPost,group,Row.names) %>% 
  gather(key = "session", value = "score",kPost, kPre) %>% 
  convert_as_factor(session,group) %>%  
  mutate(time=fct_relevel(session, 'kPre','kPost')) %>% 
  group_by(time,group)
sum <- kODT %>% get_summary_stats(score, type = "mean_sd")
sum
table3[9,] <- table3row0(9,sum,'k')
write.csv(table3,paste(analysedResults,'table3.csv',sep='/'))

# outfilters
kODT %>%
  group_by(time, group) %>%
  identify_outliers(score)

#Normality
kODT%>%
  group_by(time, group) %>%
  shapiro_test(score)
#Homogneity of variance assumption. 
kODT %>%
  group_by(time) %>%
  levene_test(score ~ group)


kODT$Row.names <- factor(kODT$Row.names) 
str(kODT)
kODT$group <- factor(kODT$group)
aT_kODT<- aTlist(kODT)
aT_kODT
aT$aT_kODT <- aT_kODT

convert2Table5 <- function(df) {
  table5 <- c()
  varObjects <- names(df)
  for (a in 1:length(varObjects)){
   thisAnova <- df[[varObjects[a]]][["ANOVA"]] %>% select(Effect,F,p,ges,pes)
   if (nrow(thisAnova)>3){thisAnova <- thisAnova[-1,]}# for prepost when having intercept only
   rownames(thisAnova) <-  thisAnova[,1]
   thisAnova <- thisAnova[,-1]
   thisBF <- df[[varObjects[a]]][["bf"]] 
   mergeStat <- merge(thisAnova,thisBF,by= 0,sort = F)
   rownames(mergeStat) <-  paste(gsub(".*_", "", varObjects[a]),mergeStat$Row.names,sep='_')
   table5 <- rbind(table5,mergeStat[,-1])
  }
  return(table5)
}

Table5 <- convert2Table5(aT)
write.csv(Table5,paste(analysedResults,'table5.csv',sep='/'))
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

table2rowsss <- function(summary,ssType){
  times= c('t1','t2','t3','t4')
  if (ssType ==1){
    ss=c('2','4','6')
  }else if(ssType==2){
    ss=c('8','16','24')
  }else{
      print('put the correct set size type!')
  }
 table2 <-  matrix(nrow=length(ss),ncol=2*length(times))
 for (s in 1:length(ss)){
   for (t in 1:length(times)){
     table2[s,(t*2-1)] =  summary$mean[ summary$setsize==ss[s] & summary$time==times[t] ]
     table2[s,2*t] =  summary$sd[ summary$setsize==ss[s] & summary$time==times[t] ]
   }
 }
 return(table2)
}

table2<- data.frame(matrix(ncol = 8, nrow = 13))
rownames(table2) <- c('stats',
                      "capacity_ort_ss2", "capacity_ort_ss4","capacity_ort_ss6","precision_ort_ss2", "precision__ort_ss4","precision_ort_ss6",
                      "acc_vs_ss8", "acc_vs_ss16","acc_vs_ss24","mrt_vs_ss8", "mrt_vs_ss16","mrt_vs_ss24")
colnames(table2) <- c('S1','S1','S2','S2','S3','S3','S4','S4')
table2[1,] <- c('M','SD','M','SD','M','SD','M','SD')

# capacity
duringORTSummary <- capT3 %>% 
  group_by(setsize,time) %>% 
  get_summary_stats(score,type='mean_sd')
duringORTSummary
table2[c(2:4),] <- table2rowsss(duringORTSummary,1)

#outliers
capT3 %>% 
  group_by(setsize,time) %>% 
  identify_outliers(score)
# normality
capT3 %>% 
  group_by(setsize,time) %>% 
  shapiro_test(score)

aTT <- NULL
aTTlist <- function(df){
  anov <- ezANOVA(data = df, dv=.(score), wid = .(Row.names), within = .(time, setsize),type = 1, detailed = TRUE)
  aTT<- aovEffectSize(anov)
  aTT2 <- aovEffectSize( aTT,effectSize = "ges")
  aTT[["ANOVA"]][["ges"]] <- aTT2[["ANOVA"]][["ges"]]
  print(aTT)
  bf <- anovaBF(score~setsize * time + Row.names,
                data = df, whichRandom = 'Row.names')
  print(bf)
  bfInteraction = bf[4]/bf[3]
  print(bfInteraction)
  bfInteraction
  1/bfInteraction
  bfDF <- bf@bayesFactor[1:2,1:2]%>% mutate(bf=exp(bf))
  bfInterDF <- bfInteraction@bayesFactor[1,1:2]%>% mutate(bf=exp(bf))
  bfTemp <- rbind(bfDF,bfInterDF)
  rownames(bfTemp) <- c(gsub(' + *.*.','',rownames(bfTemp)[1:2]), paste0(gsub(' + *.*.','',rownames(bfTemp)[1]),':',gsub(' + *.*.','',rownames(bfTemp)[2])))
  aTT[["bf"]] <- bfTemp
  return(aTT)
}
capT3 <- select(capT3,-type,score)
capT3$Row.names <- factor(capT3$Row.names)
capT3$time <- factor(capT3$time)
capT3$setsize <- factor(capT3$setsize)
str(capT3)
aTT_capT3 <- aTTlist(capT3)
aTT_capT3
aTT$aTT_capT3 <- aTT_capT3

# comparisons for setsize variable
capT3 %>% 
  pairwise_t_test(
    score ~ setsize, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )
capT3 %>%
  cohens_d(score ~ setsize, paired = TRUE)

ttestBF(x = capT3$score[capT3$setsize==2], y = capT3$score[capT3$setsize==4], paired=TRUE)
ttestBF(x = capT3$score[capT3$setsize==2], y = capT3$score[capT3$setsize==6], paired=TRUE)
ttestBF(x = capT3$score[capT3$setsize==4], y = capT3$score[capT3$setsize==6], paired=TRUE)


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
duringORTSummary2
table2[c(5:7),] <- table2rowsss(duringORTSummary2,1)

#outliers
preT3 %>% 
  group_by(setsize,time) %>% 
  identify_outliers(score)
# normality
preT3 %>% 
  group_by(setsize,time) %>% 
  shapiro_test(score)

preT3$Row.names <- factor(preT3$Row.names)
preT3$time <- factor(preT3$time)
preT3$setsize <- factor(preT3$setsize)
str(preT3)
aTT_preT3 <- aTTlist(preT3)
aTT_preT3
aTT$aTT_preT3  <- aTT_preT3 

# comparisons for setsize variable
preT3 %>% 
  pairwise_t_test(
    score ~ setsize, paired = TRUE,
    p.adjust.method = "bonferroni"
  )

preT3 %>%
  cohens_d(score ~ setsize, paired = TRUE)

ttestBF(x = preT3$score[preT3$setsize==2], y = preT3$score[preT3$setsize==4], paired=TRUE)
ttestBF(x = preT3$score[preT3$setsize==2], y = preT3$score[preT3$setsize==6], paired=TRUE)
ttestBF(x = preT3$score[preT3$setsize==4], y = preT3$score[preT3$setsize==6], paired=TRUE)


# Figure 3####
summary <-capT3 %>% 
  group_by(time,setsize) %>% 
  get_summary_stats(score) 
p1 <- ggplot() +
  geom_point(data = capT3,aes(x=time,y=score,color = setsize,shape = setsize),position = position_jitterdodge(0.2),alpha = .2)+
  geom_errorbar(data = summary,aes(x = time, ymin=mean-se, ymax=mean+se,group=setsize), linetype = "solid", width=.2, position = position_dodge(0.8))+
  geom_point(data = summary,aes(x=time,y=mean, colour = setsize, shape = setsize),size = 3,position = position_dodge(0.8))+
  geom_line(data = summary,aes(x=time,y=mean, group = setsize, colour = setsize,linetype = setsize),position = position_dodge(0.8))+
  scale_linetype_manual(values=c("dotted", "solid","dashed"))+
  labs( x= "Time (Session)", y = expression(~bold("Mean Capacity"~bolditalic(K))),size =1)+
  scale_x_discrete(labels=c("pre"= "Pre-Test","t1"= "S1", "t2" = "S2" ,"t3" = "S3","t4" = "S4",'post'='Post-Test'))+
  scale_y_continuous(labels = function(x) format(x, nsmall = 1))+
  guides(color=guide_legend(title.position="top", title.hjust = 0.5,title="Set Size"),shape=guide_legend(title.position="top",title.hjust = 0.5,title="Set Size"),linetype=guide_legend(title.position="top", title.hjust = 0.5,title="Set Size"))+
  theme(
    legend.justification = "center",
    legend.position = "top",
    legend.background=element_blank(),
    legend.key = element_blank(),
    legend.title=element_text(face="bold"),
    legend.text =element_text(face="bold"),
    element_line(colour = 'black',size = 1),
    panel.background=element_blank(),
    axis.line.y.left = element_line(size = 1),
    axis.line.x.bottom  = element_line(size = 1),
    axis.text = element_text(face="bold"),
    axis.title = element_text(face="bold"),
    text = element_text(size = 12))

# during precision
summary <-preT3 %>% 
  group_by(time,setsize) %>% 
  get_summary_stats(score) 
p2 <- ggplot() +
  geom_point(data = preT3,aes(x=time,y=score,color = setsize,shape = setsize),position = position_jitterdodge(0.2),alpha = .2)+
  geom_errorbar(data = summary,aes(x = time, ymin=mean-se, ymax=mean+se,group=setsize), linetype = "solid", width=.2, position = position_dodge(0.8))+
  geom_point(data = summary,aes(x=time,y=mean, colour = setsize, shape = setsize),size = 3,position = position_dodge(0.8))+
  geom_line(data = summary,aes(x=time,y=mean, group = setsize, colour = setsize,linetype = setsize),position = position_dodge(0.8))+
  scale_linetype_manual(name = 'Set Size',values=c("dotted", "solid","dashed"))+
  labs( x= "Time (Session)", y = expression(~bold("Mean Precision"~bolditalic(SD^-1))),size =1)+
  scale_x_discrete(labels=c("pre"= "Pre-Test","t1"= "S1", "t2" = "S2" ,"t3" = "S3","t4" = "S4",'post'='Post-Test'))+
  scale_y_continuous(limits = c(0,0.16))+
  guides(color=guide_legend(title.position="top", title.hjust = 0.5,title="Set Size"),shape=guide_legend(title.position="top",title.hjust = 0.5,title="Set Size"),linetype=guide_legend(title.position="top", title.hjust = 0.5,title="Set Size"))+
  theme(
    legend.position = "top",
    legend.background=element_blank(),
    legend.key = element_blank(),
    legend.title=element_text(face="bold"),
    legend.text =element_text(face="bold"),
    element_line(colour = 'black',size = 1),
    panel.background=element_blank(),
    axis.line.y.left = element_line(size = 1),
    axis.line.x.bottom  = element_line(size = 1),
    axis.text = element_text(face="bold"),
    axis.title = element_text(face="bold"),
    text = element_text(size = 12))
#Removed 1 rows containing missing values (`geom_point()`). 

duringORTplot <- ggarrange(p1,p2, 
          labels = c("A", "B"),
          ncol = 2, nrow = 1, 
          common.legend = TRUE, legend="top")+
  theme(
    legend.background=element_blank(),
    legend.key = element_blank(),
    legend.title=element_blank(),  element_line(colour = 'black',size = 1),panel.background=element_blank(),axis.text = element_text(face="bold"),axis.title = element_text(face="bold"),text = element_text(size = 12))
ggsave(paste(analysedResults,"Figure3.png",sep = '/'),width = 9,height = 4.5)
# in the active control ####
# ACC
accT <- groupedFinal %>% 
  filter(group == 'control') %>% 
  select(Row.names, t1_ss8.vsAcc,t2_ss8.vsAcc,t3_ss8.vsAcc,t4_ss8.vsAcc,t1_ss16.vsAcc,t2_ss16.vsAcc,t3_ss16.vsAcc,t4_ss16.vsAcc,t1_ss24.vsAcc,t2_ss24.vsAcc,t3_ss24.vsAcc,t4_ss24.vsAcc)

accT2 <- accT %>% 
  gather(key = "type", value = "score",  t1_ss8.vsAcc,t2_ss8.vsAcc,t3_ss8.vsAcc,t4_ss8.vsAcc,t1_ss16.vsAcc,t2_ss16.vsAcc,t3_ss16.vsAcc,t4_ss16.vsAcc,t1_ss24.vsAcc,t2_ss24.vsAcc,t3_ss24.vsAcc,t4_ss24.vsAcc)

accT3 <- accT2 %>% 
  mutate(time = substr(type, 1, 2)) %>% 
  mutate(setsize = substr(type, 6, 7)) 
accT3$setsize[accT3$setsize=='8.']=8

VSduringSummary <- accT3 %>% 
  group_by(setsize,time) %>% 
  get_summary_stats(score,type='mean_sd')
VSduringSummary
table2[c(8:10),] <- table2rowsss(VSduringSummary,2)

#outliers
accT3 %>% 
  group_by(setsize,time) %>% 
  identify_outliers(score)

# normality
accT3 %>% 
  group_by(setsize,time) %>% 
  shapiro_test(score)

accT3 <- select(accT3,-type,score)
accT3$Row.names <- factor(accT3$Row.names)
accT3$time <- factor(accT3$time)
accT3$setsize <- factor(accT3$setsize)
str(accT3)
aTT_accT3<- aTTlist(accT3)
aTT_accT3
aTT$aTT_accT3  <- aTT_accT3 

# comparisons for setsize variable
accT3 %>% 
  pairwise_t_test(
    score ~ setsize, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )
accT3 %>%
  cohens_d(score ~ setsize, paired = TRUE)

ttestBF(x = accT3$score[accT3$setsize==8], y = accT3$score[accT3$setsize==16], paired=TRUE)
ttestBF(x = accT3$score[accT3$setsize==8], y = accT3$score[accT3$setsize==24], paired=TRUE)
ttestBF(x = accT3$score[accT3$setsize==16], y = accT3$score[accT3$setsize==24], paired=TRUE)

# comparisons for time variable
accT3 %>% 
  pairwise_t_test(
    score ~ time, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )
accT3 %>%
  cohens_d(score ~ time, paired = TRUE)

ttestBF(x = accT3$score[accT3$time=='t1'], y = accT3$score[accT3$time=='t2'], paired=TRUE)
ttestBF(x = accT3$score[accT3$time=='t1'], y = accT3$score[accT3$time=='t3'], paired=TRUE)
ttestBF(x = accT3$score[accT3$time=='t1'], y = accT3$score[accT3$time=='t4'], paired=TRUE)
ttestBF(x = accT3$score[accT3$time=='t2'], y = accT3$score[accT3$time=='t3'], paired=TRUE)
ttestBF(x = accT3$score[accT3$time=='t2'], y = accT3$score[accT3$time=='t4'], paired=TRUE)
ttestBF(x = accT3$score[accT3$time=='t3'], y = accT3$score[accT3$time=='t4'], paired=TRUE)

# mrt----
mrtT <- groupedFinal %>% 
  filter(group == 'control') %>% 
  select(Row.names, t1_ss8.vsMrt,t2_ss8.vsMrt,t3_ss8.vsMrt,t4_ss8.vsMrt,t1_ss16.vsMrt,t2_ss16.vsMrt,t3_ss16.vsMrt,t4_ss16.vsMrt,t1_ss24.vsMrt,t2_ss24.vsMrt,t3_ss24.vsMrt,t4_ss24.vsMrt)

mrtT2 <- mrtT %>% 
  gather(key = "type", value = "score", t1_ss8.vsMrt,t2_ss8.vsMrt,t3_ss8.vsMrt,t4_ss8.vsMrt,t1_ss16.vsMrt,t2_ss16.vsMrt,t3_ss16.vsMrt,t4_ss16.vsMrt,t1_ss24.vsMrt,t2_ss24.vsMrt,t3_ss24.vsMrt,t4_ss24.vsMrt)

mrtT3 <- mrtT2 %>% 
  mutate(time = substr(type, 1, 2)) %>% 
  mutate(setsize = substr(type, 6, 7)) 
mrtT3$setsize[mrtT3$setsize=='8.']=8

VSduringSummary2 <- mrtT3 %>% 
  group_by(setsize,time) %>% 
  get_summary_stats(score,type='mean_sd')
VSduringSummary2
table2[c(11:13),] <- table2rowsss(VSduringSummary2,2)

write.csv(table2,paste(analysedResults,'table2.csv',sep = '/'))
#outliers
mrtT3 %>% 
  group_by(setsize,time) %>% 
  identify_outliers(score)

# normality
mrtT3 %>% 
  group_by(setsize,time) %>% 
  shapiro_test(score)

mrtT3 <- select(mrtT3,-type,score)
mrtT3$Row.names <- factor(mrtT3$Row.names)
mrtT3$time <- factor(mrtT3$time)
mrtT3$setsize <- factor(mrtT3$setsize)
str(mrtT3)
aTT_mrtT3<- aTTlist(mrtT3)
aTT_mrtT3
aTT$aTT_mrtT3  <- aTT_mrtT3 

Table6 <- convert2Table5(aTT)
write.csv(Table6,paste(analysedResults,'table6.csv',sep='/'))

mrtT3 %>% 
  pairwise_t_test(
    score ~ setsize, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )
mrtT3 %>%
  cohens_d(score ~ setsize, paired = TRUE)


ttestBF(x = mrtT3$score[mrtT3$setsize==8], y = mrtT3$score[mrtT3$setsize==16], paired=TRUE)
ttestBF(x = mrtT3$score[mrtT3$setsize==8], y = mrtT3$score[mrtT3$setsize==24], paired=TRUE)
ttestBF(x = mrtT3$score[mrtT3$setsize==16], y = mrtT3$score[mrtT3$setsize==24], paired=TRUE)

# Exploratory Analysis####
# response density
PRE <- read.csv(paste(shareableRawFolder,"PRE.csv",sep = '/'))
POST <- read.csv(paste(shareableRawFolder,"POST.csv",sep = '/'))
post <- POST[grep('^tatoolContinuousOrientation_\\d',POST$executableId),] 
pre<- PRE[grep('^tatoolContinuousOrientation_\\d',PRE$executableId),] 
strictToken<- read.csv('strictToken.csv')
keep <- strictToken$V1
pre <- pre[pre$sessionToken %in% keep, ]
post <- post[post$sessionToken %in% keep, ] 

postORT <- post %>% 
  cbind(check1 = grepl('^vwmt',post$extId)) %>% 
  cbind(check2 = nchar(post$extId)) %>% 
  subset(check1 == 'TRUE' & check2 == 7) %>% 
  subset(trialId >= 1 & trialId <= 560)
preORT <- pre %>% 
  cbind(check1 = grepl('^vwmt',pre$extId)) %>% 
  cbind(check2 = nchar(pre$extId)) %>% 
  subset(check1 == 'TRUE' & check2 == 7) %>% 
  subset(trialId >= 1 & trialId <= 560) 

validTokenPrePost <- function(df, requireTrial){
  exclude <- NULL
  tokens<- unique(df$sessionToken) 
  for(i in 1:length(tokens)){
    thistoken <- tokens[i]
    temp <- df %>% subset(sessionToken == thistoken)
    if (nrow(temp) < requireTrial){
      exclude <- cbind(exclude,thistoken)
    }}
  df1 <- df %>% subset(!(sessionToken %in% exclude))
  
  Ids<- unique(df1$extId)
  exclude2 <- NULL
  for(j in 1:length(Ids)){
    thisId <- Ids[j]
    temp2 <- df1 %>% subset(extId==thisId)
    moreThanOneOrNOT <- unique(temp2$sessionToken)
    if  (length(moreThanOneOrNOT)>1){
      excludeNotFirstSession <-  unique(temp2$sessionId)[-1]
      exclude2 <- rbind(exclude2 ,unique(temp2$sessionToken[ temp2$sessionId%in%excludeNotFirstSession]))
    }
  }
  df2 <- df1 %>% subset(!(sessionToken%in%exclude2))
  return(df2)
}
requireTrial <- 120
preORT <- validTokenPrePost(preORT,requireTrial)
postORT <- validTokenPrePost(postORT,requireTrial)
# excludedList<- read.csv(paste(workspace,'more_registered_Exclusion.csv',sep = '/'))
# preORTexclude <- cbind(preORT %>% subset(!(preORT$extId %in% excludedList$x)),time = 'pre')
# postORTexclude <- cbind(postORT %>% subset(!(postORT$extId %in% excludedList$x)),time = 'post')
# df <- rbind(preORTexclude,postORTexclude)

preORT <- cbind(preORT,time = 'pre')
postORT <- cbind(postORT,time = 'post')
df <- rbind(preORT,postORT)
#
df $givenResponse <- as.numeric(df $givenResponse )
df2 <- df %>% subset(group == 'experiment')
df2$time <- factor(df2$time, levels=c("pre", "post"))
p1 <- ggplot(df2, aes(x=givenResponse, linetype = time, fill = time, color =time)) +
  geom_histogram(aes(y=100*..density..,
                     linetype=time,fill=time),alpha = 0.2, bins = 60,color = '#F7F5F2',
                 #position = 'identity'
                 position = position_stack(reverse = TRUE)
  )+
  scale_color_manual(labels = c("Pre-Test", "Post-Test"),values=c( "#3C3E8E","#CE0064"))+
  scale_fill_manual(labels = c("Pre-Test", "Post-Test"),values=c(  "#3C3E8E","#CE0064"))+
  scale_linetype_manual(labels = c("Pre-Test", "Post-Test"), values = c(2,1))+
  geom_density(aes(y=100*..density..,color = time),alpha=0,fill = "white", size =1.5)+
  scale_x_continuous(breaks=seq(0,360,45), limits=c(1,360))+
  labs(x="Responses (degree)", y = "Density (%)")+
  ggtitle("Experimental Group")+
  guides(color=guide_legend(title.position="top", title.hjust = 0.5,title="Time"),linetype=guide_legend(title.position="top", title.hjust = 0.5,title="Time"),fill=guide_legend(title.position="top", title.hjust = 0.5,title = "Time"))+
  theme(
    legend.position = "top",
    legend.background=element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(0.7, "cm"),
    legend.title=element_text(face="bold"),
    legend.text =element_text(face="bold"),
    element_line(colour = 'black',size = 1),
    panel.background=element_blank(),
    axis.line.y.left = element_line(size = 1),
    axis.line.x.bottom  = element_line(size = 1),
    axis.text = element_text(face="bold"),
    axis.title = element_text(face="bold"),
    plot.title = element_text(hjust = 0.5,face="bold",size = 12),
    text = element_text(size = 12))

df3 <- df %>% subset(group == 'control') 
df3$time <- factor(df3$time, levels=c("pre", "post"))
p2 <- ggplot(df3, aes(x=givenResponse, linetype = time, fill = time, color =time)) +
  geom_histogram(aes(y=100*..density..),alpha = 0.2, bins = 60,color = '#F7F5F2', 
                 #position = 'identity'
                 position = position_stack(reverse = TRUE)
  )+
  scale_color_manual(labels = c("Pre-Test", "Post-Test"),values=c( "#3C3E8E","#CE0064"))+
  scale_fill_manual(labels = c("Pre-Test", "Post-Test"),values=c(  "#3C3E8E","#CE0064"))+
  scale_linetype_manual(labels = c("Pre-Test", "Post-Test"), values = c(2,1))+
  geom_density(aes(y=100*..density..,color = time),alpha=0,fill = "white", size =1.5)+
  scale_x_continuous(breaks=seq(0,360,45), limits=c(1,360))+
  labs(x="Responses (degree)", y = "Density (%)")+
  ggtitle("Active Control Group")+
  guides(color=guide_legend(title.position="top", title.hjust = 0.5,title="Time"),linetype=guide_legend(title.position="top", title.hjust = 0.5,title="Time"),fill=guide_legend(title.position="top", title.hjust = 0.5,title = "Time"))+
  theme(
    legend.position = "top",
    legend.background=element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(0.7, "cm"),
    legend.title=element_text(face="bold"),
    legend.text =element_text(face="bold"),
    element_line(colour = 'black',size = 1),
    panel.background=element_blank(),
    axis.line.y.left = element_line(size = 1),
    axis.line.x.bottom  = element_line(size = 1),
    axis.text = element_text(face="bold"),
    axis.title = element_text(face="bold"),
    plot.title = element_text(hjust = 0.5,face="bold",size = 12),
    text = element_text(size = 12))

responses_density <- ggarrange(p1,p2, 
                               #labels = c("A", "B"),
                               ncol = 2, nrow = 1, 
                               common.legend = TRUE, legend="top")+
  theme(
    legend.background=element_blank(),
    legend.key = element_blank(),
    legend.title=element_blank(),  element_line(colour = 'black',size = 1),panel.background=element_blank(),axis.text = element_text(face="bold"),axis.title = element_text(face="bold"),text = element_text(size = 12))
ggsave(paste(analysedResults,"Figure5.png",sep = '/'),width = 9,height = 4.5)



cardinalBin <- function(df){
  dfbycardinal <- c()
  carninalList <- c('(338,22.5]','(22.5,67.5]','(67.5,112]','(112,158]','(158,202]','(202,248]','(248,292]','(292,338]')
  dfbyhalf <- as.data.frame(table(cut(df$givenResponse,breaks=seq(-22.5,360,by=22.5),include.lowest = FALSE, right = TRUE)))
  n = nrow(dfbyhalf)
  startR= 2
  stopR = n-1
  end = (stopR-startR)/2 
  dfbyhalf <- dfbyhalf[-1,]
  dfbycardinal<- cbind(cat = carninalList[1],count = dfbyhalf[n-1,2]+dfbyhalf[startR-1,2])
  for (i in 1: end){
    newrow= cbind(carninalList[startR+1*(i-1)], dfbyhalf[2*i,2]+dfbyhalf[2*i+1,2])
    dfbycardinal <- rbind(dfbycardinal,newrow)
  }
  dfbycardinal <- as.data.frame(dfbycardinal)
  dfbycardinal$count <- as.numeric(dfbycardinal$count )
  return(dfbycardinal)
}
orient_ext_pre <- df2 %>% subset(df2$time == 'pre')
orient_ext_post <- df2 %>% subset(df2$time == 'post')
extPre <- cardinalBin(orient_ext_pre)
extPost <- cardinalBin(orient_ext_post)

orient_ac_pre <- df3 %>% subset(df3$time == 'pre')
orient_ac_post <- df3 %>% subset(df3$time == 'post')
acPre <- cardinalBin(orient_ac_pre)
acPost <- cardinalBin(orient_ac_post)

contingencyPre <- merge(x= extPre, y = acPre, by = 'cat',all.x = TRUE, all.y = TRUE)
colnames(contingencyPre) <- c("cat", 'Experimental','Active Control')
contingencyPre <- contingencyPre %>% remove_rownames %>% column_to_rownames(var="cat")
contingencyPreTB<- as.matrix(contingencyPre)
chisq <- chisq.test(contingencyPre)
chisq
bf <- contingencyTableBF(contingencyPreTB, sampleType = 'poisson')

contingencyPost <- merge(x= extPost, y = acPost, by = 'cat',all.x = TRUE, all.y = TRUE)
colnames(contingencyPost) <- c("cat", 'Experimental','Active Control')
contingencyPost <- contingencyPost %>% remove_rownames %>% column_to_rownames(var="cat")
contingencyPostTB<- as.matrix(contingencyPost)
chisq2 <- chisq.test(contingencyPost)
chisq2
contingencyTableBF(contingencyPostTB,sampleType = 'poisson')



