# futher after groupedFinal 

# one way repeated measures at ss 2, 4,6 for the experimenter group only

# at set size 2, changes during four training 
ss2_cap <- groupedFinal %>% 
  filter(group == 'experiment') %>% 
  select(Row.names, capacity_ortT_t1_ss2,capacity_ortT_t2_ss2,capacity_ortT_t3_ss2,capacity_ortT_t4_ss2)
df <- ss2_cap %>% 
  gather(key = "type", value = "score",  capacity_ortT_t1_ss2,capacity_ortT_t2_ss2,capacity_ortT_t3_ss2,capacity_ortT_t4_ss2) %>% 
  convert_as_factor(Row.names, type)
str(df)

dfsummary <- df %>%
  group_by(type) %>%
  get_summary_stats(score, type = "mean_sd")

df %>%
  group_by(type) %>%
  identify_outliers(score)
# there are extrem outlier 

df %>%
  group_by(type) %>%
  shapiro_test(score)
# not normally distributed 

res.aov <- anova_test(data = df, dv = score, wid = Row.names, within = type)
get_anova_table(res.aov)

# ANOVA Table (type III tests)
# 
# Effect  DFn   DFd    F     p p<.05     ges
# 1   type 2.24 82.79 0.14 0.891       0.00098

# res.fried <- df %>% friedman_test(score ~ type |Row.names)
# res.fried

# p_s2 <- ggplot(dfsummary, aes(x=type, y= mean)) + 
#   geom_point()+
#     geom_line(aes(group = 1)) +
#     geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
#                 position=position_dodge(0.05))


# at set size 4, changes from pre->four training ->post 
s4_cap <- groupedFinal %>% 
  filter(group == 'experiment') %>% 
  select(Row.names, capacity_ort_pre,capacity_ortT_t1_ss4,capacity_ortT_t2_ss4,capacity_ortT_t3_ss4,capacity_ortT_t4_ss4,capacity_ort_post)
df <- s4_cap %>% 
  gather(key = "type", value = "score", capacity_ort_pre,capacity_ortT_t1_ss4,capacity_ortT_t2_ss4,capacity_ortT_t3_ss4,capacity_ortT_t4_ss4,capacity_ort_post) %>% 
  convert_as_factor(Row.names, type)
str(df)

df %>%
  group_by(type) %>%
  get_summary_stats(score, type = "mean_sd")

df %>%
  group_by(type) %>%
  identify_outliers(score)
# there are no extrem outlier 

df %>%
  group_by(type) %>%
  shapiro_test(score)
# not all normally distributed 

res.aov <- anova_test(data = df, dv = score, wid = Row.names, within = type)
get_anova_table(res.aov)

# ANOVA Table (type III tests)
# 
# Effect  DFn    DFd     F     p p<.05   ges
# 1   type 3.66 135.46 3.671 0.009     * 0.017

# res.fried <- df %>% friedman_test(score ~ type |Row.names)
# res.fried

pwc <- df %>%
  pairwise_t_test(
    score ~ type, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc

s4_pre <- groupedFinal %>% 
  filter(group == 'experiment') %>% 
  select(Row.names, precision_ort_pre,precision_ortT_t1_ss4,precision_ortT_t2_ss4,precision_ortT_t3_ss4,precision_ortT_t4_ss4,precision_ort_post)
df <- s4_pre %>% 
  gather(key = "type", value = "score", precision_ort_pre,precision_ortT_t1_ss4,precision_ortT_t2_ss4,precision_ortT_t3_ss4,precision_ortT_t4_ss4,precision_ort_post) %>% 
  convert_as_factor(Row.names, type)
str(df)

df %>%
  group_by(type) %>%
  get_summary_stats(score, type = "mean_sd")

df %>%
  group_by(type) %>%
  identify_outliers(score)
# there are no extrem outlier 

df %>%
  group_by(type) %>%
  shapiro_test(score)
# not all normally distributed 

res.aov <- anova_test(data = df, dv = score, wid = Row.names, within = type)
get_anova_table(res.aov)

pwc <- df %>%
  pairwise_t_test(
    score ~ type, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc


s6_pre <- groupedFinal %>% 
  filter(group == 'experiment') %>% 
  select(Row.names, precision_ortT_t1_ss6,precision_ortT_t2_ss6,precision_ortT_t3_ss6,precision_ortT_t4_ss6)
df <- s6_pre %>% 
  gather(key = "type", value = "score", precision_ortT_t1_ss6,precision_ortT_t2_ss6,precision_ortT_t3_ss6,precision_ortT_t4_ss6) %>% 
  convert_as_factor(Row.names, type)
str(df)

df %>%
  group_by(type) %>%
  get_summary_stats(score, type = "mean_sd")

df %>%
  group_by(type) %>%
  identify_outliers(score)
# there are no extrem outlier 

df %>%
  group_by(type) %>%
  shapiro_test(score)
# not all normally distributed 

res.aov <- anova_test(data = df, dv = score, wid = Row.names, within = type)
get_anova_table(res.aov)

pwc <- df %>%
  pairwise_t_test(
    score ~ type, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc

# participant response distribution from pre-post?
setwd("~/Desktop/R/vwmt")
groupinfo <- read.csv("groupInfo.csv")
setwd("~/Desktop/R/vwmt-HIMM /cleanedPrePost")
pre_ORT <- read.csv("ort_pre.csv")
pre_ORT <- cbind(pre_ORT,time = c("pre"))
post_ORT <- read.csv("ort_post.csv")
post_ORT <- cbind(post_ORT,time = c("post"))
prepost_ORT <- rbind(pre_ORT,post_ORT)
ORT <- cbind(prepost_ORT,type = c("orient"))

pre_SRT <- read.csv("srt_pre.csv")
pre_SRT <- cbind(pre_SRT,time = c("pre"))
post_SRT <- read.csv("srt_post.csv")
post_SRT <- cbind(post_SRT,time = c("post"))
prepost_SRT <- rbind(pre_SRT,post_SRT)
SRT <- cbind(prepost_SRT, type = c("shape"))

ORTSRT <- rbind(ORT,SRT)
extraExclude <- c('5eaeb8ccea726058c6eb9a16','5ea93196653dfc000b23516b','5fad4db063bdee02d14d9511','5f0d98c1c8a2f100082295b8','5fa0100e226a1a000c4c19c3','5f566c4ac45b0308940a000b','5da9b30a3624c200157bd9f0','5f2c18b525352005ad5e5c28','5ee10d5b0017d10ec8702d10','5f636c7645f61e036f74ccbf','5cae09506d573900175d4727','5fa465fa1bae7e0d3724bfa2','5f64c18a0d65c4021a1c3370','5f9765e68112f6137e06db5d')
n82 <- subset(ORTSRT,!(extId%in% extraExclude))
n82$givenResponse[n82$givenResponse< 1] <- 360
df <- merge(x=n82, y=groupinfo, by.x="extId", by.y="Row.names")




df2 <- df %>% subset(type=='orient'& group == 'experiment')
  ggplot(df2, aes(x=givenResponse, linetype = time, fill = time, color =time)) +
  geom_histogram(aes(y=100*..density..),alpha = 0.2, bins = 60,color = 'white')+
  scale_color_manual(values=c( "#CE0064", "#3C3E8E"))+
  scale_fill_manual(values=c( "#CE0064", "#3C3E8E"))+
  geom_density(aes(y=100*..density..,color = time),alpha=0,fill = "white", size =1.5)+
  scale_x_continuous(breaks=seq(0,360,45), limits=c(1,360))+
    labs(x="Responses (degree)", y = "Density (%)")+
    theme(legend.position = "none",legend.title=element_blank(),  element_line(colour = 'black',size = 1),panel.background=element_blank(),axis.line.y.left = element_line(size = 1),axis.line.x.bottom  = element_line(size = 1),axis.text = element_text(colour = 'black',face="bold"),axis.text.y.left = element_text(colour = 'black',face="bold"),axis.title = element_text(colour = 'black',face="bold"))
  ggsave("ort_experomental_response_pre_post.png",width = 4,height = 4)
df3 <- df %>% subset(type=='orient'& group == 'control') 
  ggplot(df3, aes(x=givenResponse, linetype = time, fill = time, color =time)) +
  geom_histogram(aes(y=100*..density..),alpha = 0.2, bins = 60,color = 'white')+
  scale_color_manual(values=c( "#CE0064", "#3C3E8E"))+
  scale_fill_manual(values=c( "#CE0064", "#3C3E8E"))+
  geom_density(aes(y=100*..density..,color = time),alpha=0,fill = "white", size =1.5)+
  scale_x_continuous(breaks=seq(0,360,45), limits=c(1,360))+
  labs(x="Responses (degree)", y = "Density (%)")+
  theme(legend.position = "none",legend.title=element_blank(),  element_line(colour = 'black',size = 1),panel.background=element_blank(),axis.line.y.left = element_line(size = 1),axis.line.x.bottom  = element_line(size = 1),axis.text = element_text(colour = 'black',face="bold"),axis.text.y.left = element_text(colour = 'black',face="bold"),axis.title = element_text(colour = 'black',face="bold"))
  ggsave("ort_control_response_pre_post.png",width = 4,height = 4)
df4 <- df %>% subset(type=='shape'& group == 'experiment') 
  ggplot(df4, aes(x=givenResponse, linetype = time, fill = time, color =time)) +
    geom_histogram(aes(y=100*..density..),alpha = 0.2, bins = 60,color = 'white')+
    scale_color_manual(values=c( "#CE0064", "#3C3E8E"))+
    scale_fill_manual(values=c( "#CE0064", "#3C3E8E"))+
    geom_density(aes(y=100*..density..,color = time),alpha=0,fill = "white", size =1.5)+
    scale_x_continuous(breaks=seq(0,360,45), limits=c(1,360))+
    labs(x="Responses (degree)", y = "Density (%)")+
    theme(legend.position = "none",legend.title=element_blank(),  element_line(colour = 'black',size = 1),panel.background=element_blank(),axis.line.y.left = element_line(size = 1),axis.line.x.bottom  = element_line(size = 1),axis.text = element_text(colour = 'black',face="bold"),axis.text.y.left = element_text(colour = 'black',face="bold"),axis.title = element_text(colour = 'black',face="bold"))
  ggsave("srt_exper_response_pre_post.png",width = 4,height = 4)
  
  df5 <- df %>% subset(type=='shape'& group == 'control') 
  ggplot(df5, aes(x=givenResponse, linetype = time, fill = time, color =time)) +
    geom_histogram(aes(y=100*..density..),alpha = 0.2, bins = 60,color = 'white')+
    scale_color_manual(values=c( "#CE0064", "#3C3E8E"))+
    scale_fill_manual(values=c( "#CE0064", "#3C3E8E"))+
    geom_density(aes(y=100*..density..,color = time),alpha=0,fill = "white", size =1.5)+
    scale_x_continuous(breaks=seq(0,360,45), limits=c(1,360))+
    labs(x="Responses (degree)", y = "Density (%)")+
    theme(legend.position = "none",legend.title=element_blank(),  element_line(colour = 'black',size = 1),panel.background=element_blank(),axis.line.y.left = element_line(size = 1),axis.line.x.bottom  = element_line(size = 1),axis.text = element_text(colour = 'black',face="bold"),axis.text.y.left = element_text(colour = 'black',face="bold"),axis.title = element_text(colour = 'black',face="bold"))
  ggsave("srt_control_response_pre_post.png",width = 4,height = 4)
  
  
ggplot(df2,aes(x= givenResponse))+
  geom_histogram(bins = 30,alpha=0.05,position="identity")+
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+ 
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))+
  geom_density(aes(y=..count..,linetype = time),color= 'red')

df3 <- df %>% subset(type=='orient'& group == 'control')
ggplot(df3,aes(x= givenResponse))+
  geom_histogram(aes(fill=time), bins = 90,alpha=0.3)+
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+ 
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))+
  geom_density(aes(y= ..count..,linetype = time),color= 'red')

  geom_vline(xintercept = c(45,90,135,180,225,270,315), linetype="dashed", 
             color = "red", size=1.5)
  

  df4 <- df %>% subset(type=='shape'& group == 'experiment')
  ggplot(df4, aes(x=givenResponse, color=time, fill=time)) +
    geom_histogram(aes(y=..density..), position="identity", alpha=0.3, bins = 90)+
    geom_density(alpha=0.6)+
    scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
    scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
    labs(title="Exploratory",x="Responses", y = "Density")+
    theme_classic()
  
  
  df5 <- df %>% subset(type=='shape'& group == 'control')
  ggplot(df5, aes(x=givenResponse, color=time, fill=time)) +
    geom_histogram(aes(y=..density..), position="dodge", alpha=0.3, bins = 90)+
    geom_density(alpha=0.6)+
    scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
    scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
    labs(title="Exploratory",x="Responses", y = "Density")+
    theme_classic()
