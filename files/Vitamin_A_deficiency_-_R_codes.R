# ===================== STA 224 Final: ICHS ========================== #

# ======================== 0. Input Data ============================= #
ichs <- read.csv("https://raw.githubusercontent.com/zhikuanquan/UCD-STA/master/STA224/ichs.csv")

# ================= a. Exploratory Data Analysis ===================== #
library(ggplot2)
library(tidyverse)

### Distribution of Baseline Covariates Across Subjects ###
baseline <- ichs[ichs$time==0,]
# gender: male:female = 115:135
table(baseline$gender)
# baseline age (years)
#  1  2  3  4  5  6  7 
# 45 40 33 37 37 38 20 
table(baseline$bage)
# respiratory infection
# 0 = no, 1 = yes
# 183  :  67
table(baseline$infect)
# Vitamin A deficiency
# 0 = no,  1 = yes
# 159  :  91
table(baseline$vita)

### Distribution of the response variable for different time ###
infect_time <- ichs %>%
  group_by(time,infect) %>%
  tally()
infect_time$infect <- ifelse(infect_time$infect==0,'no','yes')
infect_time$time <- as.factor(infect_time$time)
# It seems that the distribution of infection are stable across time
ggplot(data = infect_time, aes(fill=infect, y=n, x=time)) + 
  geom_bar(position="dodge", stat="identity")+
  ylab('Number of Respiratory Infection')+
  xlab('Months')+
  theme_light()
infect_tc <- ichs %>%
  group_by(time) %>%
  summarize(infect = mean(infect))
ggplot(data = infect_tc, aes(x = time, y = infect))+
  geom_line(color = 'darkred',size = 1)+
  ylab('Proportion Respiratory Infection')+
  xlab('Months')+
  theme_light()

### Distribution of the response variable for different gender groups ###
infect_gender <- ichs %>%
  group_by(gender,infect) %>%
  tally()
infect_gender$infect <- ifelse(infect_gender$infect==0,'no','yes')
infect_gender$gender <- ifelse(infect_gender$gender==0,'male','female')
# It seems that the distribution of infection are similar between female/male
# In male group, more proportion of infection
ggplot(data = infect_gender, aes(fill=infect, y=n, x=gender)) + 
  geom_bar(position="dodge", stat="identity")+
  ylab('Number of respiratory infection')+
  xlab('Gender')+
  theme_light()

### Distribution of the response variable for different bage groups ###
infect_bage <- ichs %>%
  group_by(bage,infect) %>%
  tally()
infect_bage$infect <- ifelse(infect_bage$infect==0,'no','yes')
infect_bage$bage <- as.factor(infect_bage$bage)
# It seems that the distribution of infection are almost stable between
# groups with different baseline age. (2-3 years old tends to have higher risk)
ggplot(data = infect_bage, aes(fill=infect, y=n, x=bage)) + 
  geom_bar(position="dodge", stat="identity")+
  ylab('Number of respiratory infection')+
  xlab('Baseline Age')+
  theme_light()

### Distribution of the response variable for different vita groups ###
infect_vita <- ichs %>%
  group_by(vita,infect) %>%
  tally()
infect_vita$infect <- ifelse(infect_vita$infect==0,'no','yes')
infect_vita$vita <- ifelse(infect_vita$vita==0,'no','yes')
# It seems that people with Vitamin-A deficiency tend to have infection.
ggplot(data = infect_vita, aes(fill=infect, y=n, x=vita)) + 
  geom_bar(position="dodge", stat="identity")+
  ylab('Number of respiratory infection')+
  xlab('Vitamin-A Deficiency')+
  theme_light()


### Distribution of the vita for different gender groups ###
vita_gender <- ichs %>%
  group_by(gender,vita) %>%
  tally()
vita_gender$vita <- ifelse(vita_gender$vita==0,'no','yes')
vita_gender$gender <- ifelse(vita_gender$gender==0,'male','female')
# It seems that the vita are similar between female/male
ggplot(data = vita_gender, aes(fill=vita, y=n, x=gender)) + 
  geom_bar(position="fill", stat="identity")+
  ylab('Proportion of Vitamin-A deficiency')+
  xlab('Gender')+
  theme_light()

### Distribution of the vita for different bage groups ###
vita_bage <- ichs %>%
  group_by(bage,vita) %>%
  tally()
vita_bage$vita <- ifelse(vita_bage$vita==0,'no','yes')
vita_bage$bage <- as.factor(vita_bage$bage)
# It seems that the distribution of vita are almost stable between
# groups with different baseline age. (4-5 years old tends to have higher risk)
ggplot(data = vita_bage, aes(fill=vita, y=n, x=bage)) + 
  geom_bar(position="fill", stat="identity")+
  ylab('Proportion of Vitamin-A deficiency')+
  xlab('Baseline Age')+
  theme_light()

### How response changes by checking how many subjects with an infection ###
### at a given wave will still have an infection at the following wave   ###
changes <- ichs %>%
  group_by(id) %>%
  summarize(n_change = length(unique(infect)),first = infect[1]) %>%
  group_by(n_change,first) %>%
  tally()
# No change =  136 (first0:first1 = 109:27)
# Change = 114 (first0:first1 = 74:40)
## For the patients with changes
id_changes <-ichs %>%
  mutate(first = infect[1]) %>%
  group_by(id,first) %>%
  summarize(n_change = length(unique(infect)),num = abs(sum(first - infect))) %>%
  filter(n_change == 2)
table(id_changes$num)

# ================= b. Correlation Structure  ===================== #
## i) removing effects of mean model
logit.model <- glm(infect~(time+gender+bage+vita)^2,data=ichs,family="binomial")
summary(logit.model)
## ii) Calculate standardized residuals
pred_infect <- predict(logit.model,type="response")
#preliminary standardized residual
r <- (ichs$infect - pred_infect)/sqrt(pred_infect*(1-pred_infect))
ichs$r <- r
wide <- reshape(ichs[,c('id','r','time')],v.names="r",idvar="id",timevar="time",
              direction="wide")
# correlation matrix
cor(wide[,-1],use="pairwise.complete.obs")
library(psych)
pairs.panels(wide[,-1], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = FALSE # show correlation ellipses
)
## iii) Autocorrelation function and correlogram
# extract all pairs within each subject
ichs$id <- as.factor(ichs$id)
id.list <- levels(ichs$id)
data.pairs=list()
for(i in 1:length(id.list)){
  subject.i <- ichs[ichs$id==id.list[i],]
  data.pairs.i <- gtools::combinations(dim(subject.i)[1], 2,repeats=FALSE)
  #index of all possible pairs
  data.pairs.add=data.frame(id=rep(i,dim(data.pairs.i)[1]),
                            obs1=subject.i$time[data.pairs.i[,1]],
                            obs2=subject.i$time[data.pairs.i[,2]],
                            r1=subject.i$r[data.pairs.i[,1]],
                            r2=subject.i$r[data.pairs.i[,2]]) 
  # all pairs within subject i
  data.pairs <- rbind(data.pairs,data.pairs.add)
}
data.pairs$lag <- abs(data.pairs$obs2-data.pairs$obs1)

ACF <- c()
TL <- c()
for(lag in sort(unique(data.pairs$lag))){
  subset=data.pairs[which(data.pairs$lag ==lag),4:5]
  ACF[lag]=cor(subset)[1,2]
  TL[lag]=1.96/sqrt(dim(subset)[1])
}
summ.acf=cbind(ACF,TL)
ACF <- na.omit(ACF)
TL <- na.omit(TL)
# correlogram: smooth -> exchangeable correlation matrix
plot(sort(unique(data.pairs$lag)),ACF,"l",xlab="Time lag",ylim=c(0,1))
lines(sort(unique(data.pairs$lag)),TL,lty=2)
legend("topright",c("Autocorrelation function","95% tolerance limit"),lty=1:2)

# ================= c. GEE Model Selection  ===================== #
ichs$vita <- as.factor(ichs$vita)
ichs$gender <- as.factor(ichs$gender)
library(geepack)
# Base Model
# gender1:vita1 = 0.957
gee.model_1 <- geeglm(infect~(time+gender+bage+vita)^2, data=ichs, waves = time,
                      id = id, family=binomial("logit"), corstr="exch", std.err="san.se")
summary(gee.model_1)

# Model_2: time:gender1 = 0.861
gee.model_2 <- geeglm(infect~(time+bage+vita)^2+gender+gender:bage + gender:time, data=ichs, waves = time,
                      id = id, family=binomial("logit"), corstr="exch", std.err="san.se")
summary(gee.model_2)

# Model_3: gender1 = 0.775
gee.model_3 <- geeglm(infect~(time+bage+vita)^2+gender+gender:bage, data=ichs, waves = time,
                     id = id, family=binomial("logit"), corstr="exch", std.err="san.se")
summary(gee.model_3)
# Model_4: time:vita1 = 0.6168 
gee.model_4 <- geeglm(infect~(time+bage+vita)^2+gender:bage, data=ichs, waves = time,
                      id = id, family=binomial("logit"), corstr="exch", std.err="san.se")
summary(gee.model_4)

# Model_5: bage = 0.5328 
gee.model_5 <- geeglm(infect~time+time:bage+(bage+vita)^2+gender:bage, data=ichs, waves = time,
                      id = id, family=binomial("logit"), corstr="exch", std.err="san.se")
summary(gee.model_5)

# Model_6: vita1 = 0.23859; time:bage = 0.15656
gee.model_6 <- geeglm(infect~time+time:bage+bage:vita+vita+gender:bage, data=ichs, waves = time,
                      id = id, family=binomial("logit"), corstr="exch", std.err="san.se")
summary(gee.model_6)

# Model_7: bage:vita = 0.06704, vita = 0.37851 
ichs_s <- ichs
ichs_s$vita <- as.numeric(ichs_s$vita)-1
gee.model_7 <- geeglm(infect~time+vita:bage+vita+gender:bage, data=ichs_s, waves = time,
                          id = id, family=binomial("logit"), corstr="exch", std.err="san.se")
summary(gee.model_7)

# Model_8: vita = 0.2042
ichs_s$gender <- as.numeric(ichs_s$gender)
gee.model_8 <- geeglm(infect~time+vita+gender:bage, data=ichs_s, waves = time,
                      id = id, family=binomial("logit"), corstr="exch", std.err="san.se")
summary(gee.model_8)

## Compare the QIC
sapply(list(gee.model_1,
            gee.model_2,
            gee.model_3,
            gee.model_4,
            gee.model_5,
            gee.model_6, # Best one!
            gee.model_7,
            gee.model_8),
       QIC)

### FINAL MODEL ###
gee.model_Final <- geeglm(infect~time+time:bage+bage:vita+vita+gender:bage, data=ichs, waves = time,
                      id = id, family=binomial("logit"), corstr="exch", std.err="san.se")
summary(gee.model_Final)
### CI for beta ###
tidy(gee.model_Final,conf.int = TRUE,conf.level = 0.95)

# ================= d. GEE Model Linearity  ===================== #
ichs_gee <- ichs
ichs_gee$pred_gee <- gee.model_Final$fitted.values
ichs_gee$vita <- as.numeric(ichs_gee$vita)-1
# Empirical and predicted proportions: time
time_gee <- ichs_gee %>%
  group_by(time) %>%
  summarize(pred_time = mean(pred_gee),emp_time = mean(infect))
plot_time_gee <- gather(time_gee, variable, value, -time)
ggplot(plot_time_gee, aes(x = time, y = value, color = variable)) +
  geom_line(size = 1) +
  ylim(0,1)+
  scale_color_discrete(name = "Type", labels = c("Empirical", "Predictied"))+
  theme_light()
# Empirical and predicted proportions: bage
bage_gee <- ichs_gee %>%
  group_by(bage) %>%
  summarize(pred_time = mean(pred_gee),emp_time = mean(infect))
plot_bage_gee <- gather(bage_gee, variable, value, -bage)
ggplot(plot_bage_gee, aes(x = bage, y = value, color = variable)) +
  geom_line(size = 1) +
  ylim(0,1)+
  scale_color_discrete(name = "Type", labels = c("Empirical", "Predictied"))+
  theme_light()
# Empirical and predicted proportions: gender
gender_gee <- ichs_gee %>%
  group_by(gender) %>%
  summarize(pred_time = mean(pred_gee),emp_time = mean(infect))
plot_gender_gee <- gather(gender_gee, variable, value, -gender)
plot_gender_gee$gender <- as.numeric(plot_gender_gee$gender)
ggplot(plot_gender_gee, aes(x = gender, y = value, color = variable)) +
  geom_line(size = 1) +
  ylim(0,1)+
  scale_color_discrete(name = "Type", labels = c("Empirical", "Predictied"))+
  theme_light()
# Empirical and predicted proportions: vita
vita_gee <- ichs_gee %>%
  group_by(vita) %>%
  summarize(pred_time = mean(pred_gee),emp_time = mean(infect))
plot_vita_gee <- gather(vita_gee, variable, value, -vita)
ggplot(plot_vita_gee, aes(x = vita, y = value, color = variable)) +
  geom_line(size = 1) +
  ylim(0,1)+
  scale_color_discrete(name = "Type", labels = c("Empirical", "Predictied"))+
  theme_light()
# ================= e. GEE Model Interpretation  ===================== #
gee.model_Final <- geeglm(infect~time+time:bage+bage:vita+vita+gender:bage, data=ichs, waves = time,
                          id = id, family=binomial("logit"), corstr="exch", std.err="san.se")
sum_gee <- summary(gee.model_Final)
# ================= f. GEE Model Interpretation  ===================== #
library(lme4)
glmm.model<-glmer(infect ~ (1|id) + time+time:bage+bage:vita+vita+gender:bage, 
                 data=ichs, family=binomial("logit"))
sum_glmm <- summary(glmm.model)
sum_glmm$varcor # v^2 = 2.7^2
ratio <- sum_gee$coefficients[,1]/sum_glmm$coefficients[,1]






