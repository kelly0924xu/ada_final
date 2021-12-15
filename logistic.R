library(foreign)
library(ggplot2)
library(dplyr)
library(table1)
library(car)

demographic = read.xport('/Users/kelly/Desktop/ada/ada_final/dataset/DEMO_J.XPT')
income = read.xport('/Users/kelly/Desktop/ada/ada_final/dataset/INQ_J.XPT')
self_smoking = read.xport('/Users/kelly/Desktop/ada/ada_final/dataset/SMQ_J.XPT')
family_smoking = read.xport('/Users/kelly/Desktop/ada/ada_final/dataset/SMQFAM_J.XPT')
depression = read.xport('/Users/kelly/Desktop/ada/ada_final/dataset/DPQ_J.XPT')
----------------------------------
self_smoking$cursmoker = ifelse(self_smoking$SMQ040 %in% c(1,2), 1, ifelse(self_smoking$SMQ040 == 3,0,NA))
table(self_smoking$cursmoker) # this gives us 1021 current smokers

self_smoking$allsmoker = ifelse(self_smoking$SMD030<=76&self_smoking$SMD030>=7, 1, ifelse(self_smoking$SMD030 == 0,0,NA))
table(self_smoking$allsmoker, useNA = "ifany") #this gives us 2285 people who have ever smoked
-------------------------------
table(self_smoking$SMQ670) #tried to quit smoking 1-yes 2-no
table(self_smoking$SMQ848) #times stopped smoking
table(family_smoking$SMD470) #this gives us numbers of families who smoke at home
#variables to control for:
table(demographic$RIAGENDR) #gender 1-male 2-female
table(demographic$RIDAGEYR) #age in years
table(demographic$RIDRETH3) #race 1-Mexican American 2-Other Hispanic 3-White 4-Black 6-Asian 7-other
table(demographic$DMDCITZN) #citizenship 1-citizen 2-not a citizen of US
table(demographic$DMDMARTL) #marital status 1-married 2-widowed 3-divorced 4-separated 5-never married 6-living with a partner
table(demographic$DMDEDUC2) #adults 20+  5-college graduate or above; 1-less than 9th grade
table(income$INDFMMPI) #family monthly poverty level index
--------------------------------
  #merge the three datasets onto one for convenience 
a1 = merge(self_smoking,family_smoking,by='SEQN')
a2 = merge(a1,demographic,by='SEQN')
a3 = merge(a2,income,by='SEQN')
 #select the needed columns
library(dplyr)
a4  = a3 %>% 
  select(cursmoker, allsmoker, SMQ670, RIDRETH3,SMQ848, SMD470, RIAGENDR, RIDAGEYR, DMDCITZN, DMDMARTL, DMDEDUC2, INDFMMPI)
a5 = rename(a4, TriedQuit = SMQ670,
       TimesStop = SMQ848,
       FaNu_smokeAtHome = SMD470,
       gender = RIAGENDR,
       age = RIDAGEYR,
       citizenship = DMDCITZN,
       marital_sta = DMDMARTL,
       education = DMDEDUC2,
       poverty = INDFMMPI,
       race = RIDRETH3)
--------------------------------------
#deal with NAs
sapply(a5, function(x) sum(length(which(is.na(x)))))

source('mcar_test.R')
mcar_test(a5) # the result shows completely miss at random, so can list-wise delete

a10 = a5[which(a5$cursmoker ==1),]
a6 = a10[complete.cases(a10[,-5]),]#For TimesStop exist, the Triedquit==1
sapply(a6, function(y) sum(length(which(is.na(y)))))
tf = c(1,2,3,4,7,9,10,11)
for(i in tf){
  a6[,i] = as.factor(a6[,i])
}
---------------------------------------
#create Table 1
levels(a6$TriedQuit) = c('Yes','No')
levels(a6$gender) = c('Male','Female')
levels(a6$race) = c('Mexican American','Other Hispanic','Non-Hispanic White','Non-Hispanic Black','Non-Hispanic Asian',"Other Race")
levels(a6$marital_sta) = c('Married','Widowed','Divorced','Separated',"Never married",'Living with partner')
levels(a6$citizenship) = c('Citizen bu birth or naturalization','Not a citizen of the US','Refused','Don
                          \'t know')
levels(a6$education) = c('Less than 9th grade','9-11th grade','High school graduate','Some college','College graduate','Don\'t know')

table1(~ FaNu_smokeAtHome+ gender + race + age + marital_sta + citizenship + education + poverty|TriedQuit, a6)

-------------------------------------
#Regression on TriedQuit   
l1 = glm((2-(TriedQuit)) ~ (FaNu_smokeAtHome) + gender + age + citizenship + marital_sta + education + poverty,data=a10,family = 'binomial')
summary(l1) #original result

l2 = glm((2-as.numeric(TriedQuit)) ~ as.numeric(FaNu_smokeAtHome) + gender + age + citizenship + marital_sta + education + poverty,data=a6,family = 'binomial')
summary(l2) #list-wise deletion result
--------------------------------------
#assumption check for logistic regression
#check linearity
a6 <- a6 %>%
  mutate(fmh.times.logbmi = as.numeric(FaNu_smokeAtHome) * log(as.numeric(FaNu_smokeAtHome))) #create term to test linearity
(2-as.numeric(TriedQuit)) ~ as.numeric(FaNu_smokeAtHome) + gender + age + citizenship + marital_sta + education + poverty
boxTidwellBMI <- glm((2-as.numeric(TriedQuit)) ~ as.numeric(FaNu_smokeAtHome) + fmh.times.logbmi, data=a6, family="binomial") #Box Tidwell technique, test the assumption of linearity
summary(boxTidwellBMI)#The coefficient between number of families smoke at home and its log is not significant, the linearity assumption is not violated.

#multicollinearity - vif
vif(l2)#all VIF<5, there’s no multicollineari.

#outliers - cook's distance
plot(l2, which=4, id.n=5, col="red") 
l3 = c(4440,1118,5397)
n3 = sapply(l3,function(x) which(names(c2)==x))
a6_2 = a6[-n3,]
l3 = glm((2-as.numeric(TriedQuit)) ~ as.numeric(FaNu_smokeAtHome) + gender + age + citizenship + marital_sta + education + poverty,data=a6_2,family = 'binomial')
summary(l3)#After removing three points with large cook’s distance, the change of log(OR) is small.


