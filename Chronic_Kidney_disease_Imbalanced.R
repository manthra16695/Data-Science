##To build a model to predict the chronic kidney disease in patients from a highly imbalanced dataset
getwd()
data=read.csv('Chronic Kidney Disease Dataset.csv')
str(data)
##CKD= Chronic Kidney Disease
##Checking for data balance, if the target variables distributed are balanced
table(data$CKD)


##Seems like the data is heavily imbalanced especially positive '1' cases are very low which 
##weakens our model in predicting positive cases

##Data balancing using sampling has to be done either using Over sampling or under sampling

##Data Preparation
##Converting the variables to specified formats based on their values observed
data$PVD                 =as.factor(data$PVD)
data$Activity            =as.factor(data$Activity  )
data$PoorVision          =as.factor(data$PoorVision)
data$Smoker              =as.factor(data$Smoker)
data$Hypertension        =as.factor(data$Hypertension)
data$Fam.Hypertension    =as.factor(data$Fam.Hypertension)
data$Diabetes            =as.factor(data$Diabetes)
data$Fam.Diabetes        =as.factor(data$Fam.Diabetes)
data$Stroke              =as.factor(data$Stroke)
data$CVD                 =as.factor(data$CVD)
data$Fam.CVD             =as.factor(data$Fam.CVD)
data$CHF                 =as.factor(data$CHF)
data$Anemia              =as.factor(data$Anemia)
data$CKD                 =as.factor(data$CKD)
data$Dyslipidemia        =as.factor(data$Dyslipidemia)
data$Obese=as.factor(data$Obese)
data$Unmarried=as.factor(data$Unmarried)


##Dropping the columns which will have no effect on Chronic kidney disease
data$PoorVision=NULL

data$Insured=NULL
data$Educ=NULL
data$ID=NULL

data$Educ=NULL
data$Income=NULL
data$Insured=NULL

##Exploratory Data Analysis
##Descriptive Statistics and Variable Selection
##Selecting variables which has huge significance with target variable

boxplot(Age~CKD,data = data,col=c('red','blue'),main='Age and CKD') ##Age has a significant impact in CKD it seems like average age above 70
##has higher chances for cancer

t.test(Age~CKD,data=data)

##Checking for significance between racegroup and CKD
chisq.test(data$CKD,data$Racegrp)
racetab=table(data$CKD,data$Racegrp)
mosaicplot(racetab,main='Race and CKD')
plot(prop.table(xtabs(~data$CKD+data$Racegrp)))


##Checking for significance between BMI and CKD
nockd=data$BMI[data$CKD==0]
ckd=data$BMI[data$CKD==1]

par(mfrow=c(1,1))
boxplot(nockd,col = 'red')
boxplot(ckd,col='green')

boxplot(ckd,nockd,col=c('red','green'),main='Relation between BMI and CKD')
t.test(ckd,nockd,conf.level = 0.99)

##Checking for significance between care source and CKD
table(data$CKD,data$CareSource)
chisq.test(data$CareSource,data$CKD)

##Checking for significance between Sex and CKD
tab=table(data$Female,data$CKD)
plot(prop.table(xtabs(~data$Female+data$CKD)))
xtabs(~data$Female+data$CKD)
chisq.test(data$Female,data$CKD)

##Checking for significance between Unmarried and CKD
plot(xtabs(~data$CKD+data$Unmarried))

####Checking for significance between weight and CKD
boxplot(data$BMI~data$CKD)
summary(t.test(data$CKD~data$BMI))


####Checking for significance between Obese and CKD
barplot(table(data$Obese,data$CKD))

chisq.test(data$Obese,data$CKD)


####Checking for significance between Waist and CKD
plot(data$CKD,data$Waist)

ckdwaist=data$Waist[data$CKD==0]

nockdwaist=data$Waist[data$CKD==1]
t.test(ckdwaist,nockdwaist,conf.level = 0.99)


####Checking for significance between SBP and CKD
par(mfrow=c(1,1))
plot(data$CKD,data$SBP)
bpnockd=data$SBP[data$CKD==0]
bpckd=data$SBP[data$CKD==1]

t.test(bpnockd,bpckd)
###SBP is significant

####Checking for significance between DBP and CKD
par(mfrow=c(1,1))
plot(data$CKD,data$DBP)
bpnockd=data$DBP[data$CKD==0]
bpckd=data$DBP[data$CKD==1]

t.test(bpnockd,bpckd,alternative ='less')
###DBP is significant

##Checking for significance between Cholestrol levels and CKD
plot(data$CKD,data$HDL)
plot(data$CKD,data$LDL)
plot(data$CKD,data$Total.Chol )

ckdtotchol=data$HDL[data$CKD==0]

nockdtotchol=data$HDL[data$CKD==1]
t.test(ckdtotchol,nockdtotchol)
t.test(data$Total.Chol~data$CKD)


##Checking for significance between Dyslipidemia and CKD
barplot(table(data$Dyslipidemia,data$CKD))
chisq.test(data$CKD,data$Dyslipidemia)


##Checking for significance between PVD and CKD

barplot(table(data$PVD,data$CKD))
chisq.test(data$PVD,data$CKD)

bpnockd=data$PVD[data$CKD==0]
bpckd=data$PVD[data$CKD==1]

##Checking for significance between Physical Activity and CKD

barplot(table(data$Activity,data$CKD))
chisq.test(data$Activity,data$CKD)

##Checking for significance between Smoking habit and CKD

barplot(table(data$Smoker,data$CKD),main='Relationship between SMoking and CKD',legend=c('Non-Smoker','Smoker'))
chisq.test(data$CKD,data$Smoker)
table(data$Smoker,data$CKD)


##Checking for significance between Hypertension and CKD

barplot(table(data$Hypertension,data$CKD))
xtabs(~data$Hypertension+data$CKD)
chisq.test(data$CKD,data$Hypertension)

##Fam.Hypertension
barplot(table(data$Fam.Hypertension,data$CKD))
chisq.test(data$CKD,data$Fam.Hypertension)

##Diabetes
barplot(table(data$Diabetes,data$CKD))
chisq.test(data$CKD,data$Diabetes)

##Fam.Diabetes 
barplot(table(data$Fam.Diabetes ,data$CKD))
chisq.test(data$CKD,data$Fam.Diabetes )

##Stroke
barplot(table(data$Stroke,data$CKD))
chisq.test(data$CKD,data$Stroke)

##CVD
barplot(table(data$CVD,data$CKD))
chisq.test(data$CKD,data$CVD)

##Fam.CVD
barplot(table(data$Fam.CVD,data$CKD))
chisq.test(data$CKD,data$Fam.CVD)

##CHF
barplot(table(data$CHF,data$CKD))
chisq.test(data$CKD,data$CHF)

##Anemia
barplot(table(data$Anemia,data$CKD))
chisq.test(data$CKD,data$Anemia)


##Significant Variables
##Age
##Racegrp
##unmarried
##caresource
##Waist
##SBP
##DBP
##HDL
##LDL
##PVD
##Activity
##Smoker
##Hypertension
##Diabetes
##Stroke 
##CVD    
##Fam.CVD
##CHF    
##Anemia


##Data Cleaning and NULL value Handling
##handling NA values and removing all records which have at least one of their variable as NA
complete.cases(data)
naval=which(!complete.cases(data))##Gives rows which has NA values
str(naval)
str(data)
prdata=data[-naval,]##Final processed data which does not have any NA values
str(prdata)

table(prdata$CKD)

##Train - Test Split we take 75% data for Train data and 25% test data.

set.seed(111)
indx=sample(2,nrow(prdata),replace=TRUE ,prob=c(0.75,0.25) )
traindata=prdata[indx==1,]
testdata=prdata[indx==2,]

table(testdata$CKD)

##Simple Logistic regression Model in Imbalanced dataset
modl=glm(CKD~Age+Racegrp+Unmarried+CareSource+Waist+SBP+DBP+HDL+LDL+PVD+Activity+Smoker+Hypertension+Diabetes+Stroke+CVD+Fam.CVD+CHF+Anemia,data=traindata,family = "binomial")
summary(modl)
pred=predict(modl,testdata)
predic=ifelse(pred>=0.5, 1,0)
predic=as.factor(predic)


##Oversampling since NUmber of positive 1 CKD values are very less in proportion
##less positive values are scaled up to match the number of Negative values
install.packages("ROSE")
library(ROSE)
over=ovun.sample(CKD~Age+Racegrp+Unmarried+CareSource+Waist+SBP+DBP+HDL+LDL+PVD+Activity+Smoker+Hypertension+Diabetes+Stroke+CVD+Fam.CVD+CHF+Anemia,data=traindata,method='over',N=6972)$data
table(over$CKD)

##Under Sampling
##Higher Negative values are scaled down to match the number of Positive values present
under=ovun.sample(CKD~Age+Racegrp+Unmarried+CareSource+Waist+SBP+DBP+HDL+LDL+PVD+Activity+Smoker+Hypertension+Diabetes+Stroke+CVD+Fam.CVD+CHF+Anemia,data=traindata,method='under',N=488)$data
table(under$CKD)



##confusionMatrix
library(caret)
install.packages("e1071")
library(e1071)

confusionMatrix(predic,testdata$CKD,positive='1')
levels(predic)

###Plotting ROC curve to identify a better threshold value and to identify the performance of our model
install.packages("ROCR")
library(ROCR)
prd=prediction(as.numeric(predic),as.numeric(prdata$CKD))
roc=performance(prd,"tpr","fpr")
plot(roc)


##Logistic Regression built using Over Sampled data
overmodl=glm(CKD~Age+Racegrp+Unmarried+CareSource+Waist+SBP+DBP+HDL+LDL+PVD+Activity+Smoker+Hypertension+Diabetes+Stroke+CVD+Fam.CVD+CHF+Anemia,data=over,family = "binomial")
summary(overmodl)
pr=predict(overmodl,testdata)
pre=ifelse(pr>=0.5, 1,0)
pre=as.factor(pre)
confusionMatrix(pre,testdata$CKD,positive='1')



##Logistic Regression built using Over Sampled data
undermodl=glm(CKD~Age+Racegrp+Unmarried+CareSource+Waist+SBP+DBP+HDL+LDL+PVD+Activity+Smoker+Hypertension+Diabetes+Stroke+CVD+Fam.CVD+CHF+Anemia,data=under,family = "binomial")
summary(undermodl)
pr=predict(undermodl,testdata)
pre=ifelse(pr>=0.5, 1,0)
pre=as.factor(pre)
confusionMatrix(pre,testdata$CKD,positive='1')


##library(epiDisplay)
##logistic.display(undermodl) Odds ratio to see how values in a column affects the occurence of CKD


##Random Forests built using Over Sampled data
install.packages("randomForest")
library(randomForest)
overrandomfrstmodl=randomForest(CKD~Age+Racegrp+Unmarried+CareSource+Waist+SBP+DBP+HDL+LDL+PVD+Activity+Smoker+Hypertension+Diabetes+Stroke+CVD+Fam.CVD+CHF+Anemia,data=over)
plot(overmodl)
pr=predict(overrandomfrstmodl,testdata)

confusionMatrix(pr,testdata$CKD,positive='1')



##Random Forests built using Under Sampled data
underrandomforstmodl=randomForest(CKD~Age+Racegrp+Unmarried+CareSource+Waist+SBP+DBP+HDL+LDL+PVD+Activity+Smoker+Hypertension+Diabetes+Stroke+CVD+Fam.CVD+CHF+Anemia,data=under)
summary(underrandomforstmodl)
pr=predict(underrandomforstmodl,testdata)
confusionMatrix(pr,testdata$CKD,positive='1')

##SVM built using Over Sampled data

library(e1071)
oversvmmodl=svm(CKD~Age+Racegrp+Unmarried+CareSource+Waist+SBP+DBP+HDL+LDL+PVD+Activity+Smoker+Hypertension+Diabetes+Stroke+CVD+Fam.CVD+CHF+Anemia,data=over)
summary(oversvmmodl)
pr=predict(oversvmmodl,testdata)

confusionMatrix(pr,testdata$CKD,positive='1')



##SVM built using Under Sampled data
undersvmmodl=svm(CKD~Age+Racegrp+Unmarried+CareSource+Waist+SBP+DBP+HDL+LDL+PVD+Activity+Smoker+Hypertension+Diabetes+Stroke+CVD+Fam.CVD+CHF+Anemia,data=under)
summary(undersvmmodl)
pr=predict(undersvmmodl,testdata)
confusionMatrix(pr,testdata$CKD,positive='1')

##Other Sampling techniques include SMOTE and both over and under sampling involved

