setwd("D:\\HKUST\\Spring 1\\High Dimension\\Project")
rm(list=ls())
library(readxl)
wage <- read_excel("wage.xls",col_names=F,na="NA",skip=0)
name = c("wage","hours","IQ","KWW","educ","exper","tenure","age","married","black","south","urban","sibs","brthord","meduc","feduc","lwage")
colnames(wage) <- name
str(wage)

#scatter plot vs. wage
par(mfrow=c(3,2))
plot(wage$wage,wage$hours)
plot(wage$wage,wage$IQ)
plot(wage$wage,wage$KWW)
plot(wage$wage,wage$educ)
plot(wage$wage,wage$exper)
plot(wage$wage,wage$tenure)
par(mfrow=c(3,2))
plot(wage$wage,wage$age)
plot(wage$wage,wage$sibs)
plot(wage$wage,wage$educ)
plot(wage$wage,wage$exper)
plot(wage$wage,wage$tenure)

#scatter plot vs. lwage
par(mfrow=c(3,2))
plot(wage$lwage,wage$hours)
plot(wage$lwage,wage$IQ)
plot(wage$lwage,wage$KWW)
plot(wage$lwage,wage$educ)
plot(wage$lwage,wage$exper)
plot(wage$lwage,wage$tenure)
par(mfrow=c(3,2))
plot(wage$lwage,wage$age)
plot(wage$lwage,wage$sibs)
plot(wage$lwage,wage$educ)
plot(wage$lwage,wage$exper)
plot(wage$lwage,wage$tenure)

#categorical variables vs. wage - boxplot
library(ggplot2)
wage$married=as.factor(wage$married)
wage$black=as.factor(wage$black)
wage$south=as.factor(wage$south)
wage$urban=as.factor(wage$urban)

married<- ggplot(data=wage) + 
  geom_boxplot(mapping = aes(x = married, y= wage, color=married)) +
  theme(axis.text.x = element_text(size = 10,angle = 45,hjust = 1))+
  labs(title='Married vs. wage')

black<- ggplot(data=wage) + 
  geom_boxplot(mapping = aes(x = black, y= wage, color=black)) +
  theme(axis.text.x = element_text(size = 10,angle = 45,hjust = 1))+
  labs(title='black vs. wage')

south<- ggplot(data=wage) + 
  geom_boxplot(mapping = aes(x = south, y= wage, color=south)) +
  theme(axis.text.x = element_text(size = 10,angle = 45,hjust = 1))+
  labs(title='south vs. wage')

urban<- ggplot(data=wage) + 
  geom_boxplot(mapping = aes(x = urban, y= wage, color=urban)) +
  theme(axis.text.x = element_text(size = 10,angle = 45,hjust = 1))+
  labs(title='urban vs. wage')

gridExtra::grid.arrange(married,black,south,urban, nrow=2)

#delete N/A data
wage <- na.omit(wage)

#corrplot
wage$married=as.numeric(wage$married)
wage$black=as.numeric(wage$black)
wage$south=as.numeric(wage$south)
wage$urban=as.numeric(wage$urban)
par(mfrow=c(1,1))
library(corrplot)
corrplot(cor(wage[,c(1,2,3,4,5,6,7,8,13,14,15,16)]))


#train set vs. test set
set.seed(666)
sub<-sample(1:nrow(wage),round(nrow(wage)*0.8))
data_train<-wage[sub,]
data_test<-wage[-sub,]

#Multiple Linear Regression
RawModel=lm(lwage~hours+IQ+KWW+educ+exper+tenure+age+
              married+black+south+urban+sibs+brthord+meduc+feduc,data=data_train)
summary(RawModel)

##Check linear model assumptions
library(car)
library(MASS)
library(lmtest)
vif(RawModel)
bptest(RawModel)
plot(RawModel,which = 1:2)
par(mfrow=c(1,1)) 
qqnorm(stdres(RawModel),main="Q-Q Plot of Standardized Residuals")
abline(0,1,col="red")

#LASSO
library("lars")
x=as.matrix(data_train[,2:16])
y=as.matrix(data_train[,17])
wage.lasso <- lars(x,y,type="lasso")
wage.lasso
summary(wage.lasso)
wage.lasso$beta
plot(wage.lasso)

#PCA of training set
library(vcd)
data_train_pca<-data_train[,-13]  #remove sibs
wage.pca <- princomp(data_train_pca[,2:15], cor = TRUE) 
summary(wage.pca,loadings = TRUE)
plot(wage.pca,type="l")

##Create a dataframe of scores
pc_wage=as.data.frame(wage.pca$scores[,1:3])
pc_wage$lwage=data_train_pca$lwage
data_test_pca<-data_test[,-13]

##PCA of testing set
data_test_pca<-data_test[,-13]
wage.pca2 <- princomp(data_test_pca[,2:15], cor = TRUE) 
pc_wage2=as.data.frame(wage.pca2$scores[,1:3])
pc_wage2$lwage=data_test_pca$lwage

#Use PCA scores to make linear regression
lm_pc=lm(lwage ~ Comp.1+Comp.2+Comp.3, data=pc_wage)
summary(lm_pc)
plot(lm_pc,which=1:2)

#Predictions and MSE
linear_se = (predict(RawModel,data_test) - data.frame(data_test[,17]))^2
sapply(linear_se, mean, na.rm = TRUE)
pca_se = (predict(lm_pc,pc_wage2) - data.frame(pc_wage2$lwage))^2
sapply(pca_se, mean, na.rm = TRUE)

