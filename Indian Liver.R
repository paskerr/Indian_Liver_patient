#updated2#

#Loading required packages####
if (!require(dplyr)) install.packages('dplyr')
if (!require(car)) install.packages('car')
if (!require(caret)) install.packages('caret')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(purrr)) install.packages('purrr')
library(dplyr)
library(car)
library(caret)
library(tidyverse)
library(purrr)



#Loading data from url and naming the columns.

data=read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/00225/Indian%20Liver%20Patient%20Dataset%20(ILPD).csv", header=FALSE)

names(data)=c("Age","Gender","Total_Bilirubin","Direct_Bilirubin","Alkaline_Phosphotase","Alamine_Aminotransferase",
              "Aspartate_Aminotransferase","Total_Protiens","Albumin","Albumin_and_Globulin_Ratio","Disease") 

summary(data)
str(data)
sum(is.na(data)) # 4
data=na.omit(data)

## I assigned '0' for the patients who don't liver cancer.
data$Disease[data$Disease==2]=0


# Data Visualization ####

##Scatterplots of each variable in the dataset.####
attach(data)
par(mfrow=c(3,3))
plot(Age)
plot(Total_Bilirubin)
plot(Alkaline_Phosphotase)
plot(Alamine_Aminotransferase)
plot(Total_Protiens)
plot(Albumin)
plot(Albumin_and_Globulin_Ratio)
plot(Direct_Bilirubin)
plot(Aspartate_Aminotransferase)




##female liver-patient ratio and male liver-patient ratio####
data%>%group_by(Gender,Disease)%>%summarise(n=n())
female_ratio=as.numeric(91/(49+91))
male_ratio=as.numeric(323/(116+323))
ratio<-c(female_ratio,male_ratio)
names(ratio)<-c("female_ratio","male_ratio")
barplot(ratio)


## The Effect of Age on Liver Disease
ggplot(data, aes(x=as.factor(Disease),y=Age))+geom_boxplot(fill="blue",alpha=0.2)+xlab("Disease")



#Data Analysis####

##Partition data####
set.seed(1)
test_index<-createDataPartition(data$Disease,times=1,p=0.3,list=F)
train_data<-data %>% slice(-test_index)
test_data<-data %>% slice(test_index)
dim(train_data)
dim(test_data)



##Logistic Regression Development####
glm_fit_1<-glm(Disease~.,train_data, family=binomial)
summary(glm_fit_1)
p_hat_glm_1<-predict(glm_fit_1,newdata=test_data,type="response")
y_hat_glm_1<-ifelse(p_hat_glm_1<0.5,0,1)
cm1<- confusionMatrix(as.factor(y_hat_glm_1),as.factor(test_data$Disease))
cm1$table
results=tibble(Model="glm_fit_1", Accuracy=cm1$overall["Accuracy"], 
Sensitivity=cm1$byClass["Sensitivity"], Specificity= cm1$byClass["Specificity"])
results%>%knitr::kable()


###Collinearity diagnostics and variable selection####
vif(glm_fit_1)
pairs(data[,-2])





###Dropping the correlated predictor variables from the model.  
glm_fit_2<-glm(Disease~Age+Direct_Bilirubin+Alkaline_Phosphotase+
                 Alamine_Aminotransferase+Aspartate_Aminotransferase+
                 Total_Protiens+Albumin_and_Globulin_Ratio,train_data,family=binomial)

summary(glm_fit_2)
p_hat_glm_2<-predict(glm_fit_2,newdata=test_data,type="response")
y_hat_glm_2<-ifelse(p_hat_glm_2<0.5,0,1)
cm2<- confusionMatrix(as.factor(y_hat_glm_2),as.factor(test_data$Disease))
cm2$table
results=tibble(Model=c("glm_fit_1","glm_fit_2"), Accuracy=c(cm1$overall["Accuracy"],cm2$overall["Accuracy"]), Sensitivity=c(cm1$byClass["Sensitivity"],cm2$byClass["Sensitivity"]), Specificity=c(cm1$byClass["Specificity"],cm2$byClass["Specificity"]))
results%>%knitr::kable() 




### Removing the insignificant variables from the model according to the p- values and rebuild the model.
glm_fit_3 <- glm(Disease~Age+Direct_Bilirubin+Alkaline_Phosphotase, train_data, 
family=binomial)
summary(glm_fit_3)
p_hat_glm_3<-predict(glm_fit_3,newdata=test_data,type="response")
y_hat_glm_3<-ifelse(p_hat_glm_3<0.5,0,1)
cm3<- confusionMatrix(as.factor(y_hat_glm_3),as.factor(test_data$Disease))
cm3$table
results=tibble(Model=c("glm_fit_1","glm_fit_2","glm_fit_3"), Accuracy=c(cm1$overall["Accuracy"],
cm2$overall["Accuracy"],cm3$overall["Accuracy"]),Sensitivity=c(cm1$byClass["Sensitivity"],cm2$byClass["Sensitivity"],
cm3$byClass["Sensitivity"]), Specificity=c(cm1$byClass["Specificity"],cm2$byClass["Specificity"],cm3$byClass["Specificity"]))
results%>%knitr::kable()



### Determination of the best probability cutoff value for logistic regression.####
cutoff<- seq(0.5,0.75,length=20)

log_reg_accuracy<-map_df(cutoff,function(c){
  
  set.seed(1)
  p_hat_lg<-predict(glm_fit_3,newdata=test_data,type="response")
  y_hat_lg<-ifelse(p_hat_lg< c,0,1)
  confusionMatrix(as.factor(y_hat_lg),as.factor(test_data$Disease))
  tibble(test_accuracy=confusionMatrix(as.factor(y_hat_lg),as.factor(test_data$Disease))$overall["Accuracy"], cutoff=c)
})




###graph the accuracy values corresponding cutoff value.####
log_reg_accuracy%>%ggplot()+
  geom_line(aes(cutoff,test_accuracy),color="blue")+geom_point(data=.%>%filter(test_accuracy==max(test_accuracy)),aes(cutoff,test_accuracy),color='blue',size=3)+
  geom_label(data=.%>%filter(test_accuracy==max(test_accuracy)),aes(cutoff,test_accuracy,label=sprintf('%0.2f',test_accuracy)),hjust=-0.35)+
  labs(title="Accuracy for Different Cutoff Values in Logistic Regression", y="test accuracy",x="cutoff")+
  scale_x_continuous(breaks = seq(0.5,0.75,by=0.1))



##KNN Models Development######
 
###Scaling the variables

data_standardized=scale(data[,-c(2,11)])
data_standardized=as.data.frame(data_standardized)
data_standardized$Disease=data$Disease
data_standardized$Gender=data$Gender
summary(data)
summary(data_standardized)


### Data Partitioning####

set.seed(100)
test_index_st<-createDataPartition(data_standardized$Disease,times=1,p=0.3,list=F)
train_data_st<-data_standardized %>% slice(-test_index_st)
test_data_st<-data_standardized %>% slice(test_index_st)
train_data_st$Disease=as.factor(train_data_st$Disease)
test_data_st$Disease=as.factor(test_data_st$Disease)


### KNN Method ####
set.seed(1)
knn_fit_1 <- knn3(Disease ~ ., data = train_data_st, k=5) 
y_hat_knn_1 <- predict(knn_fit_1, test_data_st, type = "class")
cm4<-confusionMatrix(y_hat_knn_1, test_data_st$Disease)
cm4$table
results=tibble(Model=c("glm_fit_1","glm_fit_2","glm_fit_3","knn_1"), Accuracy=c(cm1$overall["Accuracy"],cm2$overall["Accuracy"],cm3$overall["Accuracy"],cm4$overall["Accuracy"]), Sensitivity=c(cm1$byClass["Sensitivity"],cm2$byClass["Sensitivity"],cm3$byClass["Sensitivity"],cm4$byClass["Sensitivity"]), Specificity=c(cm1$byClass["Specificity"],cm2$byClass["Specificity"],cm3$byClass["Specificity"],cm4$byClass["Specificity"]))
results%>%knitr::kable() 


### Selecting the Optimum 'k' Value.####
ks<-seq(1,20)

accuracy <- map_df(ks, function(k){
  set.seed(1)
  fit <- knn3(Disease ~ ., data =train_data_st, k = k)
  y_hat <- predict(fit, train_data_st, type = "class") 
  cm_train <- confusionMatrix(y_hat, train_data_st$Disease) 
  train_error <- cm_train$overall["Accuracy"]
  
  y_hat <- predict(fit, test_data_st, type = "class") 
  cm_test <- confusionMatrix(y_hat, test_data_st$Disease) 
  test_error <- cm_test$overall["Accuracy"]
  tibble(train = train_error, test = test_error, k=k) })


accuracy%>%ggplot()+geom_line(aes(k,train),color="red")+
  geom_line(aes(k,test),color="blue")+geom_point(data=.%>%filter(test==max(test)),aes(k,test),color='blue',size=3)+
  geom_label(data=.%>%filter(test==max(test)),aes(k,test,label=sprintf('%0.2f',test)),vjust=-0.5)+
  labs(title="Picking the k in KNN", y="train / test accuracy",x="k")+
  scale_x_continuous(breaks = round(seq(min(accuracy$k), max(accuracy$k), by = 1),1))

#the best 'k' value is 9

###Final KNN ####
set.seed(1)
knn_fit_1 <- knn3(Disease ~ ., data = train_data_st, k=9) 
y_hat_knn_1 <- predict(knn_fit_1, test_data_st, type = "class")
cm5<-confusionMatrix(y_hat_knn_1, test_data_st$Disease)
cm5$table
results=tibble(Model=c("glm_fit_1","glm_fit_2","glm_fit_3","knn_1","knn_2"), Accuracy=c(cm1$overall["Accuracy"],cm2$overall["Accuracy"],cm3$overall["Accuracy"],cm4$overall["Accuracy"],cm5$overall["Accuracy"]), Sensitivity=c(cm1$byClass["Sensitivity"],cm2$byClass["Sensitivity"],cm3$byClass["Sensitivity"],cm4$byClass["Sensitivity"],cm5$byClass["Sensitivity"] ), Specificity=c(cm1$byClass["Specificity"],cm2$byClass["Specificity"],cm3$byClass["Specificity"],cm4$byClass["Specificity"],cm5$byClass["Specificity"]))
results%>%knitr::kable()









