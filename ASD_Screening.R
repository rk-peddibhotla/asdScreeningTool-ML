library(e1071) 
library(caTools) 
library(rpart.plot) 
library(caret) 
library(ggplot2) 
library(psych) 
library(plyr) 
library(dplyr) 
library(pander) 
mydata<-read.csv("autism12345.csv")  
str(mydata) 
anyNA(mydata) #handle missing values 
mydata[!complete.cases(mydata),] #list out missing values 
newdata <- na.omit(mydata) #prepare a new data 
anyNA(newdata) #checks for Na values 
newdata 
boxplot(newdata$result,newdata$ASD) 
plot(newdata$age,newdata$ASD) 
class(newdata$age) 
class(newdata$result) 
library(pander) 
c1 <- newdata %>% count(result >= 7) 
c2 <- newdata %>% count(ASD == "YES") 
comp_c1_c2 <- cbind(c1,c2) 
comp_c1_c2 
pander(comp_c1_c2) 
sample=sample.split(newdata$ASD,SplitRatio = 0.95) 
train=subset(newdata,sample==TRUE) 
test=subset(newdata,sample==FALSE) 
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3) 
set.seed(3333) 
pairs(newdata[c("ASD","age","A1_Score","A2_Score","A3_Score","A4_Score","A5_Score",
 "A6_Score","A7_Score","A8_Score","A9_Score","A10_Score")]) 
pairs.panels(newdata[c("ASD","age","A1_Score","A2_Score","A3_Score","A4_Score","A5_
 Score","A6_Score","A7_Score","A8_Score","A9_Score","A10_Score")]) 
prop.table(table(train$age)) 
fit <- rpart(ASD~., data = test, method = 'class') 
rpart.plot(fit,trace=-1,box.col=c("green", "red")) 
predict(fit,test, type = 'class') 
summary(fit) 
confusionMatrix(newdata$ASD,newdata$jundice) 
confusionMatrix(newdata$ASD,newdata$austim) 
NBclassfier=naiveBayes(ASD~., data=test) 
print(NBclassfier)