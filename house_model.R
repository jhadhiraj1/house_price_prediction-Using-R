read.csv("houses.csv")->houses
str(houses)

#preprocessing the data
library(dplyr)
houses %>% select(c(-1,-2)) ->houses
houses$air_cond<-factor(houses$air_cond,labels=c("no","yes"))
houses$construction<-factor(houses$construction,labels=c("no","yes"))
houses$waterfront<-factor(houses$waterfront,labels=c("no","yes"))
houses$fuel<-factor(houses$fuel,labels=c("Gas","Electric","Oil"))
houses$sewer<-factor(houses$sewer,labels=c("Public","Private","None"))
houses$heat<-factor(houses$heat,labels=c("Hot Air","Hot Water","Electric"))

#Visualizing the data
library(ggplot2)
ggplot(data=houses,aes(x=price)) +geom_histogram(bins=40)
ggplot(data=houses,aes(x=price)) +geom_histogram(bins=40,fill="lightblue",col="blue")
ggplot(data=houses,aes(y=price,x=waterfront,fill=waterfront)) +geom_boxplot()
ggplot(data=houses,aes(y=price,x=air_cond,fill=air_cond)) +geom_boxplot()
ggplot(data=houses,aes(x=living_area,y=price,fill=living_area)) +geom_boxplot()+geom_smooth(method="lm",se=F)+geom_point(col="tomato")
ggplot(data=houses,aes(x=age,y=price)) +geom_point(col="purple") +geom_smooth(method="lm",se=F)

#Modelling the House DataSet
library(caTools)
sample.split(houses$price,SplitRatio=0.65)->splitindex
train=subset(houses,splitindex==T)
test=subset(houses,splitindex==F)
nrow(train)
nrow(test)

#Model Building
mod1<-lm(price~.,data=train)
predict(mod1,test)->result
cbind(actual=test$price,predicted=result)->compare_result
as.data.frame((compare_result))->compare_result
compare_result$actual-compare_result$predicted->error
cbind(compare_result,error)->compare_result
sqrt(mean(compare_result$error^2))->remse1
summary(mod1)

##Increasing Accuracy with removing less impacted dependent Variables

mod2=lm(price~.-fireplaces-sewer-fuel,data=train)
predict(mod2,test)->result2
cbind(actual=test$price,predicted=result2)->compare_result2
as.data.frame(compare_result2)->compare_result2
compare_result$actual-compare_result$predicted->error2
cbind(compare_result2,error2)->compare_result2
sqrt(mean(compare_result2$error2^2))->rmse2
summary(mod2)
