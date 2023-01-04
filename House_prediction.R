Data<-read.csv("House_Price.csv")
View((Data))
head(Data,8)
#Checking for NA values
colSums(is.na(Data))
#visualization
summary(Data)
str(Data)
cor(Data)
#libraries
library(tidyverse)
library(GGally)
library(ggplot2)
library(gridExtra)
library(MLmetrics)
library(car)
library(lmtest)

pairs(Data)
# high correlation to data target House price of unit area
ggcorr(Data, label = T, hjust = 1, layout.exp = 3)
#one by one (mainly spot)
pairs(~House.price.of.unit.area +Distance.from.nearest.Metro.station..km. , data = Data,
      main = "Scatterplot ")
pairs(~House.price.of.unit.area +Number.of.convenience.stores , data = Data,
      main = "Scatterplot ")
pairs(~House.price.of.unit.area +Number.of.bedrooms , data = Data,
      main = "Scatterplot ")
pairs(~House.price.of.unit.area +House.size..sqft. , data = Data,
      main = "Scatterplot ")
pairs(~House.price.of.unit.area +House.Age , data = Data,
      main = "Scatterplot ")
#\for better View and visualization
qplot(Data$House.size..sqft.,Data$House.price.of.unit.area,colour=Data$House.price.of.unit.area,
      xlab ="House size sqft", ylab="price()")
qplot(Data$House.Age,Data$House.price.of.unit.area,colour=Data$House.price.of.unit.area,
      xlab ="House Age", ylab="price()")
qplot(Data$Distance.from.nearest.Metro.station..km.,Data$House.price.of.unit.area,colour=Data$House.price.of.unit.area,
      xlab ="Distance from nearest Metro", ylab="price()")
qplot(Data$Number.of.convenience.stores,Data$House.price.of.unit.area,colour=Data$House.price.of.unit.area,
      xlab ="No of convenience Stores", ylab="price()")
qplot(Data$Distance.from.nearest.Metro.station..km.,Data$Number.of.convenience.stores,colour=Data$House.price.of.unit.area,
      xlab ="metro distance", ylab="No of Convenience stores")
#boxplot for checking outliers
boxplot(Data$Number.of.bedrooms, main="Bedrooms")
boxplot(Data$House.Age, main="Houseage")
boxplot(Data$Distance.from.nearest.Metro.station..km., main="Distance from Metro station")
boxplot(Data$Number.of.convenience.stores, main="Stores")
boxplot(Data$House.size..sqft., main="House size sqft")
boxplot(Data$House.price.of.unit.area, main="House Price")
#Scatterplot
g <- ggplot(Data, aes(Number.of.bedrooms,House.size..sqft.))
g + geom_count(col="red", show.legend=F) +
  labs(y="House size sqft", 
       x="bedrooms", 
       title="Bedrooms vs House size")
#density plot to check normality
plot(density(Data$Number.of.bedrooms), main="Density Plot: Bedrooms", ylab="Frequency",
           sub=paste("Skewness:", round(e1071::skewness(Data$Number.of.bedrooms), 2))) 
polygon(density(Data$Number.of.bedrooms), col="green")
plot(density(Data$House.Age), main="Density Plot: House Age", ylab="Frequency",
     sub=paste("Skewness:", round(e1071::skewness(Data$House.Age), 2))) 
polygon(density(Data$House.Age), col="yellow")
plot(density(Data$Distance.from.nearest.Metro.station..km.), main="Density Plot: Metro distance", ylab="Frequency",
     sub=paste("Skewness:", round(e1071::skewness(Data$Distance.from.nearest.Metro.station..km.), 2))) 
polygon(density(Data$Distance.from.nearest.Metro.station..km.), col="yellow")
plot(density(Data$Number.of.convenience.stores), main="Density Plot: Convenience stores", ylab="Frequency",
     sub=paste("Skewness:", round(e1071::skewness(Data$Number.of.convenience.stores), 2))) 
polygon(density(Data$Number.of.convenience.stores), col="yellow")
plot(density(Data$House.size..sqft.), main="Density Plot: House size sqft", ylab="Frequency",
     sub=paste("Skewness:", round(e1071::skewness(Data$House.size..sqft.), 2))) 
polygon(density(Data$House.size..sqft.), col="yellow")
#Plot  linear regression between House price of unit area and House size sqft
ggplot(Data,aes(y=House.price.of.unit.area,x=House.size..sqft.)) +
  geom_point() + 
  
  geom_smooth(formula = y ~ x,method="lm")
#


#Partition
set.seed(417)
a<- sample(nrow(Data), nrow(Data)* 0.7)
traindata <- Data[a,]
testdata <- Data[-a,]
ggcorr(traindata, label = T, hjust = 1, layout.exp = 3)
# removed transaction date. It has no effect
model_all <- lm(House.price.of.unit.area ~ Distance.from.nearest.Metro.station..km. +latitude+longitude +House.Age
                   + House.size..sqft. +Number.of.bedrooms+Number.of.convenience.stores, data = traindata)
summary(model_all)
plot(model_all)
#our pridiction
ggplot(traindata, aes(x=House.size..sqft., y=House.price.of.unit.area)) +
  geom_point(alpha=1, colour='blue') +
  geom_abline(intercept=model_all$coefficient[1], slope=model_all$coefficients[2],colour="red")
#
ggplot(testdata, aes(x=House.size..sqft., y=House.price.of.unit.area)) +
  geom_point(alpha=1, colour='blue') +
  geom_abline(intercept=model_all$coefficient[1], slope=model_all$coefficients[2],colour="red")
#Next we will make predictions into the test data with the model we have created model_all
testdata$pred <- predict(model_all, testdata)

hist(testdata$pred)
#Model Performance Testing

RMSE(y_pred = testdata$pred, y_true = testdata$House.price.of.unit.area)
#RMSE result quit small, it's mean the model has enough good to predict.
hist(model_all$residuals)
#Performance
shapiro.test(x = model_all$residuals[3:40])
# Model has normal distribution and p-value < 0.05
plot(model_all$fitted.values, model_all$residuals)
abline(h = 0, col = "red")
#for checking homoscedasticity
bptest(model_all)
#homoscedasticity is not present means we are good

#VIF measures how much the variance of an estimated regression coefficient increases if your predictors are correlated
vif(model_all)
#There is no multicultural because there is no VIF value more than 10

#conclusion--From the results of the tests that have been carried out, the model is good to have passed the test.









































































