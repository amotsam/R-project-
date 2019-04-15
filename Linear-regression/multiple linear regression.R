Multiple Linear Regression

## load the data "kuiper.csv" from your working directory.
MyData <- read.csv("kuiper.csv")

## knowing your data.
head(MyData)
summary(MyData)

## Fitting a simple linear regression model. The DV is Price and the IV is Mileage.
options(scipen =999)
fit1 <- lm(Price ~ Mileage, data = MyData)
summary(fit1)

# scatterplot between Mileage (x axis) vs. car's price (y axis).
plot(MyData$Mileage,MyData$Price,
     xlab="Mileage",
     ylab="Price",
     bg="lightblue",
     col="black",pch=21,frame=FALSE)
abline(lm(Price ~ Mileage, data = MyData),lwd=2, col="red")

## Fitting a Multiple linear regression model 
# the DV is Price , The IV's are: Mileage, Cylinder, Doors, Cruise, Sound, Leather.

fit2 <- lm(Price~Mileage + Liter+ Doors + Cruise + Sound + Leather, data = MyData)
summary(fit2)

fit3 <- lm(Price~Mileage + Liter+ Doors + Cruise + Sound + Leather, data = MyData)


## Variable importance 
# install.packages("caret")
library(caret)
varImp(fit2, scale = FALSE)
varImp(fit3, scale = FALSE)

## Colinearity

fitM1 <- lm(Price ~ Mileage+Liter, data = MyData)
fitM2 <- lm(Price ~ Mileage+Cylinder, data = MyData)
fitM3 <- lm(Price ~ Mileage+Liter+Cylinder, data = MyData)
summary(fitM1)
summary(fitM2)
summary(fitM3)
cor(MyData$Cylinder,MyData$Liter)

## Including categorical variables in LR

fit4 <- lm(Price ~ Mileage + Liter + Make, data=MyData)
summary(fit4)


# Which model is best?
# RMSE (Root Mean Squared Error) ==> a numerical measure of predictive accuracy
# The prediction error for observation i is defined as the difference between
# its actual y value and its predicted y value: ei = yi - y(predicted)

MyData$Sample <- runif(804, min = 0, max = 1)
Training <- subset(MyData, MyData$Sample > 0.3)
Test <- subset(MyData, MyData$Sample <= 0.3)

fit11 <- lm(Price ~ Mileage, data = Training)
Test$Prediction <- predict(fit11,newdata=Test)
RMSE1 <- sqrt(mean((Test$Prediction-Test$Price)^2))

fit22 <-lm(Price~Mileage + Cylinder + Doors + Cruise + Sound + Leather,
           data = Training)
Test$Prediction <- predict(fit22,newdata=Test)
RMSE2 <- sqrt(mean((Test$Prediction-Test$Price)^2))

fit33 <- lm(Price ~ Mileage+Liter+Cylinder, data = Training)
Test$Prediction <- predict(fit33,newdata=Test)
RMSE3 <- sqrt(mean((Test$Prediction-Test$Price)^2))


fit44 <- lm(Price ~ Mileage + Liter + Make, data=Training)
Test$Prediction <- predict(fit44,newdata=Test)
RMSE4 <- sqrt(mean((Test$Prediction-Test$Price)^2))


RMSE1
RMSE2
RMSE3
RMSE4



####################################################################################

# Log transfomed model
MyData$TPrice<-log(MyData$Price)
hist(MyData$TPrice, col="lightblue")
fit5 <- lm(TPrice~Mileage + Cylinder + Doors +Cruise + Sound + Leather, data = MyData)
par(mfrow = c(1,2))
plot(fit5, which = 2)
sresid <- studres(fit5) 
hist(sresid, freq=FALSE,main="Distribution of Studentized Residuals", cex.main=0.7)
summary(fit5)
par(mfrow = c(1,2))
plot(fit2, which = 1)
plot(fit5, which = 1)
par(mfrow = c(1,1))

#####################################################################################

## Testing model assumptions
# Normality of Residuals
# qq plot for studentized resid
# four diagnostic plots for LR.
par(mfrow = c(1,2))
plot(fit2, which = 2)
plot(fit2, which = 1)
hist(fit2$res)

# install.packages("MASS")
# Plotting Regression Residuals
library(MASS)
sresid <- studres(fit2) 
hist(sresid, freq=FALSE,main="Distribution of Studentized Residuals")

#####################################################################################

