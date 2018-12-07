library(MASS)
library(ISLR)

###Simple Linear Regression 
names(Boston)
## Median Housing Value - medv

plot(medv~lstat, data =Boston)
fit1 = lm(medv~lstat, data = Boston)
fit1
summary(fit1)
abline(fit1, col = "red")
confint(fit1) # 95% confidence interval
predict(fit1, data.frame(lstat = c(5,10,15)), interval = "confidence")

### Multiple Linear Regression

fit2 = lm(medv ~ lstat + age, data = Boston)
fit2
summary(fit2) # R2 higher the better, pernecentage of variance explained

fit3 = lm(medv ~ ., data = Boston)
fit3
summary(fit3)
par(mfrow = c(2,2))
#plot(fit3)
fit4 = update(fit3, ~. -age -indus)
summary(fit4)


### Non linear terms and interactions
fit5 = lm(medv ~ lstat*age, Boston)
summary(fit5)
plot(fit5)

fit6 = lm(medv~lstat + I(lstat^2),Boston)
summary(fit6)
attach(Boston)
par(mfrow = c(1,1))
plot(medv~lstat)
points(lstat, fitted(fit6), col="red", pch = 20) # fitted values from the model, plotting character to be 20
## Easier way of fitting polynomial functions

fit7 = lm(medv~poly(lstat,4))
points(lstat, filled(fit7), col = "yellow", pch = 20)
plot(1:20, 1:20, pch = 1:20, cex=2)

#Qualitative Predictors 
fix(Carseats)
summary(Carseats)    
fit8 = lm(Sales~.+Income:Advertising+Age:Price, Carseats)
summary(fit8)
plot(fit8)
contrasts(Carseats$ShelveLoc)
regplot=function(x,y){
  fit = lm(y~x)
  plot(x,y)
  abline(fit,col = "red")
}

attach(Carseats)
regplot(Price, Sales)

regplot(x,y,...){ # ... means unnamed arguments
  fit = lm(y~x)
  plot(x,y, ...)
  abline(fit, col = "red")
}

regplot(Price, Sales, xlab = "Price", ylab = "Sales", col = "blue", pch = 20)
