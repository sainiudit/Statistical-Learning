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
plot(fit3)
fit4 = update(fit3, ~. -age -indus)
summary(fit4)


### Non linear terms and interactions

