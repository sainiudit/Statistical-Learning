require(ISLR)
require(boot)

?cv.glm
plot(mpg~horsepower, data = Auto)

# LOOCV (you had to fit the model n times, n = number of traning points)

glm.fit = glm(mpg~horsepower, data = Auto) # by default fits a linear model
cv.glm(Auto, glm.fit)$delta # quite slow

#function to utlise our formula
loocv = function(fit){
  h = lm.influence(fit)$h# extract element H from the formula
  mean((residuals(fit)/(1-h))^2)
}
loocv(glm.fit) # A much faster methodology instead of the formula used above

cv.error = rep(0,5)
degree = 1:5
for(d in degree)
{
  glm.fit = glm(mpg~poly(horsepower,d), data = Auto)
  cv.error[d] = loocv(glm.fit)
}

plot(degree,cv.error, type ="b")

## 10-fold cross validation (need to only compute 9 points)
## more stable measure than LOOCV and cheaper to compute
cv.error10 = rep(0,5)
for(d in degree)
{
  glm.fit = glm(mpg~poly(horsepower,d), data = Auto)
  cv.error10[d] = cv.glm(Auto, glm.fit, K = 10)$delta[1]
}

lines(degree,cv.error10, type = "b", col = "red")

