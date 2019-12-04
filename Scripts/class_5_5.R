#BEFORE STARTING OUR CLASS, PLEASE RUN THIS SNIPPET TO INSTALL/LOAD OUR LIBRARIES NEEDED FOR THIS LESSON

packages_needed <- c('car', 'MASS', 'ISLR')
for (package_name in packages_needed) {      
  if (!(package_name %in% rownames(installed.packages()))){
    install.packages(package_name)
  }
}
for (i in 1:length(packages_needed))
{
  library(packages_needed[[i]],character.only=TRUE)
}


#summary of the data
summary(Boston)

#names of the cols
names(Boston)
attach(Boston)

#Response (Y): medv ("median value")
#Predictors (X): the rest

#simple linear regression with medv as Y and lstat as X
#we saved in a var called lm.fit
lm.fit = lm(medv~lstat)

#for additional statistics (R^2 and F use summary)
summary(lm.fit)
#additional pieces of info
names(lm.fit)
#obtaining the coefficients
coef(lm.fit)
#confidence interval
confint(lm.fit)

#produce confidence intervals and prediction of intervals for the prediction
#of medv for a given value of lstat

predict(lm.fit, data.frame(lstat=c(5,10,15)), interval = "confidence")
predict(lm.fit, data.frame(lstat=c(5,10,15)), interval = "prediction")

#plotting with te absolute line
plot(lstat,medv)
abline(lm.fit, lwd = 3, col = "red")

plot(lm.fit)


#divide the plot line
par(mfrow=c(2,2))
plot(lm.fit)

#plotting the residuals against the fitted values
plot(predict(lm.fit), residuals(lm.fit), las = 1, xlab = "Fitted values", ylab = "Residuals")

#plotting the studentized data against the residuals
plot(predict(lm.fit), rstudent(lm.fit), las = 1, xlab = "Fitted values", ylab = "Studentized Residuals" )

#as we can see some evidence of nonlinearity, we compute the leverage 
plot(hatvalues(lm.fit), ylab = "Leverage")
which.max(hatvalues(lm.fit)) #375 is the observation of the largest leverage statistic

#MULTIPLE LINEAR REGRESSION
#linear regression with 2 variables
lm.fit = lm(medv~lstat+age,data = Boston)
summary(lm.fit)

#linear regression with all variables
lm.fit = lm(medv~.,data=Boston)
summary(lm.fit)
#summary of individual items (r squared)
summary(lm.fit)$r.sq
#summary of individual items (RSS)
summary(lm.fit)$sigma

#COMPUTING THE VIF
vif(lm.fit)

#say we want to do a regression on all values but age
lm.fit1 = lm(medv~.-age, data =Boston)
summary(lm.fit1)
#alternatively we use update
lm.fit1 = update(lm.fit,~.-age)

#Interaction Terms
summary(lm(medv~lstat*age,data = Boston))

#_--------------------------------------------------------------------#

#Non-linear transformations of the predictors

lm.fit1 = lm(medv~lstat)
summary(lm.fit1)
plot(lm.fit1)

#performing regression on lstat and lstat^2 using the I function call
lm.fit2 = lm(medv~lstat+I(lstat^2))
summary(lm.fit2)
plot(lm.fit2)

#quantifying how much the quadratic fit is superior to the linear fit using ANOVA
anova(lm.fit1,lm.fit2)

#observing the nonlinearity between medv and lstat
par (mfrow = c(2,2))
plot(lm.fit2)

#doing linear regression with higher order polynomials using the poly() function
lm.fit5 = lm(medv~poly(lstat,5))
summary(lm.fit5)
par (mfrow = c(2,2))
plot(lm.fit5)

#log transformation to the regression
summary(lm(medv~log(rm),data=Boston))


#QUALITATIVE PREDICTORS
names(Carseats)
attach(Carseats)

#the : stands for interaction terms between the predictors
lm.fit = lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
summary(lm.fit)
#get the dummy vars
contrasts(ShelveLoc)
