#BEFORE STARTING OUR CLASS, PLEASE RUN THIS SNIPPET TO INSTALL/LOAD OUR LIBRARIES NEEDED FOR THIS LESSON

packages_needed <- c('car', 'MASS', 'lmtest', 'DAAG')
for (package_name in packages_needed) {      
  if (!(package_name %in% rownames(installed.packages()))){
    install.packages(package_name)
  }
}
for (i in 1:length(packages_needed))
{
  library(packages_needed[[i]],character.only=TRUE)
}

#COLUMNS OF THE DATASET
#1. Manufacturer, represented by its first initial: G=General Mills, K=Kelloggs, N=Nabisco, P=Post, Q=Quaker Oats, R=Ralston Purina.
#2. calories: number of calories in one portion.
#3. protein: grams of protein in one portion.
#4. fat: grams of fat in one portion.
#5. sodium: milligrams of sodium in one portion.
#6. fibre: grams of dietary fibre in one portion.
#7. carbo: grams of complex carbohydrates in one portion.
#8. sugars: grams of sugars in one portion.
#9. shelf: display shelf (1, 2, or 3, counting from the floor).
#10. potassium: grams of potassium.
#11. vitamins: vitamins and minerals (none, enriched, or 100%).

#load the dataset
cereal <- read.csv('Healthy_breakfast.csv', header = T)

#attach colnames
attach(cereal)

#We will try predict whether the calories are affected by sugar/ fat content etc.

#calories are our response, lets check briefly for normality
hist(calories)
#check the transformed data 
hist(log(calories))

################################################################################################

#We will do the simple linear regression with different predictors, then we'll do a multiple regression
#I'll skip the assumption checking for the single, but we will do for all of the multiple

#FIT THE MODEL FOR THE SUGAR PREDICTOR
simp.fit1 <- lm(calories ~ sugars,data = cereal)

#plot of the fit against the values
plot(sugars,calories)
abline(simp.fit1, lwd = 3, col = "red")


#lets check the p values of the t-statistic, the F-score and the R^2 score
summary(simp.fit1)

#RSS 
summary(simp.fit1)$sigma

#divide the plot line
par(mfrow=c(2,2))
plot(simp.fit1) #check for some outliers and normality

#plotting the residuals against the fitted values
plot(predict(simp.fit1), residuals(simp.fit1), las = 1, xlab = "Fitted values", ylab = "Residuals")

#plotting the studentized data against the residuals
plot(predict(simp.fit1), rstudent(simp.fit1), las = 1, xlab = "Fitted values", ylab = "Studentized Residuals" )

# compute the leverage to see if we can remove outliers
plot(hatvalues(simp.fit1), ylab = "Leverage")
which.max(hatvalues(simp.fit1)) #show the observation of the largest leverage statistic


#FIT THE MODEL FOR THE FAT CONTENT PREDICTOR

simp.fit2 <- lm(calories ~ fat,data = cereal)


#lets check the p values of the t-statistic, the F-score and the R^2 score
summary(simp.fit2)

#RSS 
summary(simp.fit2)$sigma


#plot of the fit against the values
plot(fat,calories)
abline(simp.fit2, lwd = 3, col = "red")

#divide the plot line
par(mfrow=c(2,2))
plot(simp.fit2) #check for some outliers and normality

#plotting the residuals against the fitted values
plot(predict(simp.fit2), residuals(simp.fit2), las = 1, xlab = "Fitted values", ylab = "Residuals")

#plotting the studentized data against the residuals
plot(predict(simp.fit2), rstudent(simp.fit2), las = 1, xlab = "Fitted values", ylab = "Studentized Residuals" )

# compute the leverage to see if we can remove outliers
plot(hatvalues(simp.fit2), ylab = "Leverage")
which.max(hatvalues(simp.fit2)) #show the observation of the largest leverage statistic

################################################################################################

#BOTH PREDICTORS HAD AN EFFECT ON THE RESPONSE, HOWEVER THEY DID NOT EXPLAIN THE RELATIONSHIP VERY WELL (LOW R^2+ RSS AND PLOT EVIDENCE),
#IF WE INCLUDE BOTH IN THE MODEL, COULD WE EXPLAIN IT BETTER?

#P.S: SINCE WE HAVE MORE THAN 1 PREDICTOR, WE CANNOT PLOT THE INDIVIDUAL VARIABLES, SO WE PLOT THE RESIDUALS AGAINST THE FITTED VALUES

#fit the model for the multiple linear regression using both predictors
multi.fit1 <- lm(calories ~ sugars + fat,data = cereal)

#lets investigate the F and t statistic
summary(multi.fit1) #we had a gain in the indicators, both coeffs are significant (t-statistic) and F-score is high enough

#lets see the plot of residuals against fitted
par(mfrow = c(2,2))
plot(multi.fit1)

#leverage
plot(hatvalues(multi.fit1))
which.max(hatvalues(multi.fit1))

#confidence intervals
confint(multi.fit1)

#NORMALITY, OUTLIERS AND HETEROSCEDASCITY (KINDA) WERE ALREADY CHECKED IN THE PLOT

#ASSESSING HETEROSCEDASCITY

# non-constant error variance test
ncvTest(multi.fit1)
# plot studentized residuals vs. fitted values 
spreadLevelPlot(multi.fit1)
#breusch pagan test
bptest(multi.fit1)

# ASSESSING COLLINEARITY
#we first can check the correlation matrix of the predictors
cor(cereal[,c(4,8)])

vif(multi.fit1) # variance inflation factors 
sqrt(vif(multi.fit1)) > 2 # problem? cutoff is 5 or 10

# ASSSESING NONLINEARITY
# component + residual plot 
crPlots(multi.fit1)
# Ceres plots (for multivariate) 
#ceresPlots(fit1)

#TESTING FOR INDEPENDENCE (AUTOCORRELATED ERRORS)

#plot the residuals as a function of time
x<-1:length(residuals(multi.fit1))
y<-residuals(multi.fit1)
plot(x,y, xlab = "time", ylab = 'Residuals')
s <- seq(length(x)-1)
segments(x[s], y[s], x[s+1], y[s+1], col= 'red')

#durbin watson test
dwtest(multi.fit1) #positive autocorrelation / unreliable

#breusch-godfrey test : H_0 test statistic is assymptotically chi-squared distrib.
bgtest(multi.fit1)

#check the coefficients
coeftest(bgtest(multi.fit1))

#check the bic and aic
BIC(multi.fit1)
AIC(multi.fit1)

##########################################################################################################################################

#Removing the additive assumption and adding interaction terms

#fit the model for the multiple linear regression using both predictors and an interaction term
multi.fit2 <- lm(calories ~ sugars + fat + sugars:fat,data = cereal)

#the same thing can be achieved writing
multi.fit2 <- lm(calories ~ sugars * fat,data = cereal)

#details of the fit
summary(multi.fit2)

#plot of the fit
par(mfrow = c(2,2))
plot(multi.fit2)

BIC(multi.fit2)
AIC(multi.fit2)

##########################################################################################################################################

#using dummy variables in our linear model

#we'll use the vitamins together with sugars

#contrasts of the vitamins
contrasts(vitamins)

multi.fit3 <- lm(calories ~ sugars + vitamins ,data = cereal)

#summary of multifit
summary(multi.fit3)

#lets see the plot of residuals against fitted
par(mfrow = c(2,2))
plot(multi.fit3)

#leverage
plot(hatvalues(multi.fit3))
which.max(hatvalues(multi.fit3))

#confidence intervals
confint(multi.fit3)

#BIC AND AIC
BIC(multi.fit3)
AIC(multi.fit3)


#if we put an interaction term, we see that it worsens the fit
multi.fit4 <- lm(calories ~ sugars + vitamins + sugars:vitamins  ,data = cereal)

summary(multi.fit4)

#lets see the plot of residuals against fitted
par(mfrow = c(2,2))
plot(multi.fit4)

BIC(multi.fit4)
AIC(multi.fit4)


##########################################################################################################################################


#say we want to do a regression on all values but shelf, mfr and vitamins
multi.fit.all = lm(calories~.-shelf -mfr - vitamins, data =cereal)

summary(multi.fit.all)

#lets see the plot of residuals against fitted
par(mfrow = c(2,2))
plot(multi.fit.all)

#confidence intervals
confint(multi.fit.all)

#NORMALITY, OUTLIERS AND HETEROSCEDASCITY (KINDA) WERE ALREADY CHECKED IN THE PLOT

#ASSESSING HETEROSCEDASCITY

# non-constant error variance test
ncvTest(multi.fit.all)
# plot studentized residuals vs. fitted values 
spreadLevelPlot(multi.fit.all)
#breusch pagan test
bptest(multi.fit.all)

# ASSESSING COLLINEARITY
#we first can check the correlation matrix of the predictors
cor(cereal[,c(3,4,5,6,7,8,10)])

vif(multi.fit.all) # variance inflation factors 
sqrt(vif(multi.fit.all)) > 2 # problem? cutoff is 5 or 10

# ASSSESING NONLINEARITY
# component + residual plot 
crPlots(multi.fit.all)
# Ceres plots (for multivariate) 
ceresPlots(multi.fit.all)

#TESTING FOR INDEPENDENCE (AUTOCORRELATED ERRORS)

#plot the residuals as a function of time
x<-1:length(residuals(multi.fit.all))
y<-residuals(multi.fit.all)
plot(x,y, xlab = "time", ylab = 'Residuals')
s <- seq(length(x)-1)
segments(x[s], y[s], x[s+1], y[s+1], col= 'red')

#durbin watson test
dwtest(multi.fit.all) #positive autocorrelation / unreliable

#breusch-godfrey test : H_0 test statistic is assymptotically chi-squared distrib.
bgtest(multi.fit.all)

#check the coefficients
coeftest(bgtest(multi.fit.all))

#GOODNESS OF FIT

#check the bic and aic
BIC(multi.fit.all)
AIC(multi.fit.all)

#Research question no. 2 
#Is there a significant difference between the calories and fat content of cereals placed on the different shelves?
#Based on these results can you say if healthier cereals tend to be displayed on the upper or lower shelves?