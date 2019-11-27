#BEFORE STARTING OUR CLASS, PLEASE RUN THIS SNIPPET TO INSTALL/LOAD OUR LIBRARIES NEEDED FOR THIS LESSON

packages_needed <- c('car', 'reshape2', 'e1071', 'GGally', 'DAAG' , 'MASS', 'lmtest', 'sandwich')
for (package_name in packages_needed) {      
  if (!(package_name %in% rownames(installed.packages()))){
    install.packages(package_name)
  }
}
for (i in 1:length(packages_needed))
{
  library(packages_needed[[i]],character.only=TRUE)
}

#LINEAR REGRESSION

#load the suicide per year dataset
hwd <-read.table('hwdata.txt', header = T, skip =7)
attach(hwd)

#lets first see the columns
summary(hwd)

pairs(hwd)

str(hwd)

#for now, our model wont take time into account! (we'll take only one variable at a time)

#graphical representation of our data
scatter.smooth(x=suicide, y=unemployment, main="suicide ~ unemployment")  # scatterplot

scatter.smooth(x=suicide, y=realgdp, main="suicide ~ realgdp")  # scatterplot

#checking for any outliers using boxplots

par(mfrow=c(1, 3))  # divide graph area in 2 columns
boxplot(suicide, main="Suicide", col = 'red', sub=paste("Outlier rows: ", boxplot.stats(suicide)$out))

boxplot(unemployment, main="Unemployment", col = 'blue', sub=paste("Outlier rows: ", boxplot.stats(unemployment)$out))  

boxplot(realgdp, main="Gdp", col = 'green', sub=paste("Outlier rows: ", boxplot.stats(realgdp)$out))


#looking at normality using a density plot

par(mfrow=c(1, 3))  # divide graph area in 2 columns

plot(density(suicide), main="Density Plot: Suicide", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(suicide), 2))) 
polygon(density(suicide), col="red")

plot(density(unemployment), main="Density Plot: unemployment", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(unemployment), 2)))  
polygon(density(unemployment), col="red")

plot(density(realgdp), main="Density Plot: gdp", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(realgdp), 2)))  
polygon(density(realgdp), col="red")

#lets check the correlation
cor(suicide,unemployment)

cor(suicide, realgdp)

#if we dont have that many variables, we can use the ggpairs to show the entire analysis at once
ggpairs(data=hwd, columns=1:3, title="suicide data")


#lets build the linear model using each variable

#with unemployment
#Null hypothesis: Coeff = 0
#t-value: prob  that its not 0
fit1 <- lm(suicide ~ unemployment, data = hwd)
#summary of the regression
summary(fit1)

#we can estimate the confidence intervals for the intercept and the variable
confint(fit1)

#POST-HOC ANALYSIS OF ASSUMPTIONS

#ASSESSING OUTLIERS

outlierTest(fit1) # Bonferonni p-value for most extreme obs

leveragePlots(fit1) # leverage plots

#residual  against fitted values plot
plot(fit1, which=1, col = c('blue')) #we can see some traces of heteroscedascity

#lets try with transformed data
fit1.2 <- fit1 <- lm(sqrt(suicide) ~ unemployment, data = hwd)

#residual  against fitted values plot of the transformed response data
plot(fit1.2, which=1, col = c('blue')) 


#ASSESSING NORMALITY

#qqplot of the fitted model
qqPlot(fit1, main="QQ Plot") ##qq plot of studentized residuals


#histogram of the studentized residuals
hist(studres(fit1), freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(studres(fit1)),max(studres(fit1)),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

#ASSESSING HOMOSCEDASCITY

# non-constant error variance test
ncvTest(fit1)
# plot studentized residuals vs. fitted values 
spreadLevelPlot(fit1)
#breusch pagan test
bptest(fit1)


# ASSESSING COLLINEARITY
vif(fit1) # variance inflation factors 
sqrt(vif(fit1)) > 2 # problem?

# ASSSESING NONLINEARITY
# component + residual plot 
crPlots(fit1)
# Ceres plots (for multivariate) 
#ceresPlots(fit1)

#TESTING FOR INDEPENDENCE (AUTOCORRELATED ERRORS)
dwtest(fit1) #positive autocorrelation / unreliable

#breusch-godfrey test : H_0 test statistic is assymptotic chi-sqaured
bgtest(fit1)

#check the coefficients
coeftest(bgtest(fit1))

#truncation lag variable
m <- floor(0.75 * nrow(hwd)^(1/3))

#Calculate the corrected standard errors and coeffs based on Newey West estimator
sqrt(diag(NeweyWest(fit2, prewhite = F, lag = m-1, adjust = T))) 


#-------------------------------------------------------------------------------------------------------------------

# by hand calculation of the t and p statistic
fit1.coeffs <- fit1$coeffs

beta.estimate <- fit1.coeffs["unemployment", "Estimate"]  
std.error <- fit1.coeffs["unemployment", "Std. Error"]

t_value <- beta.estimate/std.error  # calc t statistic

p_value <- 2*pt(-abs(t_value), df=nrow(hwd)-ncol(hwd))  # calc p Value

f_statistic <- fit1$fstatistic[1]  # fstatistic

f <- summary(fit1)$fstatistic  # parameters for model p-value calc

#pf is the density, quantile and random generation for the F distribution
model_p <- pf(f[1], f[2], f[3], lower=FALSE)

#H0: There is no relationship between X and Y
summary(aov(fit1))

#TEST FOR GOODNESS OF FIT

#statistic-based (LOWER THE BETTER)

fit1.bic <- BIC(fit1)
fit1.bic

fit1.aic <- AIC(fit1)
fit1.aic

#FOR THE REALGDP FIT WE SKIP THE ASSUMPTIONS FOR THE SAKE OF SIMPLICITY
#(you're free to test it though )

fit2 <- lm(suicide ~ realgdp, data = hwd)

#summary of the regression
summary(fit2)

summary(aov(fit2))

confint(fit2)
#TEST FOR GOODNESS OF FIT

#statistic-based (LOWER THE BETTER)

fit2.bic <- BIC(fit2)
fit2.bic

fit2.aic <- AIC(fit2)
fit2.aic

#lets get the residual plot of the linear fit
fit2.res = resid(fit2)

#residual plot
plot(fit2, which=1, col = c('blue')) 

#studentized residuals to check for outliers/leverage
plot(gdp, studres(fit2), ylab="Studentized Residuals", xlab="GDP", main="Suicide rate") 
abline(0, 0, col = 'red')                  # the horizon



#lets see what happens when we include both, we skip any posthoc/assumption analysis for simplicity
#(you're free to test it though )

#multiple linear regression, for now take as a given, it will be further explained next class!
fit3 <- lm(suicide ~ unemployment + realgdp, data = hwd)

#summary of the regression
summary(fit3)

summary(aov(fit3))

confint(fit3)

#TEST FOR GOODNESS OF FIT

#statistic-based (LOWER THE BETTER)

fit3.bic <- BIC(fit3)
fit3.bic

fit3.aic <- AIC(fit3)
fit3.aic



#------------------------------------------------------------------------------------------------------------

#POLYNOMIAL REGRESSION

#perhaps the model could be better explained by a second or third order polynomial of unemployment

polyfit1 <- lm(suicide ~ poly(unemployment,3), data = hwd)

summary(polyfit1)

confint(polyfit1)

plot(fitted(polyfit1), residuals(polyfit1))

plot(polyfit1, which = 1, col = c("blue"))

qqPlot(polyfit1)

polyfit1.aic <- AIC(polyfit1)
polyfit1.aic

polyfit1.bic <- BIC(polyfit1)
polyfit1.bic

#------------------------------------------------------------------------------------------------------------

#BONUS ROUND: PREDICTING NEW VALUES

# Create Training and Test data -
set.seed(1)  # setting seed to always have the same value

trainingRowIndex <- sample(1:nrow(hwd), 0.8*nrow(hwd))  # row indices for training data

trainingData <- hwd[trainingRowIndex, ]  # model training data

testData  <- hwd[-trainingRowIndex, ]   # test data

# Build the model on training data -
lmTrainMod <- lm(suicide ~ unemployment, data=trainingData)  # build the model

#predicting the model on the train data
suicidePred <- predict(lmTrainMod, testData)  # predict distance

#look at the model
summary(lmTrainMod)

#estimated AIC (usually doesnt reflect the goodness of fit in this case)
AIC(lmTrainMod)

#lets calculate the prediction accuracy and error rates
actuals_preds <- data.frame(cbind(actuals=testData$suicide, predicteds=suicidePred))
correlation_accuracy <- cor(actuals_preds) 
correlation_accuracy
head(actuals_preds)

#CALCULATION OF THE ERROR RATES
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
min_max_accuracy

#Mean Absolute percentage error
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
mape

#K-fold Cross Validation
cvResults <- suppressWarnings(CVlm(data  = hwd, form.lm=suicide ~ unemployment, m=7, 
dots=FALSE, seed=1, legend.pos="topleft",  printit=FALSE, main="Fitted Values of K-fold CV")) # performs the CV



attr(cvResults, 'ms')  # => 251.2783 mean squared error



