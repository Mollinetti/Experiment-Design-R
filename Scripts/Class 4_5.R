#BEFORE STARTING OUR CLASS, PLEASE RUN THIS SNIPPET TO INSTALL/LOAD OUR LIBRARIES NEEDED FOR THIS LESSON

packages_needed <- c('car', 'reshape2', 'psych', 'multcomp', 'effects', 'pwr', 'broom')
for (package_name in packages_needed) {      
  if (!(package_name %in% rownames(installed.packages()))){
    install.packages(package_name)
  }
}
for (i in 1:length(packages_needed))
{
  library(packages_needed[[i]],character.only=TRUE)
}



###################################################################################################
###################################################################################################
#ANCOVA

lepr <- read.csv('leprosy.csv', header = T)
attach(lepr)

str(lepr)

#equality of slopes interaction
fitted.lepr <- aov(After ~ Drug * Before, data = lepr)

#we are interested in the interaction term
summary(fitted.lepr)

#linearity of slopes (Eyeballing, just like Rick's neutrino bomb)
plot(After, Before, col = Drug, xlab = "Before", ylab = "After")

#Equality of the groups on the covariate
#Anova with the groups
fitted.eq<- aov(Before ~ Drug, data = lepr)

#Homogeneity of variances - levene test
leveneTest(After ~ Drug, center = mean, data = lepr)

#RUNNING THE ANCOVA (ORDER OF THE LINEAR FIT MATTERS!)
fit.ancova <- aov(After ~ Before + Drug, data = lepr)  # NOTE: covariate goes first!! there is no interaction

summary(fit.ancova)

#CHECK FOR NORMALITY
#shapiro test on the residuals
residuals <- resid(fit.ancova)
shapiro.test(residuals)

#diagnostic partial plot
lep.drug <- aov(After ~ Drug, lepr)
summary(lep.drug)

#histogram for the residuals
hist(residuals)

# to get the ’real’ p value and difference,
# uses the general linear hypothesis function
posthoc <- glht(fit.ancova, linfct = mcp(Drug = "Tukey"))

summary(posthoc)

confint(posthoc)

#from the psych library
describeBy(x, drug)

#from the effects library
#adjusted group means
effect("Drug", fit.ancova)

LM <- lm(After ~ Before + Drug, data = lepr)
anova(LM)

summary(LM)

#checking the r-2
tidy_aov <- tidy(fit.ancova)

#calculate the r-squared
sum_squares_regression <- tidy_aov$sumsq[1]
sum_squares_residuals <- tidy_aov$sumsq[2]

R_squared <- sum_squares_regression /(sum_squares_regression + sum_squares_residuals)


#power analysis of the ancova
pwr.f2.test(u = 2, f2 = 0.6389/ (1-0.6389), sig.level = 0.05, power = 0.85)

#n = v + u + 1
pwr.f2.test(u = 2, v = 7, f2 = 0.6389/ (1-0.6389), sig.level = 0.05)
###################################################################################################
###################################################################################################

#THE DATASET THAT WE'LL USE IS THE INSECT SPRAYS DATASETS AND IT'S ALREADY BUILT IN THE CAR LIBRARY


#load the dataset
sprays <- InsectSprays
#columns name
attach(sprays)

#unmolten dataset
un.sprays <-data.frame(count[1:12], count[13:24], count[25:36], count[37:48], count[49:60], count[61:72])
colnames(un.sprays) <- c('A','B','C','D','E','F')


#THE ANALYSIS WILL BE DONE DURING CLASS

#analysis of the columns

#box plot


#anova model used in this situation

#validation of the assumptions (levene, shapiro, qq plots of residuals)

#Post-hoc Analysis (remember to show them the Scheffe's test)

#scheffe.test()

#any extra plot

#Power Analysis! ()

###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################



#BONUS ROUND! THINK YOU CAN DO THIS ANALYSIS?

#data of deaths by lung cancer in the UK 






