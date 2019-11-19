#BEFORE STARTING OUR CLASS, PLEASE RUN THIS SNIPPET TO INSTALL/LOAD OUR LIBRARIES NEEDED FOR THIS LESSON

packages_needed <- c('car', 'reshape2', 'psych', 'multcomp', 'effects')
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

lepr <- read.table('leprosy.dat', header = T)
attach(lepr)

str(lepr)

#equality of slopes interaction
fitted.lepr <- aov(x ~ drug + y + drug:y, data = lepr)
#we are interested in the interaction term
summary(fitted.lepr)

#linearity of slopes (Eyeballing, just like Rick's neutrino bomb)
plot(y, x, col = drug, xlab = "X", ylab = "Y")

#Equality of the groups on the covariate
#Anova with the groups
fitted.eq<- aov(y ~ drug, data = lepr)
summary(fitted.eq)

#Homogeneity of variances - levene test
leveneTest(x ~ drug, center = mean, data = lepr)

#RUNNING THE ANCOVA (ORDER OF THE LINEAR FIT MATTERS!)
fit.ancova <- aov(x ~ y + drug, data = lepr)  # NOTE: covariate goes first!! there is no interaction
summary(fit.ancova)

#CHECK FOR NORMALITY
#shapiro test on the residuals
residuals <- resid(fit.ancova)
shapiro.test(residuals)

#histogram for the residuals
hist(residuals)

# to get the ’real’ p value and difference,
# uses the general linear hypothesis function
posthoc <- glht(fit.ancova, linfct = mcp(drug = "Tukey"))

summary(posthoc)

confint(posthoc)

#from the psych library
describeBy(x, drug)

#from the effects library
#adjusted group means
effect("drug", fit.ancova)


###################################################################################################
###################################################################################################

#THE DATASET THAT WE'LL USE IS THE INSECT SPRAYS DATASETS AND IT'S ALREADY BUILT IN THE CAR
#LIBRARY


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




