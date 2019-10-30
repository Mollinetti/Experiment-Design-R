#BEFORE STARTING OUR CLASS, PLEASE RUN THIS SNIPPET TO INSTALL/LOAD OUR LIBRARIES NEEDED FOR THIS LESSON

packages_needed <- c('car', 'nortest')
for (package_name in packages_needed) {      
  if (!(package_name %in% rownames(installed.packages()))){
    install.packages(package_name)
  }
}
for (i in 1:length(packages_needed))
{
  library(packages_needed[[i]],character.only=TRUE)
}


#load dataset
hp <- read.csv("HPressure.csv")

#do the t-test on the blood pressure dataset
t.test(hp$intval, mu = 120, conf.level =0.95)

#one-sided test
t.test(hp$intval, alternative = "less", mu = 120, conf.level =0.95)

#power t-test to check for power
power.t.test(n = 10, delta = 1.2, sd = sd(hp$intval), sig.level = 0.01, type = "one.sample")

#power t-test to determine sample size
power.t.test(power = 0.85, delta = 1.2, sd = sd(hp$intval), sig.level = 0.01, type = "one.sample")

#qqplot
qqPlot(hp$intval, pch = 16, cex = 1, las = 1, col = 'red')


#Analytical tests

#NORMALITY OF DATA

#anderson darling (requires nortest lib)
ad.test(hp$intval)
#shapiro wilk (requires car lib)
shapiro.test(hp$intval)
#Kolmogorov-Smirnov test (does our sample follow a normal distrib with that sample mean?)
ks.test(hp$intval, 'pnorm', mean(hp$intval))

#INDEPENDENCE OF DATA
durbinWatsonTest(lm(hp$intval ~1))



#TWO SAMPLE HYPOTHESIS TESTING

#load the dataset
data <- read.csv('HPressure_multiple')

#pooled t-test
t.test(data$control, data$test, var.equal = TRUE, conf.level = 0.95)

#paired t-test
t.test(data$control, data$test, var.equal = TRUE, conf.level = 0.95, paired = TRUE)

#NORMALITY TEST FOR THE DATA
par(mfrow=c(1,2))
qqPlot(df$control, cex=1.5,pch=16, las=1)
qqPlot(df$test, cex=1.5,pch=16, las=1)

#shapiro-wilk test
#list of results
res_shapiro <- list("control" = NULL, "test" = NULL)
for(c in colnames(df)){
  res_shapiro[[c]] <- shapiro.test(df[[c]])
}

#HETEROSCEDASCITY
#fligner kileen test
fligner.test(df)

#plot our residuals
resids <- apply(df, 1, function (x) (x- mean(x)))
resids <- list("test" = resids[1,], "control" = resids[2,])

stripchart(x = resids, 
           vertical =  TRUE,
           pch = 16,
           cex = 1.5,
           las= 1,
           xlab = 'mean',
           ylab = 'residuals')

#durbin watson for two samples
durbinWatsonTest(lm(df$control + df$test ~1))




