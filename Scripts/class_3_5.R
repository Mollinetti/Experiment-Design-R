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

#SLEEP EXPERIMENT

x<- rnorm(23,mean=7.6,sd=1)
y<- rnorm(23,mean=7.3,sd= 1.11)
z<- rnorm(23,mean=7.4,sd=1.2)

df<- data.frame(x,y,z)
colnames(df)<- c("Control", "Test", "Placebo")
write.csv(df, file = 'sleep_exp_main.csv', row.names = F)


#variables
conf_alpha<- 0.95
power_beta<- 0.85
pilot_n <- 6

sleep_pilot <- read.csv('sleep_exp_pilot.csv', header = TRUE)

#power t-test to check for power
power.t.test(n = 6, delta = 1, sd = sd(sleep_pilot$Control), sig.level = 0.05, type = "one.sample")
power.t.test(n = 6, delta = 1, sd = sd(sleep_pilot$Test), sig.level = 0.05, type = "one.sample")
power.t.test(n = 6, delta = 1, sd = sd(sleep_pilot$Placebo), sig.level = 0.05, type = "one.sample")

#Placebo was the lowest, let's determine the sample size taking placebo into consideration
#power t-test to determine sample size
power.t.test(power = 0.85, delta = 1, sd = sd(sleep_pilot$Placebo), sig.level = 0.05, type = "one.sample") 


#load the sleep_main
sleep_main <- read.csv('sleep_exp_main.csv')

#let's look at our tables
summary(sleep_main)

#VALIDATIONS FOR OUR COLUMNS (SLIDE 31)

#NORMALITY TEST FOR THE DATA
par(mfrow=c(1,3))
qqPlot(sleep_main$Control, cex=1.5,pch=16, las=1)
qqPlot(sleep_main$Test, cex=1.5,pch=16, las=1)
qqPlot(sleep_main$Placebo, cex=1.5,pch=16, las=1)

#shapiro-wilk test
#list of results
res_shapiro <- list("Control" = NULL, "Test" = NULL, "Placebo" = NULL)
for(c in colnames(sleep_main)){
  res_shapiro[[c]] <- shapiro.test(df[[c]])
}

res_shapiro[['Control']]
res_shapiro[['Test']]
res_shapiro[['Placebo']]

#HETEROSCEDASCITY
#fligner kileen test
fligner.test(sleep_main)

#plot our residuals
resids <- apply(sleep_main, 1, function (x) (x- mean(x)))
resids <- list("Test" = resids[1,], "Control" = resids[2,], "Placebo" = resids[3,] )

stripchart(x = resids, 
           vertical =  TRUE,
           pch = 16,
           cex = 1.5,
           las= 1,
           xlab = 'mean',
           ylab = 'residuals')

#durbin watson for three samples
durbinWatsonTest(lm(sleep_main$Control + sleep_main$Test + sleep_main$Placebo ~1))


#DECIDING FOR WHICH T-TEST (SLIDE 32)

#correlation and covariance
cor(sleep_main)

cov(sleep_main)

#t-test based on our decision

#pooled t-test
t.test(sleep_main$Control, sleep_main$Test, var.equal = TRUE, conf.level = 0.95)
t.test(sleep_main$Control, sleep_main$Placebo, var.equal = TRUE, conf.level = 0.95)
t.test(sleep_main$Test, sleep_main$Placebo, var.equal = TRUE, conf.level = 0.95)





