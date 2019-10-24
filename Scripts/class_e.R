#load dataset
hp <- read.csv("HPressure.csv")

#column of the data we'll use
hp$intval

#confidence interval knowing the standard deviation
error<- qnorm(0.975)*1/sqrt(nrow(hp)) #std is 1 in this case

#right side of the distrib
right<- mean(hp$intval) + error
#left side of the distrib
left<- mean(hp$intval) - error


#CONFIDENCE INTERVAL NOT KNOWING THE STANDARD DEVIATION
error<- qnorm(0.975,df=nrow(hp)-1)*sd(hp$intval)/sqrt(nrow(hp)) #n-1 degrees of freedom (t-distribution)
#right side of the distrib
right<- mean(hp$intval) + error
#left side of the distrib
left<- mean(hp$intval) - error


#CALCULATING THE TOLERANCE INTERVALS
#install the library (macos users need to install the x11)
install.packages('tolerance')
#load the library
library(tolerance)

#calculate the tol from the normtol
tol <- normtol.int(x = hp$intval, alpha = 0.05, P = 0.95, side = 2)

#interval for plotting
x<- seq(110,130)
#drawing a normal distribution from our sample norm and sd
hx<- dnorm(x, mean = mean(hp$intval), sd = sd(hp$intval))

plot(x,hx, type = "l", lty = 2, xlab = "x value", ylab = "Density")

abline(v = tol$`2-sided.lower`, col = 'red') #put a vertical line at the left side of the tolerance interval
abline(v = tol$`2-sided.upper`, col = 'red') #put a vertical line at the left side of the tolerance interval