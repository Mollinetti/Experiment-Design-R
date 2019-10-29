#BEFORE STARTING OUR CLASS, PLEASE RUN THIS SNIPPET TO INSTALL/LOAD OUR LIBRARIES NEEDED FOR THIS LESSON

packages_needed <- c('MASS','tolerance')
for (package_name in packages_needed) {      
  if (!(package_name %in% rownames(installed.packages()))){
    install.packages(package_name)
  }
}
for (i in 1:length(packages_needed))
{
  library(packages_needed[[i]],character.only=TRUE)
}


########################################################################
#########################DATA SETS PT 2#################################
########################################################################

#LOADING OUR DATASET
Heart <- read.csv('heart.csv')

#COMMON FUNCTIONS OF DATASET

dim(Heart)

names(Heart)

summary(Heart)

any(is.na(Heart)) #check for empty rows

Heart$Age

mean(Heart$Age)

pairs(Heart) #plots of everything


########################################################################
####################CATEGORICAL X NUMERICAL#############################
########################################################################

#CATEGORICAL X NUMERICAL

Heart$Cp

Heart$Sex

Heart$Oldpeak

cp<- as.factor(Heart$Cp)

plot(Heart$Cp, Heart$Oldpeak)

plot(cp, Heart$Oldpeak)

#CATEGORICAL x NUMERICAL PART 2

nm <- read.table("NelderMead.txt", header = TRUE, sep = ",", dec = ".")

names(nm)

head(nm)

unique(nm$dimension)

unique(nm$method_start)

########################################################################
###################PROBABILITY DISTRIBUTIONS############################
########################################################################

#dice experiment
for (val in c(100,1000,10000,100000)){
  rolls <- floor(runif(val,min=1, max = 7))
  #histogram
  hist(rolls, main = paste("Histogram of", val, "rolls"), col = 'green', las=1, breaks = c(0,1,2,3,4,5,6))
}

#univariate normal distribution
rnorm(10,mean=0,sd=1)

#multivariate normal distribution
mvrnorm(n=10, c(0,0,0), diag(3), tol = 1e-6, empirical = FALSE, EISPACK = FALSE)

#chi squared
rchisq(n=10, df =  7)      # 7 degrees of freedom 

#student t distribution (10 degrees of freedom)
rt(n=10, df = 10)

########################################################################
###################STATISTICAL INTERVALS################################
########################################################################


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

#calculate the tol from the normtol
tol <- normtol.int(x = hp$intval, alpha = 0.05, P = 0.95, side = 2)

#interval for plotting
x<- seq(110,130)
#drawing a normal distribution from our sample norm and sd
hx<- dnorm(x, mean = mean(hp$intval), sd = sd(hp$intval))

plot(x,hx, type = "l", lty = 2, xlab = "x value", ylab = "Density")

abline(v = tol$`2-sided.lower`, col = 'red') #put a vertical line at the left side of the tolerance interval
abline(v = tol$`2-sided.upper`, col = 'red') #put a vertical line at the right side of the tolerance interval

