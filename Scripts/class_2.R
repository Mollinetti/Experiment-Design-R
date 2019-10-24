Heart <- read.csv('heart.csv')

#COMMON FUNCTIONS OF DATASET

dim(Heart)

names(Heart)

summary(Heart)

any(is.na(Heart)) #check for empty rows

Heart$Age

mean(Heart$Age)

pairs(Heart) #plots of everything

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

#PROBABILITY DISTRIBUTIONS

#univariate normal distribution
rnorm(10,mean=0,sd=1)

#multivariate normal distribution
library(MASS)
mvrnorm(n=10, c(0,0,0), diag(3), tol = 1e-6, empirical = FALSE, EISPACK = FALSE)

#chi squared
rchisq(n=10, df =  7)      # 7 degrees of freedom 

#student t distribution (10 degrees of freedom)
rt(n=10, df = 10)

