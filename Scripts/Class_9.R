#BEFORE STARTING OUR CLASS, PLEASE RUN THIS SNIPPET TO INSTALL/LOAD OUR LIBRARIES NEEDED FOR THIS LESSON

packages_needed <- c('car', 'MASS', 'heplots',  'reshape2', 'mvnormtest', 'mvoutlier')
for (package_name in packages_needed) {      
  if (!(package_name %in% rownames(installed.packages()))){
    install.packages(package_name)
  }
}
for (i in 1:length(packages_needed))
{
  library(packages_needed[[i]],character.only=TRUE)
}

#load the dataset
ctbInjection <- read.csv("SendableCTBInjection.csv")

#attach cols
attach(ctbInjection)

#look into the details and see what we can do
summary(ctbInjection)
str(ctbInjection)

#probably, our dependent variable is Type, so we could consider: Classification, or a test of differences
Type

#QUESTION: WHY CLASSIFICATION WOULD NOT BE A GOOD IDEA?

#lets take a look at our class column
Class #if I was an expert at the field and if I knew what it was about, a possible grouping of the variables could be done

#However, note how the class has some RE, Re, with and without spaces, perhaps we could do a bit of munging in there
class.string <- as.character(Class)
#remove spaces
class.string <- as.character(lapply(class.string,(function (x) sub(' ','',x))))
#cchange to upper case
class.string <- as.character(lapply(class.string,toupper))
#take a look at the var again
class.string
#replace the column of the dataframe with the new column
ctbInjection$Class <- as.factor(class.string)


#any plots before testing?
#lets look at some boxplots divided by classes
for (name in colnames(ctbInjection)[-1]) {
  plot(Type, ctbInjection[,name], col = c('red', 'blue','yellow', 'pink', 'green'), main = name)
}


#the test that we chose was to conduct a MANOVA using all but pixels.per.1mm as dependent vars and Type as the independent var
multivar.fit <- manova( cbind(Pixels, Area,cfos, cfos.mm, ctb.mm, ctb, double, double.mm, double.cfos, double.ctb) ~ Type, data = ctbInjection)

#look at the differences
#Pillai is default test
summary(multivar.fit , test = "Pillai")
summary(multivar.fit , test = "Wilks")
summary(multivar.fit , test = "Hotelling-Lawley")
summary(multivar.fit , test = "Roy")

#do a post-hoc with one way anovas to see which dependent var was influent
summary.aov(multivar.fit)


multivar.fit2 <- manova( cbind(Pixels, cfos) ~ Type, data = ctbInjection)
summary(multivar.fit2 , test = "Pillai")
summary.aov(multivar.fit2)

#test the assumptions
#check the correlations
cor.mat<- cor(ctbInjection[,c(-1,-2,-4)])
#second highest columnwise
apply(cor.mat,2,(function (x) max(x[-which.max(x)])))
#lowest columnwise
apply(cor.mat,2,min)

#checking outliers using the mahalanobis distance
outliers <- aq.plot(ctbInjection[,c(3,9)])
#Identfy multivariate outliers by plotting the ordered squared robust Mahalanobis distances of the observations
#against the empirical distribution function
outliers # show list of outliers


#normality
mshapiro.test(t(data.matrix(sqrt(ctbInjection[,c(3,9)]))))

# Graphical Assessment of Multivariate Normality (Mahalanobis^2 distance will be approximately a chi-square distribution with p degrees of freedom)
x <- as.matrix(sqrt(ctbInjection[,c(3,9)])) # n x p numeric matrix
center <- colMeans(x) # centroid
n <- nrow(x); p <- ncol(x); cov <- cov(x); 
d <- mahalanobis(x,center,cov) # distances 
qqplot(qchisq(ppoints(n),df=p),d,
       main="QQ Plot Assessing Multivariate Normality",
       ylab="Mahalanobis D2")
abline(a=0,b=1)

#Multivariate homogeneity of variance between groups 
#Levene test for each column
responses = ctbInjection[,c(3,9)]
results.levene <- data.frame(var = colnames(responses), p = rep(NA,dim(responses)[2]))
results.levene$p <- apply(responses,2,function(x) {leveneTest(x ~ Type)[1,3]})
results.levene

#Multivariate homogeneity of covariance between groups 
bm <- boxM(responses, Type) #Box's-M test
plot(bm)
