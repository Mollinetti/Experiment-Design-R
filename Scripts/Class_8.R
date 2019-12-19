#BEFORE STARTING OUR CLASS, PLEASE RUN THIS SNIPPET TO INSTALL/LOAD OUR LIBRARIES NEEDED FOR THIS LESSON

packages_needed <- c('car', 'MASS', 'heplots', 'reshape2', 'mvnormtest', 'mvoutlier')
for (package_name in packages_needed) {      
  if (!(package_name %in% rownames(installed.packages()))){
    install.packages(package_name)
  }
}
for (i in 1:length(packages_needed))
{
  library(packages_needed[[i]],character.only=TRUE)
}

#load the skulls dataset
skulls <- read.csv("skulls.csv", header = T)

#attach columns
attach(skulls)

means <- tapply(1:nrow(skulls), skulls$epoch, function(i)
  apply(skulls[i,colnames(skulls)[-1]], 2, mean))
means <- matrix(unlist(means), nrow = length(means), byrow = TRUE)
colnames(means) <- colnames(skulls)[-1]
rownames(means) <- levels(skulls$epoch)

#we want to organize our data into independent and dependent variables
#get the independent var names 
skull.colnames <- as.character(unique(skulls$epoch))

#lets take a look at boxplots
par(mfrow = c(2, 2))
#mb
boxplot(data.frame(skulls$mb[skulls$epoch == skull.colnames[1]], skulls$mb[skulls$epoch == skull.colnames[2]],
          skulls$mb[skulls$epoch == skull.colnames[3]], skulls$mb[skulls$epoch == skull.colnames[4]], skulls$mb[skulls$epoch == skull.colnames[5]]) , las = 1, xlab = 'epochs', 
        names = skull.colnames, ylab = "maximum breadth", col = c('blue', 'red', 'yellow', 'pink'))

#basibregmatic height
boxplot(data.frame(skulls$bh[skulls$epoch == skull.colnames[1]], skulls$bh[skulls$epoch == skull.colnames[2]],
                   skulls$bh[skulls$epoch == skull.colnames[3]], skulls$bh[skulls$epoch == skull.colnames[4]], skulls$bh[skulls$epoch == skull.colnames[5]]) , las = 1, xlab = 'epochs', 
        names = skull.colnames, ylab = "basibregmatic height", col = c('blue', 'red', 'yellow', 'pink'))

#basialiveolar length
boxplot(data.frame(skulls$bl[skulls$epoch == skull.colnames[1]], skulls$bl[skulls$epoch == skull.colnames[2]],
                   skulls$bl[skulls$epoch == skull.colnames[3]], skulls$bl[skulls$epoch == skull.colnames[4]], skulls$bl[skulls$epoch == skull.colnames[5]]) , las = 1, xlab = 'epochs', 
        names = skull.colnames, ylab = "basialiveolar length", col = c('blue', 'red', 'yellow', 'pink'))

#nasal length
boxplot(data.frame(skulls$nh[skulls$epoch == skull.colnames[1]], skulls$nh[skulls$epoch == skull.colnames[2]],
                   skulls$nh[skulls$epoch == skull.colnames[3]], skulls$nh[skulls$epoch == skull.colnames[4]], skulls$nh[skulls$epoch == skull.colnames[5]]) , las = 1, xlab = 'epochs', 
        names = skull.colnames, ylab = "nasal length", col = c('blue', 'red', 'yellow', 'pink'))


#Boxplots does not tell us much about the differences
#run a manova to check the diffs
#cbind to merge the outcomes together
multivar.exp1 <- manova( cbind(mb, bh, bl, nh) ~ epoch, data = skulls)

#look at the differences
#Pillai is default test
summary(multivar.exp1, test = "Pillai")
summary(multivar.exp1, test = "Wilks")
summary(multivar.exp1, test = "Hotelling-Lawley")
summary(multivar.exp1, test = "Roy")

#details of each var over time
summary.aov(multivar.exp1)

#does correlation matrix tell us about collinearity?
cor(cbind(mb, bh, bl, nh))

#pearson pm test to identify collinearity
cor.test(mb,bh)
a<- cor.test(mb,nh)

#comparing two specific epochs
multivar.exp2 <- manova( cbind(mb, bh, bl, nh) ~ epoch, data = skulls, subset = epoch %in% c("c4000BC", "c200BC"))



#summary and details
summary(multivar.exp2)
summary.aov(multivar.exp2)

#ASSUMPTIONS

#Linearity (pearson -r test for each cols)
responses <- as.matrix(skulls[,2:5])
results.pearson <- data.frame(var = colnames(responses), mb = rep(NA,dim(responses)[2]), bh = rep(NA,dim(responses)[2]),
                              bl = rep(NA,dim(responses)[2]), nh = rep(NA,dim(responses)[2]))
results.pearson$mb <- apply(responses,2,function(x) {cor.test(x,mb)$p.value})
results.pearson$bh <- apply(responses,2,function(x) {cor.test(x,bh)$p.value})
results.pearson$bl <- apply(responses,2,function(x) {cor.test(x,bl)$p.value})
results.pearson$nh <- apply(responses,2,function(x) {cor.test(x,nh)$p.value})
results.pearson

#checking outliers using the mahalanobis distance
outliers <- aq.plot(skulls[,2:5])
#Identfy multivariate outliers by plotting the ordered squared robust Mahalanobis distances of the observations
#against the empirical distribution function
outliers # show list of outliers

#new dataset with removed outliers
skulls2 <- skulls[c(-34,-78,-4,-149),]
#outlier test for new dataset
outliers <- aq.plot(skulls2[,2:5])
outliers

#normality
mshapiro.test(t(data.matrix(skulls[,2:5])))

# Graphical Assessment of Multivariate Normality (Mahalanobis^2 distance will be approximately a chi-square distribution with p degrees of freedom)
x <- as.matrix(skulls[,2:5]) # n x p numeric matrix
center <- colMeans(x) # centroid
n <- nrow(x); p <- ncol(x); cov <- cov(x); 
d <- mahalanobis(x,center,cov) # distances 
qqplot(qchisq(ppoints(n),df=p),d,
       main="QQ Plot Assessing Multivariate Normality",
       ylab="Mahalanobis D2")
abline(a=0,b=1)

#Multivariate homogeneity of variance between groups 
#Levene test for each column
results.levene <- data.frame(var = colnames(responses), p = rep(NA,dim(responses)[2]))
results.levene$p <- apply(responses,2,function(x) {leveneTest(x ~ epoch)[1,3]})
results.levene

#Multivariate homogeneity of covariance between groups 
bm <- boxM(skulls[,2:5], epoch) #Box's-M test
plot(bm)


########################################################################################################################
########################################################################################################################
########################################################################################################################

#load the wolves dataset
wolves <- read.csv('Wolves.csv', header = T)

#attach cols
attach(wolves)

str(wolves)

#manova model 1
wolf.model <- manova(cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9) ~ group, data=Wolves)

#summary and details
summary(wolf.model)
summary.aov(wolf.model)

# using location, sex (notice that location and sex have an interaction coeff)
wolf.mod2 <-manova(cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9) ~ location*sex, data=Wolves)

summary(wolf.mod2)
summary.aov(wolf.mod2)



