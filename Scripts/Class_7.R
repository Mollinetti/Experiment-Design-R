#BEFORE STARTING OUR CLASS, PLEASE RUN THIS SNIPPET TO INSTALL/LOAD OUR LIBRARIES NEEDED FOR THIS LESSON

packages_needed <- c('car', 'MASS', 'tidyr', 'tidyverse', 'broom', 'dplyr', 'gridExtra', 'ggplot2', 'heplots', 'ROCR')
for (package_name in packages_needed) {      
  if (!(package_name %in% rownames(installed.packages()))){
    install.packages(package_name)
  }
}
for (i in 1:length(packages_needed))
{
  library(packages_needed[[i]],character.only=TRUE)
}

#load the breast cancer data
BreastCancer_full <- read.csv("BreastCancer.csv", header =  T)

#limit only to the first ten columns plus response
BreastCancer <- BreastCancer_full[,1:11]


#lets look at the cols  
names(BreastCancer) #Class is our dependent variable this time

#check for null columns
unique(is.na(BreastCancer))

#drop null columns
BreastCancer<- drop_na(BreastCancer)


#attach cols
attach(BreastCancer)

#lets look at the correlation matrix
cor(sapply(BreastCancer[,-1],as.numeric))


#logistic regression with all variables but the id (we use glm for that)
reg.fit <- glm(diagnosis~., data = BreastCancer, family = binomial)

#summary
summary(reg.fit)

#accessing the coefficients
coef(reg.fit)

#confidence intervals
confint(reg.fit)

#look at the dummy vars
contrasts(diagnosis) #Benign 0 Malign 1

#predict class using the fitted data in the form of P(Y=1|X)
glm.probs = predict(reg.fit, type = "response")
glm.probs[1:10]

#convert the probabilities of glm.probs into proper classes "Malignant" or "Benign"
#create a vector of 1250 "benign"
glm.pred = rep("B",nrow(BreastCancer))
#Fill with "Malignant" whatever probabilities above 0.5 (our chosen threshold)
glm.pred[glm.probs > 0.5] = "M"
glm.pred[1:10]

#subsets
pred.b <- subset(BreastCancer, diagnosis == 'B')
pred.m <- subset(BreastCancer, diagnosis == 'M')


#create a confusion matrix, diagonals are correct predictions
table(glm.pred, diagnosis)

#look at the mean of the data (verify the accuracy)
mean(glm.pred == diagnosis)

#scatterplot of log odds vs predictors
# Select only numeric predictors
mydata <- BreastCancer[,-1]
predictors <- colnames(mydata)
# Bind the logit and tidying the data for plot
mydata <- mydata %>%
  mutate(logit = log(glm.probs/(1-glm.probs))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

#create the scatter plot
ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")


#check for outliers
par(mfrow=c(2,2))
plot(reg.fit)

plot(reg.fit, which = 4, id.n = 3)

#check collinearity
vif(reg.fit) # variance inflation factors 
sqrt(vif(reg.fit)) > 2 # problem? cutoff is 5 or 10

#########################################################################################################################################################################################
#########################################################################################################################################################################################
#########################################################################################################################################################################################


#LINEAR DISCRIMINANT ANALYSIS

lda.fit <- lda(diagnosis~., data = BreastCancer)

lda.fit 

#summary of lda
summary(lda.fit)

#Plot of the lda
plot(lda.fit)

lda.pred = predict(lda.fit,BreastCancer)
names(lda.pred)

lda.class = lda.pred$class

table(lda.class, diagnosis)

mean(lda.class == diagnosis) 

sum(lda.pred$posterior[,1]>=.5)

sum(lda.pred$posterior[,1]<.5)

lda.pred$posterior[1:20,1] 

lda.class[1:20]

#change the threshold values
sum(lda.pred$posterior[,1]>=.9)

#ASSUMPTIONS

#equality of variance covariance

#box plots of var covar

plot <- list()

box_variables <- colnames(BreastCancer[,-1])
for(i in box_variables) {
  plot[[i]] <- ggplot(BreastCancer, aes_string(x = "diagnosis", y = i, col = "diagnosis", fill = "diagnosis")) + 
    geom_boxplot(alpha = 0.2) + 
    theme(legend.position = "none") + 
    scale_color_manual(values = c("blue", "red")) 
  scale_fill_manual(values = c("blue", "red"))
}

do.call(grid.arrange, c(plot, nrow = 1))

#plot of the spheres

covEllipses(BreastCancer[,-1], 
                     diagnosis, 
                     fill = TRUE, 
                     pooled = FALSE, 
                     col = c("blue", "red"), 
                     variables = c(1:5, 8), 
                     fill.alpha = 0.05)

#box muller test


bm <- boxM(BreastCancer[,-1], diagnosis) # using all but first column
plot(bm)

#ROC CURVE
labs <- BreastCancer[1]
labs<- unclass(labs$diagnosis)
labs <- data_frame(labs)

pred <- prediction(lda.pred$x, labs)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)

#Normality

#qq-plots of predictors
par(mfrow=c(3,4))
variable.cols <- names(BreastCancer[,-1])
for (i in variable.cols){
  qqnorm(pred.b[[i]]);qqline(pred.m[[i]], col = 2)
}

#shapiro-wilk test on dubious columns
shapiro.test(pred.m$radius_mean)

#shapiro-wilk on transformed data
shapiro.test(sqrt(pred.m$radius_mean))


#########################################################################################################################################################################################
#########################################################################################################################################################################################
#########################################################################################################################################################################################


#QUADRATIC DISCRIMINANT ANALYSIS
qda.fit = qda(diagnosis~., data = BreastCancer)

qda.fit

#predicting
qda.class = predict(qda.fit, BreastCancer)$class
table(qda.class, diagnosis)
mean(qda.class == diagnosis)
