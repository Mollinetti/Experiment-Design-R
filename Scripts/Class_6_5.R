#BEFORE STARTING OUR CLASS, PLEASE RUN THIS SNIPPET TO INSTALL/LOAD OUR LIBRARIES NEEDED FOR THIS LESSON

packages_needed <- c('car', 'leaps')
for (package_name in packages_needed) {      
  if (!(package_name %in% rownames(installed.packages()))){
    install.packages(package_name)
  }
}
for (i in 1:length(packages_needed))
{
  library(packages_needed[[i]],character.only=TRUE)
}

#COLUMNS OF THE DATASET
#1. Manufacturer, represented by its first initial: G=General Mills, K=Kelloggs, N=Nabisco, P=Post, Q=Quaker Oats, R=Ralston Purina.
#2. calories: number of calories in one portion.
#3. protein: grams of protein in one portion.
#4. fat: grams of fat in one portion.
#5. sodium: milligrams of sodium in one portion.
#6. fibre: grams of dietary fibre in one portion.
#7. carbo: grams of complex carbohydrates in one portion.
#8. sugars: grams of sugars in one portion.
#9. shelf: display shelf (1, 2, or 3, counting from the floor).
#10. potassium: grams of potassium.
#11. vitamins: vitamins and minerals (none, enriched, or 100%).

#load the dataset
cereal <- read.csv('Healthy_breakfast.csv', header = T)

#exclude manufacturer column
cereal <- cereal[,-1]

attach(cereal)

#perform best subset selection
regfit.full = regsubsets(sugars~., cereal)
summary(regfit.full)

#fitting a model with up to 6 levels
regfit.full = regsubsets(sugars~., cereal, nvmax = 6)


#perform forward best subset selection 
regfit.fwd = regsubsets(sugars~., cereal, method = "forward", nvmax = 7)

#perform backward best subset selection 
regfit.bwd = regsubsets(sugars~., cereal, method = "backward", nvmax = 7)

#the coeffs are different?
coef(regfit.fwd,7)

coef(regfit.bwd,7)

#save the summary
reg.summary = summary(regfit.fwd)

#plot the goodness of fit
par = (mfrow = c(2,2))
plot(reg.summary$rss ,xlab="Number of Variables ",ylab="RSS",type="l")
plot(reg.summary$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq",type="l")

#check the largest adjusted R^2 statistic
which.max(reg.summary$adjr2)

#highlight point 6 at the graph
points(6,reg.summary$adjr2[6], col = "red", cex = 2, pch = 20)

plot(reg.summary$cp ,xlab="Number of Variables ",ylab="Cp", type="l")
#minimum point of cp
min.cp = which.min(reg.summary$cp)
points(min.cp ,reg.summary$cp [min.cp ],col="red",cex=2,pch=20)

#minimum point of bic
min.bic <- which.min(reg.summary$bic )
#plot the bic
plot(reg.summary$bic ,xlab="Number of Variables ",ylab="BIC",type="l")
points(min.bic ,reg.summary$bic [min.bic ],col="red",cex=2,pch=20)
