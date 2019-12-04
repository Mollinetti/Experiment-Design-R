#BEFORE STARTING OUR CLASS, PLEASE RUN THIS SNIPPET TO INSTALL/LOAD OUR LIBRARIES NEEDED FOR THIS LESSON

packages_needed <- c('car', 'MASS', 'lmtest', 'DAAG', 'leaps')
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

attach(cereal)

#perform best subset selection
regfit.full = regsubsets(sugars~., cereal)
summary(regfit.full)


#fitting a model with up to 6 levels
regfit.full = regsubsets(sugars~., cereal, nvmax = 6)
