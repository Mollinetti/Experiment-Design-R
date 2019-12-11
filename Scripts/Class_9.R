#BEFORE STARTING OUR CLASS, PLEASE RUN THIS SNIPPET TO INSTALL/LOAD OUR LIBRARIES NEEDED FOR THIS LESSON

packages_needed <- c('car', 'MASS', 'HSAUR',  'reshape2', 'mvnormtest', 'mvoutlier')
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
pimaIndians<- read.csv("PimaIndians.csv")

#attach cols
attach(pimaIndians)

#look into the details and see what we can do


#any plots before testing?


#any munging needed?


#test the assumptions


#any post-hoc test, power analysis?