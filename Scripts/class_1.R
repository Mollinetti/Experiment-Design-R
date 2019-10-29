#REMINDER: BEFORE YOU RUN ANYTHING, PLEASE REMEMBER THAT TO RUN A SPECIFIC SECTION OF THE SCRIPT, 
#HIGHLIGHT IT AND PRESS RUN OR CTRL/COMMAND+ R


#First program
print("hello world!")

#Basic variable assignment
x = 3
y <- 4


#DATA TYPES
char_var = "character"
typeof(char_var)

num_var = 54
#double is a floating point number (decimal)
typeof(num_var)

int_var = 2L
typeof(int_var)

#logical is either true or false
log_var1 = TRUE
log_var2 = FALSE
typeof(log_var1)

#complex var
compl_var = 1+4i
typeof(compl_var)

#VARIABLE NAMING
#These variables are valid

var1= 60

Var2 = 3.14

camelCaseVar = 'c'

the_meaning_of_the_universe_and_everything_else = 42

Knights_WhO_SaY_NI = "Ni"

#These variables are not valid and will produce an error
this is wrong = 3

45bad_variable = 6.0

DontdoThis$ = TRUE


#DATA STRUCTURES

#VECTOR

vector("character",length = 5) #empty character vector with 5 elements

x <- c(1,2,3,4) #combination of 4 numbers

y <- c(TRUE, TRUE, FALSE) #combination of logic variables

seq(10) #sequence from 1 to 10

seq(from=1, to = 10, by = 0.1) #from 1 to 10 in 0.1 steps

x <- c("a",NA, NA, "d","e") #character vector with empty entries (NA means empty)

#acessing elements of vectors
x[1] #first

x[5] #last

x[-1] #everything but the first

x[1:3] #1st to 3rd element (slice)

#MATRIX

m<- matrix(nrow = 2, ncol = 2) #matrix with 2 rows and 2 columns (empty)

m<- matrix(c(1:3)) #3x1 matrix created using a vector

x<- 1:3
y<- 10:12
cbind(x,y) #matrix by binding two vectors

m<- matrix(1:6, nrow = 2, ncol = 3) #filling a matrix columnwise

#accessing matrix elements
m[1,1]

m[1,2]

#select an entire column (the first)
m[,1]

#select an entire row (the second)
m[2,]

#LIST

x<- list(1,'a', TRUE, 1+4i) #list with distinct data types
x<- 1:10
x<- as.list(x) #coercing a vector to be a list

#accessing list elements
x[[1]] #first elem

x[[10]] #last elem

x[[-1]] #not allowed

#DATA FRAME

dat <- data.frame(id = letters [1:10], x = 1:10, y = 11:20)
#index are letters, first col are numbers from 1 to 10, second col are numbers from 11 to 20

ht <- read.csv('Heart.csv') #read a table

#accessing elements of a data frame
dat[1,3] #row 1, col 3

dat$x #column x

dat[['y']] #column y

#CONDITIONALS
a = 5
#Ex 1
if (a < 10){
  print("small number")
}
#Ex 2
if (a<= 4){
  print("small number")
} else{
  print("big number")
}
#Ex 3
if (a<= 4){
  print("small number")
} else if (a > 4 && a <= 10){
  print("medium number")
}else{
  print("big number")
}


#NESTED IFS
a = 5
b = TRUE

if (a<=4){
  if (b == TRUE){
    print("a is small and b is true")
  }else{
    print("a is small and b is false")
  }
} else{
  if (b == TRUE){
    print("a is big and b is true")
  } else{
    print("a is big and b is false")
  }
  
}

#LOOPS

#while loop
i = 0
while (i < 4){
  print(i)
  i= i + 1 
}
#infinite while loop
while (i < 4){
  print(i)
}
#for loop
x <- c(2,5,3,9,8,11,6)
count <- 0
for (val in x){
  if (val %% 2 == 0){
    count = count + 1
  }
}
print(count)

#break statements
x <- 1:5
for (val in x){
  if (val == 3){
    break
  }
  print(val)
}
#next statement
x <- 1:5
for (val in x){
  if (val == 3){
    next
  }
  print(val)
}
#NESTED LOOPS
x <- 1:3
y<- 1:2
for (val1 in x){
  for (val2 in y){
    print(c(val1,val2))
  }
}
#FUNCTIONS

check <- function(x) {
  if (x>0){
    result <-'positive'
  } else if ( x< 0){
    result <-'negative'
  } else{
    result<-'Zero'
  }
  return (result)
}

pow <- function(x,y){
  #function to print x raised to the power of y
  result <- x^y
  return(result)
}

#IMPORTING MODULES
#installing packages
install.packages('car')
library(car)

#BASIC PLOTTING
attach(Auto)
names(Auto)
summary(Auto)

plot(horsepower,weight)

plot(mpg, cylinders)

plot(cylinders,mpg)

#fashion statement
plot(cylinders,mpg, col = 'red', varwidth = T, xlab = "cylinders", ylab = "MPG")

