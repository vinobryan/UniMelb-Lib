################################
# Prep - R Tutorial
################################

# R as a calculator
2 + 3
251*8

# Mathematical Operators

2+5    # '+' operator, Addition 
2-5    # '-' operator, Subtraction 
2*5    # '*' operator, Multiplication
2/5    # '/' operator, Division 
2^5    # '^' operator, Exponentiation (power)
5%/%2  # '%/%' operator, Integer Divide  
5%%2   # '%%' operator, Modulo 

# How to get help?
?mean
help(mean)
# Help for operators
?"&"
?"%/%"

# Create vector (assign values)
a <- 4               # '<-' assign operator 
value1 <- c(3,6,23)  # 'c()' function which combines its arguments
rep(1, 3)            # Replicates the value '1' (in -first- 'element'), '3' times (in -second- 'element')
seq(from = 1, to = 9, by = 3)   # Generate regular sequences
seq(1,9,3)
seq(by = 3, from = 1, to = 9) 
### #### This is the wrong way  
seq(3,1,9) 
########## This is the correct way
seq(from = 1, to = 4, by = 1)
## or equivalently this, for increments of 1 
1:4

## Subsetting
value1[1]         # first element of the vector value1
value1[1:2]       # first to second elements of the vector value1
value1[c(1,3)]    # first and third elements of the vector value1

# glue strings
a <- "Hello"
b <- "World"
c(a,b)
length( c(a,b)  )   # how many elements in a vector
paste(a,b)          # 'paste()' concatenates its arguments
paste(a,b, sep = "")          # control the space between the concatenated arguments  

# Logical Operators

value2 <- c(1,2,3)

value1 == 23                         # '==' operator, Equals
value1 != 23                         # '!=' operator, Not Equals
value1 < 6                           # '<' operator, Less Than
value1 > 6                           # '>' operator, Greater Than
value1 <= 6                          # '<=' operator, Less Than or Equal To
value1 >= 6                          # '>=' operator, Greater Than or Equal To
value1==6 & value2 <= 2              # '&' operator, Elementwise And   
value1==6 | value2 <= 2              # '|' operator, Elementwise Or

value1[1] <- NA

is.na(value1) && value2 == 1         # '&&' operator, Control And
which(is.na(value1) && value2 == 1)  # 'which' comand, Give the location of TRUE indices of a logical object
is.na(value1) || value2 == 4         # '||' operator, Control Or
!is.na(value1)                       # '!' operator, Logical Negation  

# Some Basic function
value1     #### recall object
mean(value1)   # average
## safer to use this
mean(value1,na.rm=TRUE)  ## removes NAs then calculates average 
value1 <- c(3,6,23) ### 
mean(value1)   # average
mean(value2)
sd(value2)     # standard deviation
median(value2) # median


# Matrix

value1 <- c(3,6,23)
cbind(value1, value2)   # combine by columns
rbind(value1, value2)   # combine by rows

matrix(c(1,2,3,2,4,6), ncol = 2)  # create 3 on 2 matrix from a given vector
matrix(c(1,2,3,2,4,6), nrow = 2)  # create 3 on 2 matrix from a given vector


################### extras #################
# if , ifelse

if(value1[1] == 3) print("Yes")

if(value1[1] == 6){ 
  print("Yes")
  } else {
    print("No")
  }

ifelse( value1[1] == 6 , "Yes" , "No")

# For loops
for(i in 1:5){
  print(  paste("iteration number", i) )
}


#install.packages("RWinEdt")
#library ("RWinEdt")
