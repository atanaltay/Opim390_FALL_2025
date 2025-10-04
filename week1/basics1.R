
print("Hellooo")

#variable declaration

var1 = 10
x <- 5
y = 7

#Types:
#Numeric, Character, Logical, Date, Vector (sequence of values), Matrix
#Factor (unordered categories) , Ordered (ordered categories), List,
#DataFrame

#Mathematical Operations

# + - * %

num1 <- 5
num2 <- 10

10+3-10
num1 * num2

sum = num1+num2

sum

#Shows how to assign values to variables in R and do basic mathematical calculations
z <- 3
y <- -2
x <- c(1,2,6) #defining a vector

#camel case notation
nameLastname = "altug tanaltay"
#define a sequence
w <- -1:4
w

# start, stop, step
seq(0,10,2)


v <- z*y
u <- z/y
t <- z+y
s <- z-y
r <- x*z
q <- x+w
p <- x*w
o <- x/w
n <- sqrt(w) #Note that sqrt(-1) produces the NaN error in R which stands for "Not a Number"
n

ls()

newEnv = new.env(hash=TRUE)
newEnv$age = 20
newEnv$name = "altug"
rm(newEnv) # removing variables from environment

ls()

rm(n)
rm(o)

rm(list=ls()) #removing all variables from environment

# installing packages
install.packages("dplyr")
install.packages("ggplot2")

vignette("dplyr")

?sum
??sum
help(sum)

