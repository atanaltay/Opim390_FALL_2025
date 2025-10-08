#Types:
#Numeric, Character, Logical, Date, Vector (sequence of values), Matrix
#Factor (unordered categories) , Ordered (ordered categories), List,
#DataFrame

#numeric variable

num1 = 5
class(num1)

#character variable
txt1 = "shoes" 
class(txt1)
log1 = TRUE
log2 = FALSE

# or -> | , and -> &
log1&log2
log1|log2

date1= as.Date("2023-01-01")
date1

seq1 = 1:10
seq1
seq2 = seq(1,10,1)
seq2

#exponent
2^3

#integer division
5%/%2

#modulus or remainder

5%%3

#when you apply operations on sequences the operation will be broadcasted
seq1
seq1%/%2
(1:20)^2

#comparison operators
# < > <= >= != ==   , and->& , or->|


n1 = 40
n2 =  60

n1>n2
n1<n2
n1<=n2

n1 != n2 & n1>26

#defining a vector

vec1 = c(1,2,3,4,5)
class(vec1)

vec2 = c("phone",2,3,200)
vec2
class(vec2)
#in a vector all elements should be of same type
v1 <- c(2,3,1,4,50,25)
v1[1]  #first element
v1[2]  #second element
v1[1:3] #first 3 elements
v1[c(1,3,5)] #1st, 3rd and fifth elems
v1[-2] #all except 2nd elem
v1[-c(2,4)] #all except 2nd and 4th
v1[v1>10] #all elems greater than 10
v1[v1%%2==0] #all even elems
v1[v1%%2!=0] #all odd elems
v1[v1>2 & v1<30] #elems greater than 2 and less than 30
v1[v1<2 | v1>30] #elems less than 2

#built in functions:

length(v1)
sum(v1)
sum(v1[v1>10])
mean(v1)
mean(v1[v1>10])
median(v1)
min(v1)
max(v1)
sort(v1)
v1
sort(v1, decreasing = TRUE)

v1_sorted = sort(v1)
v1_sorted

c(3,4,55,22,33,44)  == 30

#matrices

?matrix
x <- matrix(data=c(1,2,3,4,5,6,7,8,9), nrow=3, ncol=3) 
x

x <- matrix(data=c(1,2,3,4,5,6,7,8,9), nrow=3, ncol=3,byrow = TRUE) 
x

x <- matrix(data=c(1,2,3,4,5,6,7,8,9,19), nrow=5, ncol=2,byrow = TRUE) 
x

dim(x) #dimensions of matrix (x,y) -> x: number of rows, y: number of columns

nrow(x) #number of rows
ncol(x) #number of columns

#access matrix elements

x[1,2] #1st row, 2nd column
x[3,1] #3rd row, 1st column
x[2,] #all elements of 2nd row
x[,2] #all elements of 2nd column
x[,1:2] #all elements of 1st and 2nd column
x[,-2] #all elements except 2nd column
x[,-c(1,3)] #all elements except 1st and 3
x

#setting column names

prices = c(100,120,200)
quantity = c(2,3,2)

datamtx = matrix(c(prices,quantity), nrow=3,byrow=FALSE)
datamtx
colnames(datamtx) = c("price","quantity")
datamtx

datamtx[, c("price")]
datamtx[, c("price","quantity")]

#operations on matrices

sum(x)
sqrt(x)
x^2
x*2
x+2

x<- matrix(data=c(1,1,1,1,1,1,1,1,1),nrow = 3,ncol = 3,byrow = TRUE)
x

v1 = c(1,2,3)
x
x+v1

# a 2 element vector cannot be boradcasted, the code below throws an error

v2 = c(1,2)
x+v2

x

v1
t(x) * v1

x2 = matrix(data=c(1,2,3,4,5,6,7,8,9), nrow=3,byrow=TRUE)
x2
colSums(x2)
rowSums(x2)
colMeans(x2)
rowMeans(x2)

# apply function
x2
apply(x2,1,sum) #1 means row, 2 means column, name of the function
apply(x2,2,sum)
apply(x2,1,mean)

#an example

x
quantityShoes = c(2,3,2)
quantityShirts = c(1,0,4)
quantityPants = c(1,2,1)

datamtx = matrix(c(quantityShoes,quantityShirts,quantityPants), nrow=3,byrow=FALSE)
datamtx
colnames(datamtx) = c("shoes","shirts","pants")
datamtx
prices
datamtx * prices
t(datamtx)
prices
calcmtx = t(datamtx) * prices

calcmtx = t(calcmtx)
calcmtx

sum(calcmtx)

colSums(calcmtx) #total sales per item
rowSums(calcmtx) #total sales per day

#dot product -> %*%
prices
datamtx

rowTotals = datamtx %*% prices
rowTotals
sum(rowTotals)







