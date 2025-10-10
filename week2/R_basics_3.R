#Factors

gender <- c("male","female","male","male","male","male","male","female","female","female")
genderFactor<- factor(gender)
class(genderFactor)

summary(genderFactor)
genderFactor

levels(genderFactor)

as.numeric(genderFactor)
#######################
### discrete uniform random generation

gender_char = sample(c("male","female"),10000,replace = TRUE)

class(gender_char)

gender_fac = as.factor(gender_char)
gender_fac
summary(gender_fac)

# ordered factor

size = c("small","medium","large","small","medium","large","small","medium","large","small","medium","small","medium")

# if you use factor() with ordered samples, they wil not be assigne a numeric value accordingly
summary(factor(size))

ordered.size = factor(size,levels = c("small","medium","large"), ordered = TRUE)

summary(ordered.size)
levels(ordered.size)
as.numeric(ordered.size)

new_ordered_size = ordered(size,levels = c("small", "medium", "large"))

as.numeric(new_ordered_size)

#Conditional statements

#absolute number conversion
num <- -100

if(num<0){
  print(-1*num)
}else{
  print(num)
}

x = 30

if(x<=30){
  print("less than 30")
}else if(x<40){
  print("30-40")
}else if(x<60){
  print("40-60")
}else{
  print("Greater than 60")
}

#ifelse(CONDITION,OPERATION IF TRUE,OPERATION IF FALSE)

ifelse(x<=40,print("less or equal"),print("higher"))

# loops

names = c("jack","hennry","william","bob","alice")

for(name in names){
  
  print(name)
  
}

count <- 1

for(name in names){
  print(paste(name,count,sep = " "))
  count = count+1
}


for(i in 1:10){
  print(i)
}


for(i in 1:length(names)){
  print(names[i])
}

#########FUNCTIONS#################

add_two_numbers <- function(a,b){
  sum = a+b
  return(sum)
}

add_two_numbers(2,7)

#function to add all numbers in a vector

cn = c(200,3,23,544,11,222,3434,788,95)

sum_all <- function(vec){

  sum <- 0
  for(num in vec){
    sum=sum+num
  }
  return(sum)
}

sum_all(cn)

sum_all(cn[1:5])

#count even numbers:

count_even <- function(vec){
  
  count <- 0
  
  for(val in vec){
    if(val%%2==0){
      count = count+1
    }
    
  }
  
  return(count)
  
}

count_even(c(2,2,2,3,3,3))


#functions with named parameters

Rectangle = function(length=5, width=4){
  area = length * width
  return(area)
}

# Case 1:
print(Rectangle(2, 3))

# Case 2:
print(Rectangle(width = 8, length = 4))

# Case 3:
print(Rectangle())

#the list type

myList <- list(name="John", age=25, scores=c(90,85,88), is_student=TRUE)
myList
class(myList)
myList$name
myList$scores
myList$scores[2]
myList[["age"]]
myList[[3]][2]

#iterate over list with for loop:
for(item in myList){
  print(item)
}

for (idx in 1:length(myList)){
  print(myList[idx])
}








