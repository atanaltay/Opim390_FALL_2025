
#create a dataframe from scratch

gender = sample(c("male","female"),30,replace = TRUE)
age = sample(20:89,30,replace=TRUE)
income = rnorm(30,mean=1000,sd=500)
credit = ifelse(age>30 & income <1100,1,0)
credit


df = data.frame(gender=gender,age=age,income=income,credit=credit)

#common methods
head(df)
names(df)
View(df)

nrow(df)
ncol(df)
dim(df)

df$gender
df$income

#selecting columns

df[,c("gender","income")]
df

df[,c("gender","credit")]

#row filtering

#male clients
df[df$gender=="male",]

df[df$age>50,]

# multiple conditions we use and -> & or -> |   not eq-> !=  eq-> ==

df[(df$gender=="male") & (df$age>30),]

df[(df$gender=="male") & (df$age>30),c("income","credit")]

new_df = df[(df$gender=="male") & (df$age>30),c("income","credit")]
new_df

###########LOADING FILES FROM DISK################

########### read.csv
getwd()

#############
#File paths

#absolute path: "c://Computer/My_Documents/somefile.txt" (win)
              "/Users/atanaltay/Documents/R_projects/Opim390_FALL_2025"

#relative path: "data/mydata.txt"

#it will become "/Users/atanaltay/Documents/R_projects/Opim390_FALL_2025/data/mydata.txt"

##############

#wd: /Users/atanaltay/Documents/R_projects/Opim390_FALL_2025/
# "/Users/atanaltay/Documents/R_projects/Opim390_FALL_2025/week2/data/compact_suv_r.csv"
suv_df = read.csv("week2/data/compact_suv_r.csv")


head(suv_df)
tail(suv_df)

str(suv_df)

suv_df$Owner.Satisfaction <- ordered(suv_df$Owner.Satisfaction,levels=c("D","C","B","A"))

levels(suv_df$Owner.Satisfaction)

class(suv_df$Owner.Satisfaction)

suv_df[7,3]

suv_df[3:5,c(1,2)]

suv_df[1:5,c("Model","Overall.Score")]

#subset(DATA,CONDITION) function -> it is also for filtering the rows

subset(suv_df,suv_df$Recommended =="Yes" & suv_df$Overall.Miles.Per.Gallon>25)











