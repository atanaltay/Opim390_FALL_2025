
suv_df <- read.csv("week1/data/compact_suv_r.csv") #Reads in the CSV file CompactSUV.csv into a data frame in R
head(suv_df)
tail(suv_df)
View(suv_df) #Displays the data frame

str(suv_df) #Displays a summary of the columns in the data frame

names(suv_df)

suv_df$Owner.Satisfaction###
class(suv_df$Owner.Satisfaction)

suv_df$Owner.Satisfaction <- ordered(suv_df$Owner.Satisfaction, levels=c("D","C","B","A")) 
#Extracts a column of the data frame and converts to an ordered type
#levels are in increasing order
levels(suv_df$Owner.Satisfaction)

#indexing

entry73 <- suv_df[7,3] #Stores the entry in row 7, column 3 of data frame as data object named entry73
entry73
class(entry73) #Identifies class of data object entry73
row7 <- suv_df[7,] #Row 7 of suv_df is stored in data object named row7
row7
class(row7) #Identifies class of data object row7
col3 <- suv_df[,3] #Column 3 of suv_df is stored in data object named col3
col3
class(col3) #Identifies class of data object col3

#select multiple columns


sub_df <- suv_df[c(1:5),c(1:3,6)]  #Creates a new data frame from first five rows and columns 1, 2, 3 and 6
sub_df  #Display summary of data frame

suv_df[1:5,c("Model","Overall.Score")]

# Filtering rows based on conditions
rec_df <- subset(suv_df, Recommended == "Yes" & (Overall.Miles.Per.Gallon > 25 | Owner.Satisfaction == "A")) #Creates new data frame based on logical arguments

## 17/10/2025


suv_df$NewRec <- suv_df$Recommended

suv_df$NewRec
?which
slow_rows <- which(suv_df$Acceleration..0.60..Sec > 9)  #s rows that satisfy stated condition
slow_rows  #Displays extracted rows
suv_df[slow_rows, ]  #Displays rows of data frame that satisfy stated condition
#column operations

suv_df$high_satisfaction <- ifelse(suv_df$Owner.Satisfaction=="A",1,0)
suv_df

suv_df[suv_df$Owner.Satisfaction %in% c("C","D"),]

suv_df[suv_df$Overall.Score==  min(suv_df$Overall.Score),]

suv_df[suv_df$Overall.Score==  max(suv_df$Overall.Score),]
?order
sorted_df <- suv_df[order(suv_df$Overall.Score,decreasing = TRUE),] #Sorts data frame by Overall.Score in ascending order
sorted_df[1:3,]
#sort(suv_df$Overall.Score,decreasing = TRUE)[1:3]
#order(suv_df$Overall.Score,decreasing = TRUE)[1:3]
suv_df$Recommended <- NULL #Deletes original variable Recommended
View(suv_df)  #Displays the updated data frame
summary(suv_df)  #Displays a statistical summary of the data frame

#Type conversion
as.character(suv_df$Overall.Score) #Converts Overall.Score column to character type
as.numeric(as.character(suv_df$Overall.Score)) #Converts Overall.Score column to numeric

#applying functions to columns
#X → a matrix, array, or data frame
#MARGIN → 1 = rows, 2 = columns
#FUN → the function you want to apply (e.g. mean, sum)

#Calculate mean of Overall.Score column with apply function
apply(suv_df[c("Overall.Score")],2,mean) #Calculates mean of columns
apply(suv_df[c("Overall.Score","Acceleration..0.60..Sec")],2,sd) #Calculates standard deviation of columns

#user defined functions with apply
range01 <- function(x){(x-min(x))/(max(x)-min(x))} #Function to normalize values to range 0-1
suv_df$Overall.Score.norm <- range01(suv_df$Overall.Score) #Normalizes Overall.Score column using function range01
suv_df$Overall.Score.norm

suv_df$Overall.Score.norm <-apply(suv_df[c("Overall.Score")],2,range01)
suv_df$Overall.Score.norm


merge_str_cols <-function(row){
  return (paste(row["Make"],row["Model"],sep="_"))
}

suv_df$Make_Model <- apply(suv_df,1,merge_str_cols)
suv_df$Make_Model

suv_df$Make_Model = paste(suv_df$Make, suv_df$Model, sep="_") #Concatenates Make and Model columns with underscore separator
suv_df$Make_Model

#grepl function
?grepl
grepl("Toyota",suv_df$Make, fixed=TRUE) #Searches for string "Toyota" in Make column; returns TRUE or FALSE
suv_df[grepl("Toyota",suv_df$Make, fixed=TRUE),] #Extracts rows where Make is "Toyota"
#fixed=TRUE makes the search case-sensitive and checks the string as it is


#Read and write csv, excel....

recom_df = suv_df[suv_df$Recommended=="Yes",]
View(recom_df)

write.csv(recom_df,"week1/data/recommended.csv")
#install readxl package writexl
getwd()
read.csv("week1/data/recommended.csv")


install.packages("readxl")
install.packages("writexl")

library(readxl)
library(writexl)

write_xlsx(recom_df,"test.xlsx")
dfex = read_excel("test.xlsx")
View(dfex)

names(suv_df)
suv_df[,c("Make","Model")]


