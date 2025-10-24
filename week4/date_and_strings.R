### some string functions

name = "jack"
lastname = "william"

paste(name,lastname,sep="-")

#%s -> string placeholder
sprintf("name is %s lastname is %s",name,lastname)

#%d int, %f float %.2f, %g general number 

sprintf("The total amount is %.2f",12.23423234)


#splitting strings:

parts = strsplit("jack lovers football",split = " ")
parts

for(val in parts){
  print(val)
}

#substringing string

print("$123,345")

aNumber = "$123,345"

aNumber2 = sub(",","",aNumber)

aNumber3 = sub("\\$","",aNumber2)
as.numeric(aNumber3)

####Date type############

date() # return todays date and time

Sys.Date()

today = Sys.Date()
today

format(today,"%d/%b/%y")  # 24/10/2025  # 2025  # Oct

weekdays(today)
months(today)

format(today,"%B")

year = as.numeric(format(today,"%Y"))
year

dt = as.Date("2025-10-01")
dt

format.Date(dt,format = "%Y")
format.Date(dt,format = "%m")
format.Date(dt,format = "%d")
strftime(dt,format = "%d-%m-%Y") # only works dates, returns the formatted date as string
strftime(dt,format="%Y-%m-%d")

df <- read.csv("week4/data/Q4_bankfailures.csv")
df

str(df)

closing_date <- as.Date(df$Closing.Date,format = "%d.%b.%y")
closing_date

strftime(closing_date,format="%Y")
strftime(closing_date,format="%b")
df$Closing.Year = strftime(closing_date,format="%Y")
df$Closing.Year
df$Closing.Year = as.numeric(df$Closing.Year)
df$Closing.Year











