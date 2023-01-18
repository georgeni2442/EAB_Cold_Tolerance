library(tidyverse) #Load in relevant packages you might need
library(ggplot2)
library(plotly)


#Tasks

#Extract temperature log data from the .csv file, place into excel sheet. 
setwd()
#Read in the .csv file
x<-read.csv("SCP_Mar.csv")



#Use pipe operator to create new variable to work with, may need to read documentation
scp<-x%>%
  na.omit()%>%
  mutate(DateTime=as.POSIXctpaste(Record_Date,Record_Time))"%Y-%m-%d %H:%M:%S"

#characteristics of a df
str(scp$DateTime)
class(scp$DateTime)


#Drop NAs

#Create new variable (ex. DateTime) to convert date/time columns into as.POSIXCT format

#Manipulate columns of interest

#Create new columns with specific grouping data 

#Spread dataframe to long format (pivot_longer function)


#Plot Data using ggplot2

#make it interactive using ggplotly()
