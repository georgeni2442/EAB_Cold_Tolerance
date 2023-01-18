library(ggplot2)
library(tidyverse)
library(dplyr)
library(lubridate)
library(timetk)
library(plotly)

#Read in .csv file
setwd("~/OneDrive/Documents/R/Tet/SCP_Files")

#Read HuaTuo Logger Files   

#Make reading in files a little bit easier-maybe read through all of the files that have X structure? 
#Could use this to read in all files with pattern .csv-would need to create a separate folder just with the relevant files though
#temp = list.files(pattern="*.csv")
#myfiles = lapply(temp, read.delim)

#set working directory
setwd("~/OneDrive/Documents/R/Tet/SCP_Files")

#scp_read function-input (File name, Group), reads in thermocouple temp readouts and converts format. 

scp_read<-function(a,b){
  x<-read.csv(a, header=TRUE, na.strings=c("","NA"))
  clean<-x%>%
    drop_na()%>%
    #slice(6:length(row(x)))%>%
    # rename("Date"=V2, "Time"=V3)%>%
    mutate(DateTime=as.POSIXct(as.character(paste(DATE, TIME)), format="%m/%d/%Y %H:%M:%S"))%>%
    select(4:13)%>%
    filter(Group==b)%>% #Filtering requires manual input, maybe find a way of just making this generalizable as a function? 
    pivot_longer(cols = colnames(x[,4:11]),names_to = 'channel')
  p<-ggplot(clean, aes(x=DateTime, y=as.numeric(value)))+geom_line(colour="black")+facet_wrap(channel~.)
  p<-ggplotly(p)
 htmlwidgets::saveWidget(as_widget(p),"newplot.html")
}
scp_read("SCP_Nov.csv", "3_U")

y<-read.csv("SCP_Apr.csv")

#clean<-x%>%
#  drop_na()%>%
#slice(6:length(row(x)))%>%
# rename("Date"=V2, "Time"=V3)%>%
# mutate(DateTime=as.POSIXct(as.character(paste(DATE, TIME)), format="%m/%d/%Y %H:%M:%S"))%>%
# select(4:13)%>%
# filter(Group=='2_D')%>% #Filtering requires manual input, maybe find a way of just making this generalizable as a function? 
# pivot_longer(cols = colnames(x[,4:11]),names_to = 'channel')%>%
#3 filter(channel=="T1.oC")
#group_by(Group, Date)
#clean$Time<-as.POSIXct(clean$Time, format= "%H:%M:%S")

#Can make this a function that just returns p everytime
p<-ggplot(clean, aes(x=DateTime, y=as.numeric(value)))+geom_line(colour="black")+ylab("Temperature C")+xlab("Time")
p<-ggplotly(p)
p


#Reading in SCP Files for Data Analysis

 
scp_val<-read.csv("SCP_Nov_Apr_Compiled.csv")
 
 
scp<-scp_val%>%
  drop_na()%>%
  mutate(Group=as.factor(Group),Group_2=as.factor(Group_2))
  
t.test.res<-t.test(data=scp,SCP_Value~Group_2)
t.test.res

#Quick plot
ggplot(data=scp,SCP_Value~Group, main="SCP Acclimation Period")

ggplot(data = scp_val,mapping = aes(x=Group, y=SCP_Value, fill=Group))+geom_boxplot()+xlab("Chill Group")+ylab("SCP (\u00B0C)")+theme(legend.title=element_blank())+ggtitle("Chill Group Testing")


ggplot(data=scp,mapping=aes(x=Group, y=SCP_Value, fill=Group))+geom_boxplot()+xlab("Chill Group")+ylab("SCP (\u00B0C)")+ggtitle("Chill Groups Testing")

aovres<-aov(data=scp, formula=SCP_Value~Group)
aovres

class(clean$DateTime)
clean$Date<-mdy(clean$Date)
clean$Date<-as.Date(clean$Date)
clean$Time<-hms(as.character(clean$Time))
clean$Time <- format(as.POSIXct(strptime(clean$Time, "%H:%M:%S",tz="")) ,format = "%H:%M:%S")
clean$Time
clean$Time<-as.POSIXct(clean$Time, format= "%H:%M:%S")
#pivot_longer(cols = c("V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11"))%>%
#group_by(Date, Group)

ggplot(data=clean, aes(x=Time, y= value, col=name))+geom_line()


#Convert Time to date-time format

#Plots for all 8 samples at once

#Create 8 plots of each sample individually and store 

#Grab supercooling point from each plot (given an estimated range)

#Analyze data in time series

#Double check NA values showing up
stats<-read.csv("SCP_Nov_Apr_Compiled.csv")
all(stats$Month %in% month.name) # Validate month names
stats$Month<- factor(stats$Month, levels=c("November", "December", "January", "February", "March", "April"))
edit_stats<-stats%>%
  mutate(Treat=substr(Jar,3, nchar(Jar)), Treatment=ifelse(Treat=="Dense",yes = "Treatment", no="Control"))
          
plot<-ggplot(data=edit_stats, mapping=aes(x=Month, y=SCP_Value, fill=Jar))+geom_boxplot()+facet_wrap(~Jar,ncol=2)+ylim(-30,-20)+ggtitle("SCP Values of Tets Winter 2021-22")+theme(axis.text.x=element_text(angle=25))+ylab(expression("Temperature" ( degree*C)))
plot

x<-stats%>%
  group_by(Jar,Month)%>%
  summarise(MeanSCP=mean(SCP_Value))%>%
  mutate(Block=substr(Jar, 1,1),Treat=substr(Jar,3, nchar(Jar)), Treatment=ifelse(Treat=="Dense",yes = "Sheltered", no="Control"),month=match(Month, month.name))%>% #Month=as.POSIXct(month,format="%m")
  ungroup()%>%
  select(Treatment, Block, Month, MeanSCP)


plot<-ggplot(data=x, aes(x=Month, y=MeanSCP, col=Block))+geom_boxplot()
plot


plot<-ggplot(data=x, mapping=aes(x=Month, y=MeanSCP, col=Treatment, shape=Block))+geom_point(size=2)+ggtitle("Mean SCP Values of Tets Winter 2021-22")+theme(axis.text.x=element_text(angle=25))+ylab(expression("Temperature" ( degree*C)))
plot
lm(data=x, formula=MeanSCP~Treatment+month+Block)
#Create treatment col, block (as character string) from Jar col, Time/Date (convert), average SCP. 


hist<-ggplot(data=stats, aes(x=SCP_Value))+geom_histogram(fill="red")+stat_function(fun = function(x, mean, sd, n){
  n * dnorm(x = x, mean = mean, sd = sd)
}, 
args = with(stats, c(mean = mean(SCP_Value), sd = sd(SCP_Value), n= length(SCP_Value))))+scale_x_continuous("SCP Values")

hist
#Log transformation to see difference in means
log<-stats%>%
  mutate(log=log(SCP_Value+1-min(SCP_Value)))

hist<-ggplot(data=log, aes(x=log))+geom_histogram(fill="darkred")+ggtitle("SCP Histogram (Log-scaled)")
hist

plot<-ggplot(data=log, mapping=aes(x=Month, y=log, fill=Jar))+geom_boxplot()+facet_wrap(~Jar,ncol=2)
plot

#Test for normality using QQ Plot
qqnorm(stats$SCP_Value, pch=1, frame=FALSE)
qqline(stats$SCP_Value, col="steelblue", lwd=2)

#Test for normality using Shapiro-Wilks Test
shapiro.test(stats$SCP_Value)

#Statistically significant normality


#Area under the curve of each month vs. SCP values exhibited per group type plot

#Try linear model where you knockdown time as a function of treatment by time. 
#knockdown~Treatment*time+site

#x<-lm(data=stats,formula = SCP_Value~)

