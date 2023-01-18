# Data Analysis for Tetrastichus planipennisi field experiments-Supercooling Point and Emergence Data 
library(tidyverse)
library(lubridate)
library(ggplot2)
library(PerformanceAnalytics)
library(moments)
library(gridExtra)
library(forecast)
library(pracma)
library(DescTools)

#SCP Experiment Plots
#read in data 

#Data first moments
#Box Plots
#Analyze data in time series
setwd("~/OneDrive/Documents/R/Tet/SCP_Files")
stats<-read.csv("SCP_Nov_Apr_Outliers_Compiled.csv")
stats<-read.csv("SCP_Nov_Apr_Compiled.csv")

all(stats$Month %in% month.name) # Validate month names
stats$Month<- factor(stats$Month, levels=c("November", "December", "January", "February", "March", "April"))
edit_stats<-stats%>%
  mutate(Treat=substr(Jar,3, nchar(Jar)), Treatment=ifelse(Treat=="Dense",yes = "Treatment", no="Control"), Block=substr(Jar, start=1, stop=1))%>%
  mutate(Treatment=as.factor(Treatment))


plot<-ggplot(data=edit_stats, mapping=aes(x=Month, y=SCP_Value, fill=Treatment))+geom_boxplot(position="identity",alpha=0.5)+facet_wrap(~Block,ncol=1)+ylim(-30,-18)+ggtitle("SCP Values of Tets Winter 2021-22")+theme(axis.text.x=element_text(angle=25))+ylab(expression("Temperature" ( degree*C)))
plot

x<-stats%>%
  group_by(Jar,Month)%>%
  summarise(MeanSCP=mean(SCP_Value))%>%
  mutate(Block=substr(Jar, 1,1),Treat=substr(Jar,3, nchar(Jar)), Treatment=ifelse(Treat=="Dense",yes = "Treatment", no="Control"),month=match(Month, month.name))%>% #Month=as.POSIXct(month,format="%m")
  ungroup()%>%
  select(Treatment, Block, Month, MeanSCP)

#Blocks-5_1=Block 1, 11_1=BLock 2, 11_2=Block 3

plot<-ggplot(data=x, aes(x=Month, y=MeanSCP, col=Treatment))+geom_boxplot()
plot


plot<-ggplot(data=x, mapping=aes(x=Month, y=MeanSCP, col=Treatment))+geom_point(size=2)+facet_wrap(~Block, ncol=1)+ggtitle("Mean SCP Values of Tets Winter 2021-22")+theme(axis.text.x=element_text(angle=25))+ylab(expression("Temperature" ( degree*C)))
plot

linmod<-lm(data=x, formula=MeanSCP~Treatment+Month+Block)
summary(linmod)

#Create treatment col, block (as character string) from Jar col, Time/Date (convert), average SCP. 

#Overlay treatments per block plot and see the differences 

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


#Try linear model where you knockdown time as a function of treatment by time. 
#knockdown~Treatment*time+site

#x<-lm(data=stats,formula = SCP_Value~)


#iButton Data Logger Temperature Plots
#Time Series Plots
#Future plots/Analysis needed
setwd("~/OneDrive/Documents/R/Tet/SCP_Files/Processed_Files/")
ibutton<-read.csv("iButton_All.csv")

datalogger<-ibutton%>%
  group_by(Group)%>%
  mutate(Group=as.factor(Group),Temp=as.numeric(Temp), Block=as.factor(Block), DateTime=mdy_hm(DateTime))%>%
  ungroup()%>%
  mutate(Treatment=as.factor(case_when(str_detect(ibutton$Group, "U") ~ "Control", TRUE ~ "Treatment")))%>%
  select(-X)%>%
  filter(Block==1)

#Plot ibutton logger for each month 
block1<-ggplot(data=datalogger, mapping=aes(x=DateTime, y=Temp))+geom_line()+facet_wrap(~Treatment)+geom_hline(yintercept = 0)+ylab("Temperature C")+xlab("Time")+ggtitle("iButton Datalogger Temperature Records per Jar")
block1 

block1<-ggplot(data=datalogger, mapping=aes(x=DateTime, y=Temp))+geom_line()
block1


#Plot multiple time series (Urban vs. dense per group and facet)
pdf("Ibutton_Logger")
plot<-ggplot(data=datalogger, mapping = aes(x=DateTime, y=Temp, group=1))+geom_line()+facet_wrap(vars(Block,Treatment))+geom_hline(yintercept = 0)+ylab("Temperature C")+xlab("Time")+ggtitle("iButton Datalogger Temperature Records per Jar")
plot

class(datalogger$Block)

moments<-data_all%>%
  group_by(Group)%>%
  summarise(mean=mean(Tempc), max=max(Tempc), min=min(Tempc),var=var(Tempc), stdev=sd(Tempc), skew=(sum((Tempc - mean(Tempc))^3)/length(Tempc))/
              ((sum((Tempc - mean(Tempc))^2)/length(Tempc)))^(3/2),kurtosis=kurtosis(Tempc))


#Emergence/Eclosion Temperature Plots
setwd("~/OneDrive/Documents/R/Tet/Emergence")

emerge<-read.csv("Emergence_Spreadsheet.csv")

#Box Plots-Treatment vs. Control

edit<-emerge%>%
  group_by(Group,Treatment)%>%
  summarize(Tets=sum(Tetrastichus), Failed=sum(Tetrastichus.Failed.to.Eclose))

plot<-ggplot(data=edit, mapping=aes(x=Treatment, y=Tets))+geom_boxplot()
plot

plot2<-ggplot(data=emerge, mapping=aes(x=Treatment, y=Tetrastichus))+geom_boxplot()+facet_wrap(~Group)
plot2

plot3<-ggplot(data=emerge, mapping=aes(x=Treatment, y=Tetrastichus))+geom_boxplot()
plot3

ANOVA<-aov(data=edit, Tets~Treatment)
fail<-aov(data=edit, Failed~Treatment)
summary(ANOVA)
summary(fail)

hist<-ggplot(data=emerge, mapping=aes(x=emerge$Tetrastichus, color=Treatment))+geom_histogram(bins=30)
hist
#Future Plots
