#Read Monthly Temperature Data in plotly 
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(PerformanceAnalytics)
library(moments)
library(gridExtra)
library(forecast)
library(pracma)
library(DescTools)
#Read in data
#Grab all files with form "ibutton"
setwd("~/OneDrive/Documents/R/Tet/SCP_Files/Full_Ibutton_Logs") #This folder has all full ibutton logs (17 items because of malfunctions in one ibutton)

#filelist<-list.files(pattern="Ibutton") #For previous set
filelist<-list.files(pattern="Full") #Get list of all files with "Full" in the filename.

#Read in files from input list, provide conversions, then rewrite tables as output.
for (i in 1:length(filelist)){
  input<-filelist[i]
  output<-paste0(input)
  print(paste("Processing the file:", input))
  data <- read.csv(input)
  data<-data%>%
    mutate(Group=Group,Temp=as.numeric(Temperature), DateTime=mdy_hm(DateTime))%>%
  select(DateTime,Group,Temp)
  setwd("~/OneDrive/Documents/R/Tet/SCP_Files/Processed_Files")
  write.table(data, file=output, sep=",", col.names=TRUE, row.names=FALSE)
setwd("~/OneDrive/Documents/R/Tet/SCP_Files/Full_Ibutton_Logs")
}

input<-"11_1_2_U_Full.csv"
x<-read.csv("11_1_2_U_Full.csv")

data<-x%>%
  mutate(Group=Group,Temp=as.numeric(Temperature), DateTime=mdy_hm(DateTime))
class(data$X)


setwd("~/OneDrive/Documents/R/Tet/SCP_Files/Processed_Files/")

data_all<-list.files(path="~/OneDrive/Documents/R/Tet/SCP_Files/Processed_Files/", pattern="Full")%>%
  lapply(read_csv)%>%
  bind_rows%>%
  mutate(Group=as.factor(Group))


write.csv(data_all,"~/OneDrive/Documents/R/Tet/SCP_Files/Processed_Files/iButton_All.csv", row.names=TRUE)

#Start here

setwd("~/OneDrive/Documents/R/Tet/SCP_Files/Processed_Files/")
data_all<-read.csv("iButton_All.csv")
data_edit<-data_all%>%
  mutate(Treatment=case_when(str_detect(data_all$Group, "U") ~ "Control", TRUE ~ "Treatment"))%>%
  mutate(Jar=as.numeric(str_match(Group, '([^_]+)(?:_[^_]+){1}$')[,2]))%>%
  group_by(Block)%>%
  mutate(Pair=case_when(Jar==1~1, Jar==2~2, Jar==3~3, Jar==4~1, Jar==5~2, Jar==6~3))


#Clean Data/Formatting

#Plot multiple time series (Urban vs. dense per group and facet=)
plot<-ggplot(data=data_edit, mapping = aes(x=DateTime, y=Temp, fill=Pair))+geom_line(alpha=0.5)+facet_wrap(~Block)+geom_hline(yintercept = 0)+ylab("Temperature C")+xlab("Time")+ggtitle("iButton Datalogger Temperature Records per Jar")
plot


#Need to plot 17 individual plots-we could group them together? Either that or maybe overlay them together based on groups to see how they differ? You'd want to see the treatments overlaid on top of each other though so you can spot any differences.



#Time Series Plot of every individual jar, colored by Treatment 
tplot<-ggplot(data=data_edit, mapping=aes(x=DateTime, y=Temp, group=1))+geom_line(aes(color=Treatment))+facet_wrap(Block~Pair)
tplot
#Omit plots without full time series?
#Plots w/o: 11_1_4_D. 11_2_3_U. 5_1_4_D. 
#Or just group them anyways and see what happens
sliced<-data_edit%>%
  filter(!str_detect(Group,"11_1_4_D|11_2_3_U|5_1_4_D"))


#So a single plot will have for example jar 1 and 4, 2 and 5, 3 and 6. That way each plot will have a Control and Treatment plot overlaid on top of each other
tplot<-ggplot(data=sliced, mapping=aes(x=DateTime, y=Temp, group=1))+geom_line(aes(color=Treatment))+facet_wrap(Block~Pair)
tplot

#Time Series Plot of every jar grouped by Block and colored by Treatment overlaid on top of each other? Could be too messy


#Time Series Plot of every jar grouped by Block, and colored by Treatment overlaid on top of each other, and paired up. 

#Time Series Plot of every jar grouped by Block and paired by Treatment. Temperatures summarized to: 24 hr period mean temperature 
daily<-data_edit%>%
  group_by(Group)%>%
  mutate(DateTime=mdy_hm(DateTime), Treatment=as.factor(Treatment))%>%
  group_by(Month=month(DateTime),Day=day(DateTime),  Group) %>% 
  mutate(DailyMean = mean(Temp))

dailyplot<-ggplot(data=daily, mapping=aes(x=DateTime, y=DailyMean, group=1))+geom_line(aes(color=Treatment))+facet_wrap(Block~Pair)

dailyplot

class(data_edit$DateTime)
#Time Series Plot of every jar grouped by Block and paired by Treatment. Temperatures summarized to: weekly mean temperature 
week<-data_edit%>%
  group_by(Group)%>%
  mutate(DateTime=mdy_hm(DateTime), Treatment=as.factor(Treatment))%>%
  group_by(Month=month(DateTime), Week=week(DateTime), Day=day(DateTime), Group)%>%
  mutate(WeeklyMean=mean(Temp))
  
weeklyplot<-ggplot(data=week, mapping=aes(x=DateTime, y=WeeklyMean, group=1))+geom_line(aes(color=Treatment), alpha=0.4)+facet_wrap(Block~Pair)

weeklyplot

#Violin plot  

#Make it a plotly function? 

#Analysis-Average/Min/Max/Intervals? For each month/week find mean temperature

moments<-data_all%>%
  group_by(Group)%>%
  summarise(mean=mean(Temp), max=max(Temp), min=min(Temp),var=var(Temp), stdev=sd(Temp), skew=(sum((Temp - mean(Temp))^3)/length(Temp))/
              ((sum((Temp - mean(Temp))^2)/length(Temp)))^(3/2),kurtosis=kurtosis(Temp))

# First Four Moments
#1-Mean-Mean temperature is lower for all 3 Dense groups compared to their urban counterparts. This is in-line with our expectation of some temperature change that we've enacted due to our treatment. Each urban treatment also has higher maximum temperature values experienced overall compared to their dense treatment.Urban treatments also have lower minimum values experienced 
#2-Variance-High variance across treatments
#3-Skewness-Site 2 has more skew, all negative
#4-Kurtosis-negative kurtosis=platykurtic, meaning the distribution is flatter (less peaked) when compared with the normal distribution. 
#Next: Create plots of four moments for analysis

#Next Steps: New Plot: At each time point look at average change between D and U groups across time. The null hypothesis is that the diff is 0, but you might see some change across the 0 up and down. Look at the difference between the outcomes at time points. After that plot SC on Y and some factor of Temperature on the X (mean), with groups divided by type (Sheltered vs. control) instead of anything else

#Urban=control, Dense=Sheltered
#First variables to look at temperature data are mean and maximum, pull out daily maxes
#Perhaps control vs. shade-shade is reducing sunlight available. 

#Use lubridate to round time intervals to half hour periods (Every half hour), then find the difference between D and U at each half-hour time point. 



#Mean Daily Temperature Differences between Sheltered and Control Groups.
dailyave<-data_all%>%
  mutate(round=round_date(DateTime,unit="day"))%>%
  group_by(Day=as.Date(round), Group)%>%
  summarise(d=mean(Tempc))%>%
  mutate(rep=substr(Group,1,1))%>%
  group_by(Day,rep)%>%
  summarise(difference=diff(d))

plot<-ggplot(data=dailyave, mapping = aes(x=Day, y=difference))+geom_line()+facet_wrap(~rep,nrow=3)+geom_hline(yintercept = 0)+ggtitle("Mean Daily Temp Diff b/t Treatments")+ylab("Daily Diff C")
plot


#Daily Differences between Sheltered vs. Control Groups
time<-data_all%>%
  mutate(round=round_date(DateTime,unit="day"))%>%
  group_by(Day=as.Date(round), rep)%>%
  summarise(difference=diff(Tempc))

plot<-ggplot(data=time, mapping = aes(x=Day, y=difference))+geom_line()+facet_wrap(~rep,nrow=3)+geom_hline(yintercept = 0)
plot



#Half Hour Differences between Sheltered vs. Control Groups- U-D, so negative values mean that sheltered groups experienced higher temperatures than controls, and positive values mean sheltered groups experienced lower temperatures. 
time<-data_all%>%
  mutate(round=round_date(DateTime,unit="30 mins"))%>%
  group_by(rep,round)%>%
  summarise(diff=diff(Tempc))

plot<-ggplot(data=time, mapping = aes(x=round, y=diff))+geom_line()+facet_wrap(~rep,nrow=3)+geom_hline(yintercept = 0)+ggtitle("Half hour Diff b/t Groups")
plot

#Daily Maxes for each Group-differences between groups
dailymax<-data_all%>%
  mutate(round=round_date(DateTime,unit="day"),rep=substr(Group,1,1))%>%
  group_by(Group,round)%>%
  summarise(Max=max(Tempc),rep=rep)



dailymaxplot<-ggplot(data=dailymax, mapping = aes(x=round, y=Max))+geom_line()+facet_wrap(~Group,nrow=3, ncol=2)+ggtitle("Daily Max")+ylab("Temperature C")
dailymaxplot

dailymax<-data_all%>%
  mutate(round=round_date(DateTime,unit="day"),rep=substr(Group,1,1))%>%
  group_by(Group,round)%>%
  summarise(Max=max(Tempc))%>%
  mutate(rep=substr(Group,1,1))%>%
    group_by(rep,round)%>%
   summarise(diff=diff(Max))

dailymaxdiffplot<-ggplot(data=dailymax, mapping = aes(x=round, y=diff))+geom_line()+facet_wrap(~rep,nrow=3)+ggtitle("Daily Max Diff b/t Groups")+ylab("Temperature C")+geom_hline(yintercept=0)
dailymaxdiffplot

#Daily Max Area Under Curve
library(pROC)
library(zoo)

areadat<-dailymax%>%
  group_by(rep)%>%
  mutate(julian=yday(round), month=month(round), year=year(round))%>%
  group_by(rep,month)%>%
  summarize(auc=AUC(julian,diff))
  
areadatmin<-dailymin%>%
  group_by(rep)%>%
  mutate(julian=yday(round), month=month(round), year=year(round))%>%
  group_by(rep,month)%>%
  summarize(auc=AUC(julian,diff))

monthSCP<-scp%>%
  group_by(Month)%>%
  mutate(rep=substr(Jar,1,1))%>%
  group_by(rep,Jar,Month)%>%
  summarise(avgSCP=mean(SCP_Value),Jar=Jar)%>%
  ungroup()%>%
  mutate(month=match(Month, month.name))%>%
  select(rep,avgSCP, month,Jar)%>%
  right_join(areadat)

monthSCPmin<-scp%>%
  group_by(Month)%>%
  mutate(rep=substr(Jar,1,1))%>%
  group_by(rep,Month)%>%
  summarise(avgSCP=mean(SCP_Value))%>%
  ungroup()%>%
  mutate(month=match(Month, month.name))%>%
  select(rep,avgSCP, month)%>%
  right_join(areadatmin)

aucmaxplot<-ggplot(data=monthSCP, aes(x=auc, y=avgSCP, color=Jar))+geom_point(size=3)+geom_smooth(method="lm")+ggtitle("Avg. SCP vs. AUC for daily Max temps")
aucmaxplot

aucminplot<-ggplot(data=monthSCPmin, aes(x=auc, y=avgSCP, color=rep))+geom_point()+geom_smooth(method="lm")+ggtitle("Avg. SCP vs. AUC for daily Min Temps")
aucminplot

#Daily Mins 
dailymin<-data_all%>%
  mutate(round=round_date(DateTime,unit="day"))%>%
  group_by(Group,round)%>%
  summarise(Min=min(Tempc))

dailyminplot<-ggplot(data=dailymin, mapping = aes(x=round, y=Min))+geom_line()+facet_wrap(~Group,nrow=3, ncol=2)+geom_hline(yintercept = 0)

dailyminplot
#Daily Min Diff
dailymin<-data_all%>%
  mutate(round=round_date(DateTime,unit="day"),rep=substr(Group,1,1))%>%
  group_by(Group,round)%>%
  summarise(Min=min(Tempc))%>%
  mutate(rep=substr(Group,1,1))%>%
  group_by(rep,round)%>%
  summarise(diff=diff(Min))

dailymindiffplot<-ggplot(data=dailymin, mapping = aes(x=round, y=diff))+geom_line()+facet_wrap(~rep,nrow=3)+ggtitle("Daily Min Diff b/t Groups")+ylab("Temperature C")+geom_hline(yintercept = 0)
dailymindiffplot


#Pull out Daily Maximum values

#Average diff. between D and U groups

#Controls are much warmer than the sheltered ones overall, the difference is quite large in Reps 1 and 2, but also the same for 3. So the sheltered jars are getting much less sunlight 


#Plot Supercooling on Y and Temperature on X (mean), and the groups divided by type. 
#Temperature can be the mean monthly temperature? So we have ~5 or 6 time points, and then the associated SCP points at each time point grouped by the type (Control vs. Sheltered)
scp<-scp%>%
  group_by(Month)%>%
  mutate(nMonth)

  
  
temp<-data_all%>%
  group_by(Month=floor_date(DateTime,"month"),Group)%>%
  left_join(scp,by=as.Date(Month))
  summarise(Mean=mean(Tempc))

plot<-ggplot(data=temp, mapping = aes(x=Month, y=Mean))+geom_point()+facet_wrap(~Group,nrow=, ncol=2)+geom_hline(yintercept = 0)
plot



pdf("Ibutton_Data_Moments.pdf")
grid.table(moments)
dev.off()


