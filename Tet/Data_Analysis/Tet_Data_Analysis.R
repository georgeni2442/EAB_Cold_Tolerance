# Data Analysis for Tetrastichus planipennisi field experiments-Supercooling Point and Emergence Data 
#options(scipen = 999) Turning Scientific Notation off
#options(scipen = 0) #Turning Scientific Notation Back on

#Loading in Packages
library(tidyverse)
library(lubridate)
library(PerformanceAnalytics)
library(moments)
library(gridExtra)
library(forecast)
library(pracma)
library(DescTools)
library(viridis)
library(TukeyC)
library(ggpubr)
library(patchwork)
library(ggforce)

#Notes:
#SCP Experiment Plots
#Blocks-5_1=Block 1, 11_1=BLock 2, 11_2=Block 3

#Read in data
setwd("~/OneDrive/Documents/R/EAB_Cold_Tolerance/Tet/SCP_Files")
stats<-read.csv("SCP_All_Sites_Testing.csv")
setwd("~/OneDrive/Documents/R/EAB_Cold_Tolerance/Tet/SCP_Files/Processed_Files/")
ibutton<-read.csv("iButton_All.csv")
setwd("~/OneDrive/Documents/R/EAB_Cold_Tolerance/Tet/Emergence")
emerge<-read.csv("Emergence_Spreadsheet.csv")



#Data Transformation and Dataframes

#Editing Stats to add treatment labels
#all(stats$Month %in% month.name) # Validate month names
stats$Month<- factor(stats$Month, levels=c("November", "December", "January", "February", "March", "April"))
edit_stats<-stats%>%
  mutate(Treat=substr(Jar,3, nchar(Jar)), Treatment=ifelse(Treat=="Dense",yes = "Sheltered", no="Control"), Block=substr(Jar, start=1, stop=1))%>%
  mutate(Treatment=as.factor(Treatment))

#Summarized version of stats for MeanSCP
meanSCP<-stats%>%
  group_by(Jar,Month)%>%
  summarise(MeanSCP=mean(SCP_Value))%>%
  mutate(Block=substr(Jar, 1,1),Treat=substr(Jar,3, nchar(Jar)), Treatment=ifelse(Treat=="Dense",yes = "Treatment", no="Control"),month=match(Month, month.name))%>% #Month=as.POSIXct(month,format="%m")
  ungroup()%>%
  select(Treatment, Block, Month, MeanSCP)

datalogger<-ibutton%>%
  group_by(Group)%>%
  mutate(Group=as.factor(Group),Temp=as.numeric(Temp), Block=as.factor(Block), DateTime=mdy_hm(DateTime))%>%
  ungroup()%>%
  mutate(Treatment=as.factor(case_when(str_detect(ibutton$Group, "U") ~ "Control", TRUE ~ "Treatment")))%>%
  select(-X)%>%
  filter(Block==1)

ecloseedit<-emerge%>%
  group_by(Group,Treatment)%>%
  summarize(Tets=sum(Tetrastichus), Failed=sum(Tetrastichus.Failed.to.Eclose))

maxtemps<-ibutton%>% #CHECK for tempreadouts
  group_by(Group)%>%
  mutate(Group=as.factor(Group),Temp=as.numeric(Temp), Block=as.factor(Block), DateTime=mdy_hm(DateTime))%>%
  ungroup()%>%
  mutate(Treatment=as.factor(case_when(str_detect(ibutton$Group, "U") ~ "Control", TRUE ~ "Treatment")))%>%
  select(-X)%>%
  group_by(Group)%>%
  mutate(Day=yday(DateTime))%>%
  group_by(Group, Day)%>%
  summarise(MaxC=max(Temp),DateTime=DateTime)

#Subset data for highs/lows
mintemps<-ibutton%>% #CHECK for tempreadouts
  group_by(Group)%>%
  mutate(Group=as.factor(Group),Temp=as.numeric(Temp), Block=as.factor(Block), DateTime=mdy_hm(DateTime))%>%
  ungroup()%>%
  mutate(Treatment=as.factor(case_when(str_detect(ibutton$Group, "U") ~ "Control", TRUE ~ "Treatment")))%>%
  select(-X)%>%
  group_by(Group)%>%
  mutate(MonthNum=month(DateTime), Month=month.name[MonthNum])%>%
  group_by(Group, Month, MonthNum)%>%
  summarise(MinC=min(Temp))


#Mean temp for 24 hrs, 1 week, 2 weeks, 1 month
meantemps<-ibutton%>% #CHECK for tempreadouts
  group_by(Group)%>%
  mutate(Group=as.factor(Group),Temp=as.numeric(Temp), Block=as.factor(Block), DateTime=mdy_hm(DateTime))%>%
  ungroup()%>%
  mutate(Treatment=as.factor(case_when(str_detect(ibutton$Group, "U") ~ "Control", TRUE ~ "Treatment")))%>%
  select(-X)%>%
  group_by(Group)%>%
  mutate(Day=yday(DateTime))%>%
  group_by(Group, Day)%>%
  summarise(MinC=min(Temp),Mean=mean(Temp), MaxC=max(Temp),DateTime=DateTime, Week=week(DateTime), Month=month(DateTime))

meanmonthlytemps<-ibutton%>% #CHECK for tempreadouts
  group_by(Group)%>%
  mutate(Group=as.factor(Group),Temp=as.numeric(Temp), Block=as.factor(Block), DateTime=mdy_hm(DateTime))%>%
  ungroup()%>%
  mutate(Treatment=as.factor(case_when(str_detect(ibutton$Group, "U") ~ "Control", TRUE ~ "Treatment")))%>%
  select(-X)%>%
  group_by(Group)%>%
  mutate(Month=month(DateTime))%>%
  group_by(Group, Month)%>%
  summarise(MinC=min(Temp),Mean=mean(Temp), MaxC=max(Temp),DateTime=DateTime, Week=week(DateTime), Month=month(DateTime))


#Do plots for weeks and months-*CHECK for tempreadouts
meanweeklytemps<-ibutton%>%
  group_by(Group)%>%
  mutate(Group=as.factor(Group),Temp=as.numeric(Temp), Block=as.factor(Block), DateTime=mdy_hm(DateTime))%>%
  ungroup()%>%
  mutate(Treatment=as.factor(case_when(str_detect(ibutton$Group, "U") ~ "Control", TRUE ~ "Treatment")))%>%
  select(-X)%>%
  group_by(Group)%>%
  mutate(Week=week(DateTime))%>%
  group_by(Group,Week)%>%
  summarise(MinC=min(Temp),Mean=mean(Temp), MaxC=max(Temp))

###########Not sure about above meanweeklytemps, let's try again to filter. 
#Get temperature readouts for every Day/Week/2 Weeks/Month for each Jar 
#Ibutton data:May has a huge gap in the dataset, so we're going to filter that out

#Combining the above dataframes into one large one. So we may be able to archive the other ones
tempreadouts<-ibutton%>% #Take the ibutton datalogger data
  filter(Group=="5_1_1_U" | Group== "5_1_5_D" | Group=="11_1_1_U" | Group=="11_1_5_D" | Group=="11_2_1_U" | Group== "11_2_4_D")%>% #Filter the data by these group names
  group_by(Group)%>% #Group by Group
  mutate(Group=as.factor(Group),Temp=as.numeric(Temp), Block=as.factor(Block), DateTime=mdy_hm(DateTime), Date=date(DateTime))%>%
  ungroup()%>% #Create new variables 
  mutate(Treatment=as.factor(case_when(str_detect(Group, "U") ~ "Control", TRUE ~ "Sheltered")))%>% #Create new column of Treatment that will be Control or Treatment
  dplyr::select(-X)%>% #Delete the X column
  mutate(Week=1+ as.numeric(Date - as.Date("2021-11-18")) %/% 7)%>% #Create a week column that is weekly intervals from the start date of the experiment.
  group_by(Group)%>% #Group by Group (Again?)
  mutate(MonthNum=month(DateTime), Month=month.name[MonthNum])%>% #Create a MonthNum, Month Column
  ungroup()%>%
  group_by(Group, Date)%>%
  mutate(DailyMean=mean(Temp), DailyMin=min(Temp), DailyMax=max(Temp))%>% #Calculate Daily Mean, Min, and Max of temperature
  ungroup()%>%
  group_by(Group,Week)%>%
  mutate(WeeklyMean=mean(Temp), WeeklyMin=min(Temp), WeeklyMax=max(Temp), WeekVar=var(Temp), WeeklyRange=(max(Temp)-(min(Temp))))%>% #Calculate Weekly Mean, Min, and Max of temperature
  ungroup()%>%
  group_by(Group, Month)%>%
  mutate(MonthlyMean=mean(Temp), MonthlyMin=min(Temp), MonthlyMax=max(Temp), Var=var(Temp), MonthlyRange=(max(Temp)-(min(Temp)))) #Calculate Monthly Mean, Min, and Max of temperature
  
#RENAME: ex is going to be data transformation for the two week mean prior to collection dates.

ex<-tempreadouts%>%
  mutate(TwoWeekDate=case_when(month(Date)=='11'~'2021-11-16', month(Date)=='12'~'2021-12-17', month(Date)=='1'~'2022-01-15', month(Date)=='2'~'2022-02-14', month(Date)=='3'~'2022-03-16', month(Date)=='4'~'2022-04-16'))%>%
  filter(Month=="November"|Month=="December"|Month=="January"|Month=="February"|Month=="March"|Month=="April")%>%
  group_by(Block, Month, Treatment)%>%
  mutate(TwoWeekTemp=case_when(Date>TwoWeekDate~Temp))%>%
  mutate(TwoWeekMeanTemp=mean(TwoWeekTemp, na.rm=TRUE))%>%
  drop_na()%>%
  ungroup()%>%
  group_by(Block, Date, Treatment)%>%
  mutate(TwoWeekMinTemp=min(TwoWeekTemp, na.rm=TRUE), TwoWeekMaxTemp=max(TwoWeekTemp, na.rm=TRUE))%>%
  group_by(Block, TwoWeekDate, Treatment)%>%
  mutate(TwoWeekMinTemp=mean(TwoWeekMinTemp), TwoWeekMaxTemp=mean(TwoWeekMaxTemp))%>%
  summarize(Block=Block, Treatment=Treatment, Month=Month, TwoWeekDate=TwoWeekDate, TwoWeekMinTemp=TwoWeekMinTemp, TwoWeekMeanTemp=TwoWeekMeanTemp, TwoWeekMaxTemp=TwoWeekMaxTemp, Var=Var, MonthlyRange=MonthlyRange)%>%
  distinct()%>%
  mutate(TwoWeekDate=ymd(TwoWeekDate))


durationtemps<-ibutton%>%
  filter(Group=="5_1_1_U" | Group== "5_1_5_D" | Group=="11_1_1_U" | Group=="11_1_5_D" | Group=="11_2_1_U" | Group== "11_2_4_D")%>% #Filter the data by these group names
  group_by(Group)%>% #Group by Group
  mutate(Group=as.factor(Group),Temp=as.numeric(Temp), Block=as.numeric(Block), DateTime=mdy_hm(DateTime), Date=date(DateTime))%>%
  ungroup()%>% #Create new variables 
  mutate(Treatment=as.factor(case_when(str_detect(Group, "U") ~ "Control", TRUE ~ "Sheltered")))%>% #Create new column of Treatment that will be Control or Treatment
  dplyr::select(-X)%>%
  group_by(Group)%>%
  mutate(MonthNum=month(DateTime), Month=month.name[MonthNum], Week=week(Date))%>%
  mutate(Period=cut(Date, "7 days"))%>%
  ungroup()%>%
  group_by(Group, Period)%>%
  mutate(WeeklyMean=mean(Temp), WeeklyMin=min(Temp), WeeklyMax=max(Temp))%>%
  filter(MonthNum=="11"|MonthNum=="12"|MonthNum=="1"|MonthNum=="2"|MonthNum==3|MonthNum==4)%>%
  mutate(Group=factor(Group, levels=c("5_1_5_D", "5_1_1_U", "11_1_5_D", "11_1_1_U", "11_2_4_D", "11_2_1_U")))%>%
  arrange(Block)

filttemp<-tempreadouts%>%
  filter(Date=="2022-1-1")


temps<-tempreadouts%>%
  select(2, 4, 5, 6,7, 8,15)%>%
  group_by(Date)%>%
  distinct()%>%
  group_by(Date,Block)%>%
  mutate(tempdiff=max(WeeklyMax))%>%
  group_by(Date,Block, Treatment)%>%
  mutate(tempdiff=WeeklyMax-tempdiff)


monthtemps<-tempreadouts%>%
  select(2, 4, 5, 6,8,16)%>%
  group_by(Date)%>%
  distinct()%>%
  group_by(Month,Block)%>%
  summarize(Group=Group, Block=Block, Treatment=Treatment, MonthlyMean=MonthlyMean, Month=Month)%>%
  distinct() #MonthlyMean is the Monthly Mean Temperatures

#Pull out the weekly maximum temperature differences between control and treatments
maxtempdiff<-tempreadouts%>%
  select(2, 4, 5, 6,7, 8,9,12)%>%
  filter(Month=="November"|Month=="December"|Month=="January"|Month=="February"|Month=="March"|Month=="April")%>%
  group_by(Date)%>%
  distinct()%>% 
  group_by(Date,Block)%>% #For every control/treatment of a block on a date... 
  mutate(tempdiff=max(DailyMax))%>% #Get the highest temperature between the two 
  group_by(Date,Block, Treatment)%>% #Then for every control/treatment
  mutate(tempdiff=DailyMax-tempdiff)%>% #Subtract that temperature from its temperature-the ones with maximum temperatures should be 0, the other should be lower than 0 as it's smaller. 
  group_by(Block,Week)%>%
  mutate(weekdiff=mean(tempdiff))%>%
  group_by(Date,Block,Treatment)%>%
  summarize(tempdiff=min(tempdiff), Treatment=Treatment, DailyMax=DailyMax, Week=Week, weekdiff=weekdiff )#Trying to eliminate the duplicates/reduce the amount of values for easier plotting 


#pull out the weekly minimum temperature differences between control and treatments
mintempdiff<-tempreadouts%>%
  select(2, 4, 5, 6,7,8,9,11)%>%
  filter(Month=="November"|Month=="December"|Month=="January"|Month=="February"|Month=="March"|Month=="April")%>%
  group_by(Date)%>%
  distinct()%>% 
  group_by(Date,Block)%>% #For every control/treatment of a block on a date... 
  mutate(tempdiff=min(DailyMin))%>% #Get the lowest temperature between the two 
  group_by(Date,Block, Treatment)%>% #Then for every control/treatment
  mutate(tempdiff=-(tempdiff-DailyMin))%>% #Subtract that temperature from its temperature-the ones with minimum temperatures should be 0, the other should be lower than 0 as it's larger. 
  group_by(Date,Block,Treatment)%>%
  summarize(tempdiff=min(tempdiff), Treatment=Treatment, DailyMin=DailyMin, Week=Week) #Trying to eliminate the duplicates/reduce the amount of values for easier plotting 


#Take out the temperature readings for May? Not sure if I still need this, probably need to delete
tempdiff<-tempreadouts%>%
  select(2,4,6,7, 8,9,14)%>%
  distinct()%>%
  group_by(Week, Treatment)%>%
  summarize(MeanWeeklyMin=mean(WeeklyMin))%>%
  ungroup()%>%
  group_by(Week)%>%
  mutate(MeanWeeklyMindiff=diff(MeanWeeklyMin))


tempsfilt<-temps%>% #Not sure what this one's for, might need to DELETE
  filter(Month=="November"|Month=="December"|Month=="January"|Month=="February"| Month=="March"| Month=="April"| Month=="May")%>%
  group_by(Group, Block,Treatment, Month)%>%
  summarize(WeeklyMax=WeeklyMax)%>%
  distinct()%>%
  group_by(Block,Month)%>%
  mutate(Difference=diff(WeeklyMax))

#Quick filter tempreadouts for November-April, may need to remove later DELETE
tempsubset<-tempreadouts%>%
  filter(Month=="November"|Month=="December"|Month=="January"|Month=="February"| Month=="March"| Month=="April")%>%
  mutate(Block=factor(Block, levels=c("1", "2", "3"))) #Make all Blocks factors


#Pulling out temperature differences and two-week min and average temperatures
#Step 1-Pull out the temperature data for Controls and Treatment at X time scale
#Step 2-Calculate the difference between Controls and Treatment at a particular time step 
#Step 3- rebind this to the dataset-dependent on the time scale of the data, not the treatments


tempreadouts$Block<-factor(tempreadouts$Block, levels=c("1", "2","3"))


#Calculating the min and average temperature for the two weeks preceding the sample dates.
#So the two week date comes from the stats object, but it needs to also be attached to the tempreadouts df so that you can calculate 
twoweeks<-edit_stats%>%
  mutate(CollectedDate=mdy(CollectedDate), TwoWeekDate=mdy(TwoWeekDate), MonthNum=month(CollectedDate), Block=as.factor(Block))
  

#Join the twoweeks dataframe to the monthtemps dataframe so that we can do the CPlot with all of the temperatures, not just the two week lag. 

twoweeks<-twoweeks%>%
  left_join(monthtemps,by=c("Month", "Treatment", "Block"))

mintwoweeks<-ibutton%>%
  filter(Group=="5_1_1_U" | Group== "5_1_5_D" | Group=="11_1_1_U" | Group=="11_1_5_D" | Group=="11_2_1_U" | Group== "11_2_4_D")%>%
  group_by(Group)%>%
  mutate(Group=as.factor(Group),Temp=as.numeric(Temp), Block=as.factor(Block), DateTime=mdy_hm(DateTime), Date=date(DateTime))%>%
  ungroup()%>%
  mutate(Treatment=as.factor(case_when(str_detect(Group, "U") ~ "Control", TRUE ~ "Treatment")))%>%
  dplyr::select(-X)%>%
  group_by(Group)%>%
  mutate(MonthNum=month(DateTime), Month=month.name[MonthNum])

#for loop for each month, where you iterate through each month, and then filter between the dates of twoweeks' CollectedDate and TwoWeekDate

p<-unique(mintwoweeks$MonthNum)[-c(7,8,9)]
counter<-0
y<-list()
x<-list()
data<-data.frame(matrix(NA, nrow=6, 6))

#For loop to extract mean temperature for each Jar for two weeks preceding collection date. Can rerun this to get the min temp instead.
for(i in p){
  counter<-counter+1
  scpedit<-twoweeks%>%
    filter(MonthNum==i)
  
  edit<-mintwoweeks%>%
    filter(MonthNum==i)%>%
    filter(Date <=as.Date(unique(scpedit$CollectedDate)) & Date >= as.Date(unique(scpedit$TwoWeekDate)))%>%
    mutate(Day=day(Date))%>%
    group_by(Group, Day)%>%
    summarize(Min=min(Temp))%>%
    ungroup()%>%
    group_by(Group)%>%
    summarize(MeanMin=sum(Min)/14)
  
  x[[counter]]<-edit$Group
  
  colnames(data)<-x[[counter]]
  
  data[counter,]<-edit$MeanMin
}

data$MonthNum<-unique(twoweeks$MonthNum)

meantemp<-data%>%
  pivot_longer(cols = 1:6, names_to = "Jar", values_to = "Mean")%>%
  mutate(Treatment=as.factor(case_when(str_detect(Jar, "U") ~ "Control", TRUE ~ "Sheltered")), Block=as.factor(rep(x=c(2,2,3,3,1,1), times=6)), Mean=as.numeric(Mean))

meantempfilt<-data%>%
  pivot_longer(cols = 1:6, names_to = "Jar", values_to = "Mean")%>%
  filter(MonthNum=="2")%>%
  mutate(Treatment=as.factor(case_when(str_detect(Jar, "U") ~ "Control", TRUE ~ "Sheltered")), Block= as.factor(c(2,2,3,3,1,1)))

joined<-left_join(twoweeks, meantemp,by=c("Treatment", "Block", "MonthNum")) 

fullData<-joined%>%
  group_by(Block, Jar.y)%>%
  mutate(MeanSCP=mean(SCP_Value))%>% #MeanSCP is mean SCP per jar/block
  group_by(Block, Treatment,Month)%>%
  mutate(MeanMonthlySCP=mean(SCP_Value))%>% #MeanMonthlySCP is mean SCP per treatment/block/month
  mutate(Month=factor(Month, levels=c("November", "December", "January", "February", "March", "April")))%>%
  group_by(Month, Treatment,Block)%>%
  mutate(MeanSCP=mean(SCP_Value)) 

join<-fullData%>%
  left_join(ex, by=c("TwoWeekDate", "Treatment", "Block"))%>%
  mutate(Month=Month.x)


#For B Plot
#Plot weekly temperatures and the differences between them 
#Let's grab the weekly temperatures and clean up the dataset a bit
weeklytemps<-tempreadouts%>%
  select(Group,Date,Block, Treatment,Week, Month, WeeklyMax,WeeklyMin)%>%
  distinct()%>%
  filter(Month=="November"|Month=="December"|Month=="January"|Month=="February"|Month=="March"|Month=="April")%>%
  group_by(Block, Week)%>%
  mutate(maxtemp=max(WeeklyMax), mintemp=min(WeeklyMin))%>%
  group_by(Block, Date)%>%
  mutate(maxdiff=diff(WeeklyMax),mindiff=diff(WeeklyMin))%>%
  summarize(maxdiff=maxdiff, Date=Date, Block=Block, mindiff=mindiff)%>%
  mutate(Group=ifelse(mindiff<=0, "Control", "Sheltered"))%>%
  distinct()
#Filter tempreadouts for a smaller subset of data (November-April)
subset<-tempreadouts%>%
  select(Group,Date,Block, Treatment,Week, Month, WeeklyMax,WeeklyMin, Var, WeekVar, WeeklyRange, MonthlyRange)%>%
  distinct()%>%
  filter(Month=="November"|Month=="December"|Month=="January"|Month=="February"|Month=="March"|Month=="April")
  
subset$Month<- factor(subset$Month, levels=c("November", "December", "January", "February", "March", "April"))

#Combining join and subset to have the range and SCP values in the same dataset
tempreadoutfilt<-subset%>%
  group_by(Month, Treatment, Block)%>%
  summarize(Var=Var, MonthlyRange=MonthlyRange)%>%
  distinct()


combined<-subset%>%
  summarize(Month=Month,Block=Block, Treatment=Treatment, MonthlyRange=MonthlyRange, Var=Var )%>%
  distinct()%>%
  left_join(join, by=c("Month", "Block","Treatment"))
  
  
   #Too many duplicaes of monthly range and variance-think about simplifying this dataset.
#Plotting

tempreadouts$Month<- factor(tempreadouts$Month, levels=c("November", "December", "January", "February", "March", "April"))
#Variance boxplot
boxplot<-ggplot(data=tempreadouts, mapping=aes(x=Month, y=Temp, col=Treatment))+facet_wrap(~Block, ncol=3)+theme_bw()+geom_boxplot()+scale_color_manual(values=c("green","purple"))+theme(axis.text.x=element_text(angle=45, vjust=0.5, hjust=1))
boxplot

MonthVar<-ggplot(data=subset, mapping=aes(x=Month, y=Var, fill=Treatment,col=Treatment, group=Treatment))+facet_wrap(~Block, ncol=3)+theme_bw()+geom_line(size=1.5, aes(alpha=Treatment))+geom_point(shape=21,fill="black", size=2.5)+scale_color_manual(values=c("green","purple"))+theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))+ylab("Temperature Variance")+scale_fill_manual(values=c("green","purple"))+theme(text=element_text(family="Times New Roman"))+scale_alpha_manual(values=c(0.7, 0.5))+theme(axis.text.x=element_text(face="bold", size=10))

MonthVar #Think about making this a line chart, and also adding the range of temperatue as a single figure. #For the points, change the point type for 21-circle filled with a color 

#New figure-for plot C-plotting the x axis on a common scale, ignoring month-and then plotting all the points on two lines (Control and Shelter). Do the same thing for the maximum temperature (with a different scale)
#Main argument- treatments are affecting the temperature, and the temperature is affecting the SCP. Graphs 1-3 are arguing this.

#Another graph-SCP vs. Range 
#SCP on the y-axis. x axis-the temperature range? 

#Monthly pattern is more clear (In my opinion) than weekly patterns, so maybe find a way to make that more clear? I don't think it can be a boxplot though, so maybe just a point or some other visualization?

#A Plot-3 Different versions, Just pick one, each work fairly well
plot<-ggplot(data=fullData, mapping=aes(x=Month, y=SCP_Value, fill=Treatment))+facet_wrap(~Block, ncol=3)+geom_boxplot(outlier.shape=NA)+theme_bw() + geom_point(col="grey", alpha=0.5)+scale_fill_manual(values=c("green","purple"))+theme(axis.text.x=element_text(angle=45, vjust=0.5, hjust=1))+geom_line(mapping = aes(x=Month, y=MeanMonthlySCP,col=Treatment, group=Treatment))+scale_color_manual(values=c("green","purple"))+theme(panel.spacing = unit(2, "lines"))
plot

plot<-ggplot(data=fullData, mapping=aes(x=Month, y=SCP_Value,fill=Treatment))+geom_boxplot(outlier.shape=NA, position="identity",alpha=0.5)+facet_wrap(~Block, ncol=3)+scale_fill_manual(values=c("green", "purple"))+theme_bw()+geom_point(col="grey", alpha=0.5)+theme(axis.text.x=element_text(angle=45, vjust=0.9, hjust=1))+geom_line(data=fullData, mapping = aes(x=Month, y=MeanMonthlySCP, col=Treatment, group=Treatment))+scale_color_manual(values=c("green","purple"))+ylab("Mean Supercooling Point (\u00B0C)")+theme(text=element_text(family="Times New Roman"))
                                
plot 

ggsave("APlot.png", plot=plot)


Aplot<-ggplot(data=fullData, mapping=aes(x=Month, y=SCP_Value))+facet_wrap(~Block, nrow=3)+geom_boxplot(aes(fill=Treatment),outlier.shape=NA)+ylab("Supercooling Point Value")+ggtitle("SCP Values of Tetrastichus Winter 2021-22")+theme_bw()+scale_fill_manual(values=c("green","purple"))+geom_line(data=fullData, mapping = aes(x=Month, y=MeanMonthlySCP,col=Treatment, group=Treatment))+scale_color_manual(values=c("green","purple"))
Aplot



#B Plot-Plot Env Temperature vs. Time. So First find the ibutton logger temperatures that were experienced, maybe across every X time period and do the mean/raw temperature experienced across time. Readouts imply that monthly mean and monthly min might not be very useful as they're visually very similar, but there may be a sig difference in the monthly maximum temperature occurred between treatments.Even if we don't see a visually striking difference, there is however evidence from previous literature that the minimum temperatures experienced do seem to have an effect on it. 
WeeklyMaxplot<-ggplot(data=subset, mapping=aes(x=Date, y=WeeklyMax, col=Treatment))+geom_line()+facet_wrap(~Block)+theme_bw()+scale_color_manual(values=c("darkgreen", "purple"))+theme(axis.text.x=element_text(angle=45, vjust=0.5, hjust=1))+theme(text=element_text("Times New Roman")) +ylab("Weekly Maximum Temperatures (\u00B0C)")
WeeklyMaxplot


maxbarplot<-ggplot(data=weeklytemps)+geom_bar(aes(x=Date, y=maxdiff, fill=ifelse(maxdiff<0, 'green', 'purple')), stat="identity")+facet_wrap(~Block)+scale_fill_manual(values=c("green", "purple"))+theme_bw()+theme(axis.text.x=element_text(angle=45, vjust=0.5, hjust=1))+ylab("Difference in Treatment Temperatures (\u00B0C)")+theme(text=element_text("Times New Roman"))+theme(legend.position="None")+xlab("Month")+geom_hline(yintercept = 0)

maxlineplot<-ggplot(data=weeklytemps, aes(x=Date, y=maxdiff))+geom_link2(lwd=0.5,aes(color=after_stat(y<0)))+scale_color_manual(values=c("purple", "darkgreen"))+facet_wrap(~Block)+theme_bw()+theme(axis.text.x=element_text(angle=45, vjust=0.5, hjust=1))+ylab("Difference in Temperatures (\u00B0C)")+theme(text=element_text("Times New Roman"))+theme(legend.position="None")+xlab("Month")
maxlineplot
#Notes from Nick: Change the barplot so that there's an origin, and then just the difference rather than absolute values for it-also potentially just making it a line chart instead. 

maxbarplot   #Larger bar plots mean that for that treatment group, there is a larger difference (so it has a lower maximum temperature than the other)


WeeklyMaxplot

WeeklyMaxplot/maxlineplot

ggsave("BPlot.png")


minbarplot<-ggplot(data=weeklytemps, mapping=aes(x=Date, y=mindiff, fill=ifelse(mindiff<0, 'green', 'purple')))+geom_bar(stat="identity")+facet_wrap(~Block)+scale_fill_manual(values=c("green", "purple"))+theme(axis.text.x=element_text(angle=45, vjust=0.5, hjust=1))+ylab("Difference in Treatment Temperatures (\u00B0C)")+theme(text=element_text("Times New Roman"))+theme(legend.position="None")
minbarplot



minlineplot<-ggplot(data=weeklytemps, aes(x=Date, y=mindiff))+geom_link2(lwd=0.5,aes(color=after_stat(y<0)))+scale_color_manual(values=c("purple", "darkgreen"))+facet_wrap(~Block)+theme_bw()+theme(axis.text.x=element_text(angle=45, vjust=0.5, hjust=1))+ylab("Difference in Temperatures (\u00B0C)")+theme(text=element_text("Times New Roman"))+theme(legend.position="None")+xlab("Month")
minlineplot


WeeklyMinplot<-WeeklyMaxplot<-ggplot(data=subset, mapping=aes(x=Date, y=WeeklyMin, col=Treatment))+geom_line()+facet_wrap(~Block)+scale_color_manual(values=c("darkgreen", "purple"))+theme_bw()+theme(axis.text.x=element_text(angle=45, vjust=0.5, hjust=1))+theme(text=element_text("Times New Roman"))+ylab("Weekly Minimum Temperatures (\u00B0C)")
WeeklyMinplot


WeeklyMinplot/minlineplot

ggsave("BPlot_Min.png")
#Test visualization, can move or DELETE
diffplot<-ggplot(data=tempreadouts, mapping = aes(x=Date, y=DailyMax, col=Treatment))+facet_wrap(~Block)+geom_line(alpha=0.5)+theme_bw()+geom_bar(data=maxtempdiff, aes(x=Date, y=tempdiff, color="blue",fill="white"), stat="identity")

#B plots are going to be new, they're going to be of the variance/range-plotted next to each other for the mean minimum temperature. Let's do this first, and replace it on the page.

Bplot<-ggplot(data=tempreadouts, mapping = aes(x=Date, y=WeeklyMax,col=Treatment))+facet_wrap(~Block)+geom_line()+theme_bw()+scale_color_manual(values=c("green","purple")) #Add the box plot here!

Bplot

#Delete the Month axis for Month Var
WeekVar<-ggplot(data=subset, mapping=aes(x=Week, y=WeekVar, col=Treatment))+facet_wrap(~Block, ncol=3)+theme_bw()+geom_point()+scale_color_manual(values=c("green","purple"))+theme(axis.text.x=element_text(angle=45, vjust=0.5, hjust=1))+ylab(" Temperature Variance (\u00B0C)")+ggtitle("Weekly Temperature Variance")
WeekVar

rangeplot<-ggplot(data=subset, mapping=aes(x=Month, y=MonthlyRange, col=Treatment, group=Treatment))+theme_bw()+facet_wrap(~Block, ncol=3)+theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))+ylab("Temperature Range (\u00B0C)")+theme(text=element_text(family="Times New Roman"))+geom_line(size=1.5, aes(alpha=Treatment))+scale_color_manual(values=c("green","purple"))+scale_alpha_manual(values=c(0.7, 0.5))+geom_point(shape=21, fill="black",size=2.5)+theme(axis.text.x=element_text(face="bold", size=10))
rangeplot

rangeplot/MonthVar

filteredBplot<-ggplot(data=durationtemps, mapping = aes(x=Date, y=WeeklyMin,col=Treatment))+facet_wrap(~Block)+geom_line()+theme_bw()+scale_color_manual(values=c("green","purple"))+ylab("Weekly Minimum Temperature") #Add the box plot here!

filteredBplot

testplot<-ggplot(data=duration_test, mapping=aes(x=Date, y=WeeklyMin, col=Treatment))+facet_wrap(~Block)+geom_line()+theme_bw()+scale_color_manual(values=c("green","purple"))+ylab("Weekly Minimum Temperature")

testplot


#C Plot-Plot SCP to 2 Week Mean Temperature Period
#New figure-for plot C-plotting the x axis on a common scale, ignoring month-and then plotting all the points on two lines (Control and Shelter). Do the same thing for the maximum temperature (with a different scale)
#Main argument- treatments are affecting the temperature, and the temperature is affecting the SCP. Graphs 1-3 are arguing this.
Cplot<-ggplot(data=join, mapping=aes(x=TwoWeekMeanTemp, y=SCP_Value, col=Treatment))+geom_point(col="grey")+facet_grid(~Month.x, scales="free")+stat_smooth(method="lm", formula=y~x, geom="smooth")+scale_color_manual(values=c("green","purple"))+theme_bw()+ylab("Mean Supercooling Point (\u00B0C)")+xlab("Two Week Mean Temperature (\u00B0C)")+theme(axis.text.x=element_text(angle=45, vjust=0.5))+theme(plot.subtitle  =element_text(face="bold"))+theme(text=element_text("Times New Roman"))


Cplot

MinCplot<-ggplot(data=join, mapping=aes(x=TwoWeekMinTemp, y=SCP_Value, col=Treatment, group=Treatment))+geom_point(col="grey")+stat_smooth(method="lm", formula=y~x, geom="smooth",size=1.5, aes(linetype=Treatment))+scale_color_manual(values=c("green","purple"))+theme_bw()+ylab("Mean Supercooling Point (\u00B0C)")+xlab("Two Week Mean Minimum Temperature (\u00B0C)")+theme(plot.subtitle =element_text(face="bold"))+theme(text=element_text("Times New Roman", "bold", size=10))+scale_linetype_manual(values=c("solid","longdash"))+theme(axis.text.x=element_text(face="bold", size=10))

MaxCplot<-ggplot(data=join, mapping=aes(x=TwoWeekMaxTemp, y=SCP_Value, col=Treatment, group=Treatment))+geom_point(col="grey")+stat_smooth(method="lm", formula=y~x, geom="smooth", size=1.5, aes(linetype=Treatment))+scale_color_manual(values=c("green","purple"))+theme_bw()+ylab("Mean Supercooling Point (\u00B0C)")+xlab("Two Week Mean Maximum Temperature (\u00B0C)")+theme(text=element_text("Times New Roman", "bold", size=10), legend.position="none")+scale_linetype_manual(values=c("longdash", "longdash"))+theme(axis.text.x=element_text(face="bold", size=10))

sum(edit_stats$Treatment=="Sheltered")


MaxCplot/MinCplot
MaxCplot
MinCplot
#Quick Regression of C Plot
#Regression Model
#Probably going to need to use a 2-Way ANOVA, with treatment groups, with covariates (Variance or Range). Add months as a random factor as well. 

anova<-aov(data=combined, formula = MeanSCP~Treatment*MonthlyRange.y+Error(Month)) 
anova2<-aov(data=combined, formula=MeanSCP~Treatment*TwoWeekMinTemp+Error(Month))
anova2<-aov(data=combined, formula=MeanSCP~Treatment*Var.y+Error(Month))
summary(anova2)
summary(anova)#Both range and variance are significant predictors of SCP. 



#Regression Analysis of the Data-Looks like including Block is useful, as well as including TwoWeekMinTemp along with MonthlyRange-but does it make sense to include both range and min temp? Aren't those correlated with each other? 
library(boot)
library(car)
filt<-join%>%
  filter(Treatment=="Control")
filt2<-join%>%
  filter(Treatment=="Sheltered")
linreg<-lm(data=filt, formula = SCP_Value~MonthlyRange)

#linreg<-lm(data=filt, formula = SCP_Value~Block*MonthlyRange+Block* TwoWeekMinTemp+MonthlyRange*TwoWeekMinTemp)
summary(linreg)

#Checking for multicollinearity -Seems like including the interaction effects includes too much multicollinearity
vif(linreg)
#Simplified model for less multicollinearity-all values near 1, indicating that predictors are not highly correlated with each other. 
simp_reg<-lm(SCP_Value ~ Treatment+ Block + MonthlyRange +TwoWeekMinTemp, data = join)
simp_reg_2<-lm(SCP_Value~Block+MonthlyRange+TwoWeekMinTemp, data=filt)

AIC(simp_reg)
AIC(simp_reg_2)

plot(simp_reg, which = 1)
vif(simp_reg)
summary(simp_reg_2)



#Let's try some k-fold cross validation
cv_results<-cv.glm(filt, simp_reg, K=10)

cv_results$delta

#Now let's do the regression for the sheltered treatment groups
linreg_shelt<-lm(data=filt2, formula = SCP_Value~MonthlyRange*Block*TwoWeekMinTemp)
summary(linreg_shelt)


linreg_treat<-lm(data=join, formula=SCP_Value~Treatment*MonthlyRange*TwoWeekMinTemp)
summary(linreg_treat)


#Plotting the control groups
ggplot(data=filt, mapping=aes(x=MonthlyRange, y=SCP_Value, color=Block))+geom_point()+stat_smooth(method="lm", geom="smooth", se=FALSE)+scale_color_manual(values=c("green", "orange", "purple"))+theme_bw()+ylab("Mean Supercooling Point (\u00B0C)")+xlab("Monthly Range (\u00B0C)")+theme(text=element_text("Times New Roman", "bold", size=10))+scale_linetype_manual(values=c("longdash", "longdash"))+theme(axis.text.x=element_text(face="bold", size=10))


#plotting the shelteredg roups
ggplot(data=filt2, mapping=aes(x=MonthlyRange, y=SCP_Value))+geom_point()+stat_smooth(method="lm", geom="smooth", se=FALSE)+scale_color_manual(values=c("green", "orange", "purple"))+theme_bw()+ylab("Mean Supercooling Point (\u00B0C)")+xlab("Monthly Range (\u00B0C)")+theme(text=element_text("Times New Roman", "bold", size=10))+scale_linetype_manual(values=c("longdash", "longdash"))+theme(axis.text.x=element_text(face="bold", size=10))

#Plotting everything together, but with a facet_wrap
ggplot(data=join, mapping=aes(x=MonthlyRange, y=SCP_Value, color=Block))+geom_point()+stat_smooth(method="lm", geom="smooth", se=FALSE)+scale_color_manual(values=c("blue", "#ff7f0e", "darkgreen"))+theme_bw()+facet_wrap(~Treatment)+ylab("Mean Supercooling Point (\u00B0C)")+xlab("Monthly Range (\u00B0C)")+theme(text=element_text("Times New Roman", "bold", size=10))+scale_linetype_manual(values=c("longdash", "longdash"))+theme(axis.text.x=element_text(face="bold", size=12))+theme(strip.text.x=element_text(size=12))

 #3D Plot
library(plotly)
plot_ly(filt, x = ~MonthlyRange, y = ~TwoWeekMinTemp, z = ~SCP_Value, color = ~Block, type = 'scatter3d', mode = 'markers') %>%
  layout(scene = list(xaxis = list(title = 'MonthlyRange'),
                      yaxis = list(title = 'TwoWeekMinTemp'),
                      zaxis = list(title = 'SCP_Value')))

#Assessing correlation between monthly range and two week min temp: 
cor(filt$MonthlyRange, filt$TwoWeekMinTemp, use="complete.obs")

#Correlation coefficient is only 0.13, so a weak positive correlation. 
linreg2<-lm(data=filt2, formula = SCP_Value~TwoWeekMinTemp+Block,family = gaussian)
summary(linreg2)

linreg3<-glm(data=filt, formula = SCP_Value~TwoWeekMaxTemp,family = gaussian)
summary(linreg3)

linreg4<-glm(data=filt2, formula = SCP_Value~TwoWeekMaxTemp,family = gaussian)
summary(linreg4)



ggsave("CPlot_MonthMean.png")

Cplottest<-ggplot(data=join, mapping=aes(x=TwoWeekMaxTemp, y=SCP_Value, col=Treatment))+geom_point(aes(color=Block))+facet_grid(~Month.x+Treatment, scales="free")+stat_smooth(method="lm", formula=y~x, geom="smooth")+theme_bw()+ylab("Mean Supercooling Point (\u00B0C)")+xlab("Mean Minimum Temperature 2 Week (\u00B0C)")+theme(axis.text.x=element_text(angle=45, vjust=0.5))+theme(plot.subtitle =element_text(face="bold"))+theme(text=element_text("Times New Roman"))
Cplottest

ggplot(data=join , mapping=aes(x = "", y=SCP_Value))+geom_boxplot()


testplot<-ggplot(data=combined, mapping=aes(x=MonthlyRange, y=SCP_Value, col=Treatment))+geom_point()+stat_smooth(method="lm", formula=y~x, geom="smooth")+theme_bw()
              
testplot

testjoin<-join%>%
  filter(Block==2|Block==3)

test2<-ggplot(data=testjoin, mapping=aes(x=TwoWeekMinTemp, y=SCP_Value, col=Treatment))+geom_point(col="grey")+facet_grid(~Month.x, scales="free")+stat_smooth(method="lm", formula=y~x, geom="smooth")+scale_color_manual(values=c("green", "purple"))+theme_bw()+ylab("Mean Supercooling Point (\u00B0C)")+xlab("Two Week Mean Minimum Temperature (\u00B0C)")+theme(axis.text.x=element_text(angle=45, vjust=0.5))+theme(plot.subtitle =element_text(face="bold"))+theme(text=element_text("Times New Roman"))+ylim(-27, -22)
test2

summary(glm(data=testjoin, formula=MeanSCP~TwoWeekMinTemp))

#Next Step-Output plots for Two Weeks and Mean Temp regression plots, place into Supplementary materials, and then look at the regression analyses

templot<-ggplot(data=fullData, mapping=aes(x=Mean, y=SCP_Value, col=Treatment))+geom_point()+facet_grid(~MonthNum, scales="free")+stat_smooth(method="lm", formula=y~x, geom="smooth")
templot


#Histogram of Data
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



#Plot of SCP values for Winter Treatment vs. Control
scpplot<-ggplot(data=edit_stats, mapping=aes(x=Month, y=SCP_Value, fill=Treatment))+geom_boxplot(position="identity",alpha=0.5)+facet_wrap(~Block,ncol=1)+ylim(-30,-18)+ggtitle("SCP Values of Tets Winter 2021-22")+theme(axis.text.x=element_text(angle=25))+ylab(expression(Temperature *degree* (C)))
scpplot

MeanSCPplot<-ggplot(data=meanSCP, aes(x=Month, y=MeanSCP, col=Treatment))+geom_point()+ylab("Mean SCP")+ggtitle("Mean Supercooling Point Values over Time")+facet_wrap(~Block, ncol=1)
MeanSCPplot #Maybe use this as a main plot to analyze the differences between control and treatment? Except that there may be a difference between blocks...

pointplot<-ggplot(data=meanSCP, mapping=aes(x=Month, y=MeanSCP, col=Treatment))+geom_point(size=2)+facet_wrap(~Block, ncol=1)+ggtitle("Mean SCP Values of Tets Winter 2021-22")+theme(axis.text.x=element_text(angle=25))+ylab(expression("Temperature" ( degree*C)))
pointplot



jarplot<-ggplot(data=x, mapping=aes(x=Month, y=SCP_Value, col=Treatment))+geom_boxplot()+facet_wrap(~Jar.x)
jarplot

#Log Boxplot 
plot<-ggplot(data=log, mapping=aes(x=Month, y=log, fill=Jar))+geom_boxplot()+facet_wrap(~Jar,ncol=2)
plot

#Plot ibutton logger for each month 
block1<-ggplot(data=datalogger, mapping=aes(x=DateTime, y=Temp))+geom_line()+facet_wrap(~Treatment)+geom_hline(yintercept = 0)+ylab("Temperature C")+xlab("Time")+ggtitle("iButton Datalogger Temperature Records per Jar")
block1 


block1<-ggplot(data=datalogger, mapping=aes(x=DateTime, y=Temp))+geom_line()
block1


#Plot multiple time series (Urban vs. dense per group and facet)
#pdf("Ibutton_Logger")
plot<-ggplot(data=datalogger, mapping = aes(x=DateTime, y=Temp, group=1))+geom_line()+facet_wrap(vars(Block,Treatment))+geom_hline(yintercept = 0)+ylab("Temperature C")+xlab("Time")+ggtitle("iButton Datalogger Temperature Records per Jar")
plot


#Eclosion Histogram
hist<-ggplot(data=emerge, mapping=aes(x=emerge$Tetrastichus, color=Treatment))+geom_histogram(bins=30)
hist

#Eclosion Box Plots-Treatment vs. Control

plot<-ggplot(data=edit, mapping=aes(x=Treatment, y=Tets))+geom_boxplot()
plot

plot2<-ggplot(data=emerge, mapping=aes(x=Treatment, y=Tetrastichus))+geom_boxplot()+facet_wrap(~Group)
plot2

plot3<-ggplot(data=emerge, mapping=aes(x=Treatment, y=Tetrastichus))+geom_boxplot()
plot3


#Environmental Temperature Plots
maxtemplot<-ggplot(data=maxtemps, mapping=aes(x=DateTime, y=MaxC))+facet_wrap(~Group)+geom_line()+geom_hline(yintercept = 0, linetype="dashed", color="red", linewidth=1)
maxtemplot


mintemplot<-ggplot(data=mintemps, mapping=aes(x=DateTime, y=MinC))+facet_wrap(~Group)+geom_line()+geom_hline(yintercept = 0, linetype="dashed", color="blue", linewidth=1)
mintemplot



weektemplot<-ggplot(data=meanweeklytemps, mapping=aes(x=DateTime, y=MinC))+facet_wrap(~Group)+geom_line()+geom_hline(yintercept = 0, linetype="dashed", color="blue", linewidth=1)
weektemplot


#Data Analysis

#Generalized Linear Models (GLM) for SCP values related to monthly mean temperature. Run this for every month
#Also do this for Two Week period!
#Combine all of them into a table of some kind?
#Maybe we first make this into a function-that way we can just loop over it and gather the stats for each time.
# 
# filt <- function(a, b, y, x) {
#   .<-join%>%
#     filter(Month.x==a, Treatment==b)
#   joined<-glm(data=., formula = paste(y, "~",x, collapse= " "))
#   (coef(summary(joined)))
# }
# months<-unique(join$Month.x)
# treatments<-unique(join$Treatment)

# regression_stats<-list()
# for(i in seq_along(months)){
#   for(j in seq_along(treatments)){
#    reg_stats[i, j]<- filt(months[i], treatments[j], "SCP_Value", "TwoWeekMeanTemp")
#   
#   }
# }

x<-join%>%
  filter(Month.x=="January", Treatment=="Control")
y<-glm(x, formula=SCP_Value~TwoWeekMinTemp, family=gaussian)
summary(y)

regstats<-function(data, a, b, c){
  .<-data%>%
    filter(Month.x==a, Treatment==b)
  joined<-glm(data=., formula=paste("SCP_Value", "~",c, collapse= " "))
  x<-(coef(summary(joined)))
  return(x)
}

months<-unique(join$Month.x)
treatments<-unique(join$Treatment)
reg<-expand.grid(Months=months, Treatments=treatments)
reg<-reg%>%
  arrange(factor(Months, levels=c("November", "December", "January", "February", "March", "April")))
reg<-reg[rep(seq_len(nrow(reg)),each=2),]
s<-rep(c("Intercept","TwoWeekMeanTemp"), times=12)
reg$Coefficient<-s

regression_stats<-list()
for(i in seq_along(months)){
  for(j in seq_along(treatments)){
    .<-(regstats(join, months[i], treatments[j], "TwoWeekMinTemp")) #Change Independent variable here
    regression_stats[[paste0(months[i], "_vs_", treatments[j])]]<-.
  }
}
regression_stats

Minframe<-do.call("rbind", regression_stats) #This one works
TwoWeekMinTempStats<-bind_cols(reg,Minframe)

#Mean Temp
regression_stats<-list()
for(i in seq_along(months)){
  for(j in seq_along(treatments)){
    .<-(regstats(join, months[i], treatments[j], "TwoWeekMeanTemp")) #Change Independent variable here
    regression_stats[[paste0(months[i], "_vs_", treatments[j])]]<-.
  }
}
Meanframe<-do.call("rbind", regression_stats)
TwoWeekMeanTempStats<-bind_cols(reg,Meanframe)

#Max Temp
regression_stats<-list()
for(i in seq_along(months)){
  for(j in seq_along(treatments)){
    .<-(regstats(join, months[i], treatments[j], "TwoWeekMaxTemp")) #Change Independent variable here
    regression_stats[[paste0(months[i], "_vs_", treatments[j])]]<-.
  }
}
Maxframe<-do.call("rbind", regression_stats)
TwoWeekMaxTempStats<-bind_cols(reg,Maxframe)
TwoWeekMinTempStats$`Pr(>|t|)`<-format(TwoWeekMinTempStats$`Pr(>|t|)`, scientific=FALSE)
class(TwoWeekMinTempStats)

#Month Mean Temp
regression_stats<-list()
for(i in seq_along(months)){
  for(j in seq_along(treatments)){
    .<-(regstats(join, months[i], treatments[j], "MonthlyMean")) #Change Independent variable here
    regression_stats[[paste0(months[i], "_vs_", treatments[j])]]<-.
  }
}
MonthMeanFrame<-do.call("rbind", regression_stats)
MonthMeanTempStats<-bind_cols(reg,MonthMeanFrame)





library(gt)
options(scipen=999)

TwoWeekMinTempStats<-as_tibble(TwoWeekMinTempStats)
gt_TwoWeekMinTempStats<-gt(TwoWeekMinTempStats)


gt_TwoWeekMinTempStats<-gt_TwoWeekMinTempStats%>%
  tab_header(title="SCP vs. Mean Minimum Temperature Two Weeks Prior to Collection")%>%
  #data_color(column=Treatments, target_columns = everything(), palette = c("green", "purple"))%>%
  opt_table_font(font = google_font(name="Times New Roman"))%>%
  opt_align_table_header("left")%>%
  tab_options(table.margin.left = 0, table.margin.right=-10)

gt_TwoWeekMinTempStats

gtsave(gt_TwoWeekMinTempStats, "MeanMinTempStats2.png")
  

TwoWeekMaxTempStats<-as.tibble(TwoWeekMaxTempStats)
gt_TwoWeekMaxTempStats<-gt(TwoWeekMaxTempStats)

gt_TwoWeekMaxTempStats<-gt_TwoWeekMaxTempStats%>%
  tab_header(title="SCP vs. Mean Maximum Temperature Two Weeks Prior to Collection")%>%
  #data_color(column=Treatments, target_columns = everything(), palette = c("green", "purple"))%>%
  opt_table_font(font = google_font(name="Times New Roman"))%>%
  opt_align_table_header("left")%>%
  tab_options(table.margin.left = 0, table.margin.right=-10)

gt_TwoWeekMaxTempStats

gtsave(gt_TwoWeekMaxTempStats, "MeanMaxTempStats2.png")



TwoWeekMeanTempStats<-as.tibble(TwoWeekMeanTempStats)
gt_TwoWeekMeanTempStats<-gt(TwoWeekMeanTempStats)

gt_TwoWeekMeanTempStats<-gt_TwoWeekMeanTempStats%>%
  tab_header(title="SCP vs. Mean Temperature Two Weeks Prior to Collection")%>%
 # data_color(column=Treatments, target_columns = everything(), palette = c("green", "purple"))%>%
  opt_table_font(font = google_font(name="Times New Roman"))%>%
  opt_align_table_header("left")%>%
  tab_options(table.margin.left = 0, table.margin.right=-10)

gt_TwoWeekMeanTempStats

gtsave(gt_TwoWeekMeanTempStats, "MeanTempStats2.png")


MonthMeanTempStats<-as.tibble(MonthMeanTempStats)

gt_MonthMeanTempStats<-gt(MonthMeanTempStats)

gt_MonthMeanTempStats<-gt_MonthMeanTempStats%>%
  tab_header(title="SCP vs. Mean Monthly  Temperature")%>%
  # data_color(column=Treatments, target_columns = everything(), palette = c("green", "purple"))%>%
  opt_table_font(font = google_font(name="Times New Roman"))%>%
  opt_align_table_header("left")%>%
  tab_options(table.margin.left = 0, table.margin.right=-10)

gt_MonthMeanTempStats
#Collect reg for: TwoWeekMeanTemp, TwoWeekMinTemp, and TwoWeekMaxTemp, as well as MonthMean, MonthMin, and Month Max. Compile all of them into summary tables in kable (?) and double check them. Probably put these into supplementary materials.

#After that point,you should be ready to input all of them into your figures-from there, it's time to look over your references, and then draft comment/integration again. I'd be happy if you worked on the figures/finished them and the references for today, and then slowly work on the draft comments this weekend. 
#Also change everything to times new roman font!

# 
# #frame<-data.frame(t(sapply(regression_stats, function(x) x[1:max(length(regression_stats))])))
# paste0(months[i], "_vs_", treatments[j])
# reg_stats<-bind_rows(regression_stats)
# 
# reg<-expand.grid(Months=months, Treatments=treatments)
# 
# reg<-reg%>%
#   group_by(Months, Treatment)%>%
#   mutate(x=regstats(join,Months, Treatments))



NovControl<-fullData%>%
  filter(Month=="November", Treatment=="Control")
Ncontrol<-glm(data=NovControl, formula=SCP_Value~MonthlyMean)
summary(Ncontrol)

NCjoin<-join%>%
  filter(Month.x=="January", Treatment=="Control")
NCjoin<-glm(data=NCjoin, formula=SCP_Value~TwoWeekMeanTemp)
summary(NCjoin)


NovTreat<-fullData%>%
  filter(Month=="November",Treatment=="Sheltered")
NTreat<-glm(data=NovTreat, formula=SCP_Value~MonthlyMean)
summary(NTreat)

DecControl<-fullData%>%
  filter(Month=="December", Treatment=="Control")
DControl<-glm(data=DecControl, formula=SCP_Value~MonthlyMean)
summary(DControl)

DecTreat<-fullData%>%
  filter(Month=="December", Treatment=="Sheltered")
DTreat<-glm(data=DecTreat, formula=SCP_Value~MonthlyMean)
summary(DTreat)


JanControl<-fullData%>%
  filter(Month=="January", Treatment=="Control")
JControl<-glm(data=JanControl, formula=SCP_Value~MonthlyMean)
summary(JControl)

JanTreat<-fullData%>%
  filter(Month=="January", Treatment=="Sheltered")
JTreat<-glm(data=JanTreat, formula=SCP_Value~MonthlyMean)
summary(JTreat)


FebControl<-fullData%>%
  filter(Month=="February", Treatment=="Control")
FControl<-lm(data=FebControl, formula=SCP_Value~MonthlyMean)
out<-summary(FControl) #Significant
coef(FControl)
out$coefficients

FebTreat<-fullData%>%
  filter(Month=="February", Treatment=="Sheltered")
FTreat<-glm(data=FebTreat, formula=SCP_Value~MonthlyMean)
out<-summary(FTreat) #Significant
coef(FTreat)
out$coefficients

MarControl<-fullData%>%
  filter(Month=="March", Treatment=="Control")
MControl<-glm(data=MarControl, formula=SCP_Value~MonthlyMean)
summary(MControl)

MarTreat<-fullData%>%
  filter(Month=="March", Treatment=="Sheltered")
MTreat<-glm(data=MarTreat, formula=SCP_Value~MonthlyMean)
summary(MTreat)


AprControl<-fullData%>%
  filter(Month=="April", Treatment=="Control")
AControl<-glm(data=AprControl, formula=SCP_Value~MonthlyMean)
summary(AControl) #Significant

AprTreat<-fullData%>%
  filter(Month=="April", Treatment=="Sheltered")
ATreat<-glm(data=AprTreat, formula=SCP_Value~MonthlyMean)
summary(ATreat)


#Test for normality using QQ Plot
qqnorm(stats$SCP_Value, pch=1, frame=FALSE)
qqline(stats$SCP_Value, col="steelblue", lwd=2)

#Test for normality using Shapiro-Wilks Test
shapiro.test(stats$SCP_Value)


#Linear Regression
linmod<-lm(data=fullData, formula=MeanSCP~Treatment+Month+Block)
summary(linmod)

plot<-ggplot(data=fullData,mapping = aes(x=Month, y = SCP_Value, col=Treatment))+geom_point()+geom_jitter()+facet_wrap(facets = "Block")+geom_smooth(method=lm, se=FALSE)

plot



library(lme4)
library(nlme)
library(lmerTest)

#Nested ANOVA Analysis #https://conjugateprior.org/2013/01/formulae-in-r-anova/
#Originally, anova<-aov(data=fullData, formula=MeanSCP~Treatment*Month+Block)
#Ok, so we treat Block as a random effect that's why we just add them 
anova<-aov(data=fullData, formula = MeanSCP~Treatment*Month+Block)#Final Nested ANOVA design, same as: MeanSCP~Block+Treatment+Month+Treatment:Month. So does block interact with month, or only treatment?

x<-aov(data=fullData, formula=MeanSCP ~ Treatment*Month+Block+Error(Block))
summary(x)
summary(anova)
anova<-lmer(data=fullData, formula = MeanSCP~Treatment*Month+(1|Block))
summary(anova)


mixed_model <- lmer(MeanSCP ~ Month + (1|Block/Treatment), data = fullData)

mixed_model <- lmer(MeanSCP ~ Treatment*Month + (1|Block/Treatment), data = fullData)

summary(mixed_model)
mixed_model


summary(anova)
# nested<-aov(data=fullData, formula=MeanSCP~Treatment+Block+Treatment/Month+Treatment:Block+Month:Block)
# summary(nested)
#nested<-aov(data=fullData, formula=MeanSCP~Treatment+Error(Block/Treatment/(Month*Block))


eta_squared(anova, partial=FALSE)


library(TukeyC)
#Post Hoc Tukey HSD Test
tuk<-TukeyC(MeanSCP~Treatment*Month+Block,data=fullData, which="Treatment")
summary(tuk)

tukey<-TukeyHSD(anova)
tukey
tukey$Treatment #Take a look at the specific interactions of the Tukey HSD test and look up interpretations!
tukey$Block #MeanSCP values are different across each Block, too....does that mean that each Block had differences in terms of the weather it experienced? That is true...but to this degree? Is that why? What temperatures did they experience? How different were they? Maybe get the max temp/mean temp of each Block for every month and see the differences? 


#Plot confidence intervals of the Tukey's HSD test. Don't need to do it for everything, but for treatment and Block it might be good.
library(multcompView)
tukey_multiple_comparisons<-TukeyHSD(anova,conf.level=0.95,ordered=TRUE)
tukey_multiple_comparisons
tukey_multiple_comparisons$Treatment[4]
tukey_multiple_comparisons$Month

Month<-TukeyHSD(anova,"Month",conf.level=0.95, ordered=TRUE)
Month
plot(Month,las=1)

Treatment<-TukeyHSD(anova,"Treatment", conf.level = 0.95, ordered=TRUE)
Treatment
plot(Treatment, las=1)

Block<-TukeyHSD(anova,"Block",conf.level=0.95, ordered=TRUE)
Block
plot(Block, las=1)


par(mar=c(11, 11, 0,0), cex=0.)
TreatMonth<-TukeyHSD(anova, "Treatment:Month", conf.level = 0.95, ordered=TRUE)
TreatMonth
plot(TreatMonth, las=1, cex=0.02)

#Calculating Welch Sample t-test to detect differences in maximum and minimum temperatures. 




#Eclosion ANOVA
ANOVA<-aov(data=edit, Tets~Treatment)
fail<-aov(data=edit, Failed~Treatment)
summary(ANOVA)
summary(fail)

#Create treatment col, block (as character string) from Jar col, Time/Date (convert), average SCP. 


#Try linear model where you knockdown time as a function of treatment by time. 
#knockdown~Treatment*time+site


#x<-lm(data=stats,formula = SCP_Value~)

#setwd("~/OneDrive/Documents/R/EAB_Cold_Tolerance/Tet/SCP_Files/Processed_Files/")
#ibutton<-read.csv("iButton_All.csv")

#moments<-data_all%>%
#  group_by(Group)%>%
#  summarise(mean=mean(Tempc), max=max(Tempc), min=min(Tempc),var#=var(Tempc), stdev=sd(Tempc), skew=(sum((Tempc - mean(Tempc))^3)/length(Tempc))/
#              ((sum((Tempc - mean(Tempc))^2)/length(Tempc)))^(3/2),kurtosis=kurtosis(Tempc))



#Environmental Temperature Plots for Various time points and summary statistics
meanbiweeklytemps<-ibutton%>%
  group_by(Group)%>%
  mutate(Group=as.factor(Group),Temp=as.numeric(Temp), Block=as.factor(Block), DateTime=mdy_hm(DateTime))%>%
  ungroup()%>%
  mutate(Treatment=as.factor(case_when(str_detect(ibutton$Group, "U") ~ "Control", TRUE ~ "Treatment")))%>%
  select(-X)%>%
  group_by(Group)%>%
  mutate(Week=ceiling(week(DateTime)/2))%>%
  group_by(Group, Week)%>%
  summarise(MinC=min(Temp),Mean=mean(Temp), MaxC=max(Temp),DateTime=DateTime)

biweektemplot<-ggplot(data=meanbiweeklytemps, mapping=aes(x=DateTime, y=MinC))+facet_wrap(~Group)+geom_line()+geom_hline(yintercept = 0, linetype="dashed", color="blue", linewidth=1)
biweektemplot


meanmonthlytemps<-ibutton%>%
  group_by(Group)%>%
  mutate(Group=as.factor(Group),Temp=as.numeric(Temp), Block=as.factor(Block), DateTime=mdy_hm(DateTime))%>%
  ungroup()%>%
  mutate(Treatment=as.factor(case_when(str_detect(ibutton$Group, "U") ~ "Control", TRUE ~ "Treatment")))%>%
  select(-X)%>%
  group_by(Group)%>%
  mutate(Month=month(DateTime))%>%
  group_by(Group, Month)%>%
  summarise(MinC=min(Temp),Mean=mean(Temp), MaxC=max(Temp),DateTime=DateTime)

monthtemplot<-ggplot(data=meanmonthlytemps, mapping=aes(x=DateTime, y=Mean))+facet_wrap(~Group)+geom_line()+geom_hline(yintercept = 0, linetype="dashed", color="blue", linewidth=1)
monthtemplot

#Calculate daily, weekly, monthly range/variance in temp

meantemps<-ibutton%>%
  group_by(Group)%>%
  mutate(Group=as.factor(Group),Temp=as.numeric(Temp), Block=as.factor(Block), DateTime=mdy_hm(DateTime))%>%
  ungroup()%>%
  mutate(Treatment=as.factor(case_when(str_detect(ibutton$Group, "U") ~ "Control", TRUE ~ "Treatment")))%>%
  select(-X)%>%
  group_by(Group)%>%
  mutate(Day=yday(DateTime))%>%
  group_by(Group, Day)%>%
  summarise(MinC=min(Temp),Mean=mean(Temp), MaxC=max(Temp),DateTime=DateTime)

summary_temps<-ibutton%>%
  group_by(Group)%>%
  mutate(Group=as.factor(Group),Temp=as.numeric(Temp), Block=as.factor(Block), DateTime=mdy_hm(DateTime))%>%
  ungroup()%>%
  mutate(Treatment=as.factor(case_when(str_detect(ibutton$Group, "U") ~ "Control", TRUE ~ "Treatment")))%>%
  select(-X)%>%
  group_by(Group)%>%
  mutate(Day=yday(DateTime), Week=week(DateTime), Biweek=ceiling(week(DateTime)/2), Month=month(DateTime))%>%
  group_by(Day)%>%
  mutate(DailyMin=min(Temp), DailyMean=mean(Temp), DailyMax=max(Temp), DailyRange=DailyMax-DailyMin, DailyVar=var(Temp))%>%
  ungroup()%>%
  group_by(Group, Week)%>%
  mutate(WeeklyMin=min(Temp), WeeklyMean=mean(Temp), WeeklyMax=max(Temp), WeeklyRange=WeeklyMax-WeeklyMin, WeeklyVar=var(Temp))%>%
  ungroup()%>%
  group_by(Group, Biweek)%>%
  mutate(BiweeklyMin=min(Temp), BiweeklyMean=mean(Temp), BiweeklyMax=max(Temp), BiweeklyRange=BiweeklyMax-BiweeklyMin, BiweeklyVar=var(Temp))%>%
  ungroup()%>%
  group_by(Group, Month)%>%
  mutate(MonthlyMin=min(Temp), MonthlyMean=mean(Temp), MonthlyMax=max(Temp), MonthlyRange=MonthlyMax-MonthlyMin, MonthlyVar=var(Temp))


unique(summary_temps$Group)

#Temperature Profile Significant differences
#We're trying to see whether there are significant differences in the microclimate of the treatment groups. So, for mean, minimum, and maximum temperatures across a monthly/weekly/daily interval, run a t-test for Control vs. Treatments and see whether those values are significantly different. Do that for all months and then compile into a table. 

#Get temperature readouts from ibutton logger
#use summary_temps 
#Make sure there are treatment groups
#Group data by Mean/Min/Max for Month/Week/Day
#Run analysis for each (May want to for loop)
#Store in a dataframe/table

ttest<-t.test(WeeklyMin~Treatment, data=summary_temps)

ttest


#### Environmental Temperature Compiled into daily, weekly, monthly mean temperatures, range, variance

meantemps<-ibutton%>%
  group_by(Group)%>%
  mutate(Group=as.factor(Group),Temp=as.numeric(Temp), Block=as.factor(Block), DateTime=mdy_hm(DateTime))%>%
  ungroup()%>%
  mutate(Treatment=as.factor(case_when(str_detect(ibutton$Group, "U") ~ "Control", TRUE ~ "Treatment")))%>%
  select(-X)%>%
  group_by(Group)%>%
  mutate(Day=yday(DateTime))%>%
  group_by(Group, Day)%>%
  summarise(MinC=min(Temp),Mean=mean(Temp), MaxC=max(Temp),DateTime=DateTime)

summary_temps<-ibutton%>%
  group_by(Group)%>%
  mutate(Group=as.factor(Group),Temp=as.numeric(Temp), Block=as.factor(Block), DateTime=mdy_hm(DateTime))%>%
  ungroup()%>%
  mutate(Treatment=as.factor(case_when(str_detect(ibutton$Group, "U") ~ "Control", TRUE ~ "Treatment")))%>%
  select(-X)%>%
  group_by(Group)%>%
  mutate(Day=yday(DateTime), Week=week(DateTime), Biweek=ceiling(week(DateTime)/2), Month=month(DateTime))%>%
  group_by(Day)%>%
  mutate(DailyMin=min(Temp), DailyMean=mean(Temp), DailyMax=max(Temp), DailyRange=DailyMax-DailyMin, DailyVar=var(Temp))%>%
  ungroup()%>%
  group_by(Group, Week)%>%
  mutate(WeeklyMin=min(Temp), WeeklyMean=mean(Temp), WeeklyMax=max(Temp), WeeklyRange=WeeklyMax-WeeklyMin, WeeklyVar=var(Temp))%>%
  ungroup()%>%
  group_by(Group, Biweek)%>%
  mutate(BiweeklyMin=min(Temp), BiweeklyMean=mean(Temp), BiweeklyMax=max(Temp), BiweeklyRange=BiweeklyMax-BiweeklyMin, BiweeklyVar=var(Temp))%>%
  ungroup()%>%
  group_by(Group, Month)%>%
  mutate(MonthlyMin=min(Temp), MonthlyMean=mean(Temp), MonthlyMax=max(Temp), MonthlyRange=MonthlyMax-MonthlyMin, MonthlyVar=var(Temp))%>%
  select(-Temp)%>%
  distinct(across(-DateTime))

head(summary_temps)

edited_temps<-ibutton%>%
  group_by(Group)%>%
  mutate(Group=as.factor(Group),Temp=as.numeric(Temp), Block=as.factor(Block), DateTime=mdy_hm(DateTime))%>%
  ungroup()%>%
  mutate(Treatment=as.factor(case_when(str_detect(ibutton$Group, "U") ~ "Control", TRUE ~ "Treatment")))%>%
  select(-X)%>%
  group_by(Group)%>%
  mutate(Biweek=ceiling(week(DateTime)/2), Month=month(DateTime))%>%
  ungroup()%>%
  group_by(Group, Biweek)%>%
  mutate(BiweeklyMin=min(Temp), BiweeklyMean=mean(Temp), BiweeklyMax=max(Temp), BiweeklyRange=BiweeklyMax-BiweeklyMin, BiweeklyVar=var(Temp))%>%
  ungroup()%>%
  group_by(Group, Month)%>%
  mutate(MonthlyMin=min(Temp), MonthlyMean=mean(Temp), MonthlyMax=max(Temp), MonthlyRange=MonthlyMax-MonthlyMin, MonthlyVar=var(Temp))%>%
  select(-Temp)%>%
  distinct(across(-DateTime))

#Let's grab the weekly temperatures and clean up the dataset a bit
weeklytemps<-tempreadouts%>%
  select(Group,Date,Block, Treatment,Week, Month, WeeklyMax,WeeklyMin)%>%
  distinct()%>%
  filter(Month=="November"|Month=="December"|Month=="January"|Month=="February"|Month=="March"|Month=="April")%>%
  group_by(Block, Week)%>%
  mutate(maxtemp=max(WeeklyMax))%>%
  group_by(Block, Week)%>%
  mutate(weekdiff=abs(WeeklyMax-maxtemp))


maxbarplot<-ggplot(data=weeklytemps, mapping=aes(x=Date, y=weekdiff, fill=Treatment))+geom_bar(stat="identity")+facet_wrap(~Block)+scale_fill_manual(values=c("green", "purple"))+theme(axis.text.x=element_text(angle=45, vjust=0.5, hjust=1))+ylab("Difference in Weekly Maximum Temp. ")


maxbarplot   #Larger bar plots mean that for that treatment group, there is a larger difference (so it has a lower maximum temperature than the other)

WeeklyMaxplot

WeeklyMaxplot/maxbarplot


#Work in Progress-Unfinished Questions
#Does the sCP have any relevance to freeze tolerance? Were there any points on the datalogger that indicated that the temperatures reached points below their SCP, and did they survive, indicating that they might be partially freeze tolerant?

#Get the SCP for each group at each block/jar, and then each minimum temperature experienced per month. 

#Restructure both datasets and merge them.
#Minimum temperatures per month
mintemps<-ibutton%>%
  group_by(Group)%>%
  mutate(Group=as.factor(Group),Temp=as.numeric(Temp), Block=as.factor(Block), DateTime=mdy_hm(DateTime))%>%
  ungroup()%>%
  mutate(Treatment=as.factor(case_when(str_detect(ibutton$Group, "U") ~ "Control", TRUE ~ "Treatment")))%>%
  select(-X)%>%
  group_by(Group)%>%
  mutate(Month=month(DateTime))%>%
  group_by(Group, Month)%>%
  summarise(MinC=min(Temp))%>%
  mutate(Treatment=as.factor(case_when(str_detect(Group, "U") ~ "Control", TRUE ~ "Treatment")))
  #Will need to select the specific groups that are meaningful, otherwise why would you keep them?

monthlystats<-stats%>%
  mutate(MDate=match(stats$Month, month.name))%>%
  group_by(Jar,MDate)%>%
  summarize(MinSCP=min(SCP_Value))


#Cumulative Degree Days

# Temperature analysis using cumulative degree days
# 
# Accumulation of growing degree days calculation:
# add the high and low temperatures and divide by two, then subtract the threshold temperature for the particular insect (Tets: LT50 at -20 degrees C, optimal rearing temperatures are 25-30 C. 
# T. planipennisi was calculated as 319.79 DD (averaged across temperature
# treatments from 15 to 30 °C) above an estimated lower developmental
# threshold of 13.29 °C Duan et al).
# 
# Take min and max temperatures about the same time each day, mid morning or late afternoon. 
# 
# TotalDegreeDays is the total number of degree days experienced in each jar for the duration of the experiment-alternatively I could just pull in the weather data for the closest site and compare to see how well they match up. So the duration-period would be the same (Nov-Apr), and then just do a quick calculation for how much they say they have versus what they actually experienced (Since we were able to catch that at the local environments). Will probably need these site to pull that data: http://uspest.org/NE/VT/index.html
# http://uspest.org/cgi-bin/ddmodel.us?sta=F9323&mdt=ben&spp=aaa&cal=A&tlow=-20&thi=130&stm=11&std=1&styr=22&enm=4&end=31&cel=0&fcast=1&spyr=0&shd=1&mkt=0&mkg=1&ipc=1&evnts=3

#Degree Days calculated from the start of the experiment-when would be a more reasonable point to start? Some literature has stated it makes more sense to start with a "biofix". We can either choose a particular date or get some advice from Jian about this.

cumulativedegreedays<-ibutton%>%
  group_by(Group)%>%
  mutate(Group=as.factor(Group),Temp=as.numeric(Temp), Block=as.factor(Block), DateTime=mdy_hm(DateTime))%>%
  ungroup()%>%
  mutate(Treatment=as.factor(case_when(str_detect(ibutton$Group, "U") ~ "Control", TRUE ~ "Treatment")))%>%
  select(-X)%>%
  group_by(Group)%>%
  mutate(Day=yday(DateTime))%>%
  group_by(Treatment, Group, Day)%>%
  summarize(DegreeDay=((max(Temp)+min(Temp))/2)-13.29)%>%
  ungroup()%>%
  group_by(Group)%>%
  summarize(TotalDegreeDays=sum(DegreeDay[DegreeDay>0]))




#Do we need a Modified Average Method-to take into account the length of time that the daily temperatures may exceed the baseline temperature? To do this, we just need to modify the formula so that the lower threshold temperature is used instead of the daily minimum, or replace the maximum temperature with the upper threshold temperature

#New plots 6/1/2023-Instead of having the month treatments, just look at control vs treatment with block. 
# 
# SCPfilt<-twoweeks%>%
#   filter(MonthNum=="2")
# 
# 
# ggplot(data=joined, mapping = aes(x=Mean, y=SCP_Value, col=Treatment))+geom_boxplot(mapping=aes(Mean, SCP_Value, gr))+facet_wrap(~Block)
# 
# joined<-joined%>%
#   group_by(Group)%>%
#   mutate(MeanSCP=mean(SCP_Value))
#   filter(Treatment=="Control")
# linmod<-lm(data=joined, formula = SCP_Value~Mean)
# 
# 
# summary(linmod)
# 
# IQR(joined$SCP_Value)*1.5
# joined$SCP_Value
# 
# quantile(joined$SCP_Value)
# 
