#Tetrastichus planipennisi Chill Experiment to test for variation in chill groups

setwd("~/OneDrive/Documents/R/Tet/SCP_Files")

# Read csv file for chilling experiment (out-dated)
x<-read.csv("SCP_Jan_1_U_D_2_U_D.csv", header=TRUE, na.strings = c("","NA"))

#Convert to datetime and set as numeric
y<-x%>%
  drop_na()%>%
  mutate(DateTime=as.POSIXct(as.character(paste(Data,Time)), format="%m/%d/%Y %H:%M:%S"))%>%
  mutate(Ch_1=as.numeric(CH.1), Ch_2=as.numeric(CH.2))

#Plotting (Chill Experiment)
y%>%
  plot_time_series(DateTime, Ch_1, .plotly_slider=TRUE, .smooth=FALSE)


y<-x%>%
  drop_na()%>%
  slice(3:length(row(x)))%>%
  rename("Obs"=V1, "Date"=V2, "Time"=V3, "Ch_1"=V5, "Ch_2"=V8)%>%
  select(Date, Time, Ch_1, Ch_2)%>%
  mutate(DateTime=as.POSIXct(as.character(paste(Date, Time)), format="%m/%d/%Y %H:%M:%S"))%>%
  mutate(DateTime=ymd_hms(DateTime))%>%
  mutate(Ch_1=as.numeric(Ch_1), Ch_2=as.numeric(Ch_2))

y%>%
  plot_time_series(DateTime, Ch_1, .plotly_slider=TRUE, .smooth=FALSE)

y%>%
  plot_time_series(DateTime, Ch_2, .plotly_slider=TRUE, .smooth=FALSE)


